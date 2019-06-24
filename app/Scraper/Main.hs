{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, BangPatterns, RecordWildCards, ViewPatterns #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Database.PostgreSQL.Simple as PGS
import Control.Concurrent
import Control.Concurrent.Async
import Control.DeepSeq
import Control.Exception(evaluate)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Data.Monoid
import Data.Time.LocalTime
import Data.Proxy
import Network.HTTP.Client(HttpException(..), HttpExceptionContent(..))
import Network.HTTP.Conduit hiding(Proxy)
import Network.HTTP.Types.Status(statusCode)
import Options.Applicative
import Text.HTML.DOM(parseLBS)
import Text.XML.Cursor(fromDocument, node)
import Time.Repeatedly
import System.Log.FastLogger
import System.Posix.Signals
import System.Remote.Monitoring

import Cohabr.Db
import Cohabr.Db.Inserts
import Cohabr.Db.HelperTypes
import Cohabr.Db.Queries
import Cohabr.Db.SqlMonad
import Cohabr.Db.Updates
import Cohabr.Metrics
import Habr.Normalizer
import Habr.Parser
import Habr.RSS
import Habr.Types
import Habr.Util

withConnection :: (MonadMask m, MonadIO m) => String -> (PGS.Connection -> m c) -> m c
withConnection dbName = bracket
  (liftIO $ do
    conn <- PGS.connect PGS.defaultConnectInfo { PGS.connectDatabase = dbName }
    -- I really regret doing this, but the rest of the DB has been created assuming
    -- Moscow locale, and let's at least be consistent.
    void $ PGS.execute_ conn "SET timezone = 'Europe/Moscow'"
    pure conn)
  (liftIO . PGS.close)

refetchPost :: (SqlMonad m, MonadCatch m, MonadMetrics m) => PostHabrId -> m ()
refetchPost habrPostId = handleJust selector handler $ do
  writeLog LogDebug $ "fetching post " <> show habrPostId
  now <- liftIO $ zonedTimeToLocalTime <$> getZonedTime
  let url = urlForPostId $ getHabrId habrPostId
  let normalize = normalizeText $ normalizeUrls $ URL url
  postPage <- timed PageFetchTime $ simpleHttp url
  let root = fromDocument $ parseLBS postPage
  void $ timed PageXMLParseTime $ force' $ node root
  parseResult <- timed PageContentsParseTime $ force' $ normalize $ runExcept $ runReaderT ((,) <$> parsePost root <*> parseComments root) ParseContext { currentTime = now }
  case parseResult of
    Left errs -> writeLog LogError $ unlines $ "Unable to parse " <> show habrPostId : errs
    Right (post, comments) -> do
      let commentsLength = getSum $ mconcat $ Sum . length <$> comments
      writeLog LogDebug "fetched!"
      trackLogging FetchedCommentsCount $ fromIntegral commentsLength
      maybeStoredInfo <- timed StoredPostInfoRetrievalTime $ getStoredPostInfo habrPostId
      case maybeStoredInfo of
        Nothing -> timed TotalInsertTime $ do
          writeLog LogDebug "inserting new one"
          dbId <- timed PostInsertTime $ insertPost habrPostId post
          unless (null comments) $ timedAvg PerCommentInsertTime commentsLength $ insertCommentTree dbId comments
        Just storedInfo -> timed TotalUpdateTime $ do
          writeLog LogDebug "updating"
          timed PostUpdateTime $ updatePost $ postUpdateActions storedInfo post
          timedAvg PerCommentUpdateTime commentsLength $ updateComments $ commentsUpdatesActions storedInfo comments
  writeLog LogDebug $ "done processing " <> show habrPostId
  where
    selector (HttpExceptionRequest _ (StatusCodeException resp contents))
      | statusCode (responseStatus resp) == 403
      , "<a href=\"https://habr.com/ru/users/" `BS.isInfixOf` contents = Just ()
    selector _ = Nothing

    handler _ = do
      writeLog LogError $ "Post is unavailable: " <> show habrPostId
      track DeniedPagesCount

    force' = liftIO . evaluate . force

pollRSS :: (SqlMonad m, MonadCatch m, MonadMetrics m) => m ()
pollRSS = do
  rss <- simpleHttp "https://habr.com/ru/rss/all/all/?fl=ru%2Cen"
  let maybeIds = recentArticles rss
  case maybeIds of
    Nothing -> undefined
    Just ids -> do
      writeLog LogDebug $ "Got new posts: " <> show ids
      recs <- selectMissingPosts $ HabrId <$> ids
      track NewPostsCount $ fromIntegral $ length recs
      writeLog LogDebug $ "Got missing posts: " <> show recs
      mapM_ refetchPost recs

data ExecutionMode
  = BackfillMode { inputFilePath :: String }
  | PollingMode { pollInterval :: Rational }
  deriving (Eq, Show)

data Options = Options
  { logFilePath :: String
  , sqlFilePath :: Maybe String
  , monitoringHost :: BS.ByteString
  , monitoringPort :: Int
  , dbName :: String
  , executionMode :: ExecutionMode
  } deriving (Eq, Show)

options :: Parser Options
options = Options
  <$> strOption (long "log-file" <> short 'l' <> help "Log file name")
  <*> optional (strOption $ long "sql-log-file" <> help "Log file for the SQL statements")
  <*> strOption (long "monitoring-host" <> help "Monitoring server bind host name" <> value "localhost" <> showDefault)
  <*> option auto (long "monitoring-port" <> help "Monitoring server bind port" <> value 8000 <> showDefault)
  <*> strOption (long "dbname" <> short 'd' <> help "Database name" <> value "habr" <> showDefault)
  <*> executionMode
  where
    executionMode = subparser $ mconcat
      [ command "backfill" $ info (backfill <**> helper) $ progDesc "Execute the scraper in backfill mode"
      , command "polling" $ info (polling <**> helper) $ progDesc "Execute the scraper in polling mode"
      ]
    backfill = BackfillMode <$> strOption (long "input-path" <> short 'i' <> help "\\n-separated post IDs file")
    polling = PollingMode <$> option auto (long "poll-interval" <> help "Polling interval (in seconds)")

mkLoggers :: Options -> IO (LoggerHolder, IO ())
mkLoggers Options { .. } = do
  cache <- newTimeCache simpleTimeFormat
  (sqlLogger, sqlCleanup) <- newTimedFastLogger cache sqlLoggerType
  (restLogger, restCleanup) <- newTimedFastLogger cache normalLoggerType
  let logger level msg | level == LogSqlStmt = liftIO $ sqlLogger logStr
                       | otherwise = liftIO $ restLogger logStr
        where logStr ts = toLogStr ts <> " [" <> levelStr level <> "] " <> toLogStr msg <> "\n\n"
  pure (LoggerHolder logger, sqlCleanup >> restCleanup)
  where
    normalLoggerType | logFilePath == "-" = LogStdout defaultBufSize
                     | otherwise = LogFileNoRotate logFilePath defaultBufSize
    sqlLoggerType | Just path <- sqlFilePath = LogFileNoRotate path $ defaultBufSize * 10
                  | otherwise = LogNone
    levelStr LogSqlStmt = "sql"
    levelStr LogDebug = "debug"
    levelStr LogWarn = "warn"
    levelStr LogError = "error"

main :: IO ()
main = do
  opts@Options { .. } <- execParser $ info (options <**> helper) $ fullDesc <> progDesc "Habr scraper"

  (getLogger -> stmtLogger, loggerCleanup) <- mkLoggers opts

  let runSqlMonad act = withConnection dbName $ \conn -> runReaderT act SqlEnv { .. }

  ekgServer <- forkServer monitoringHost monitoringPort
  withMetricsStore ekgServer $ \metrics ->
    case executionMode of
      PollingMode { .. } -> do
        let rssPoller = runSqlMonad $ runMetricsT pollRSS metrics
        rssPoller
        rssPollHandle <- asyncRepeatedly (1 / pollInterval) rssPoller

        let sigintHandler = Catch $ do
              stmtLogger LogDebug "shutting down..."
              cancel rssPollHandle

        void $ installHandler sigINT sigintHandler Nothing
        void $ installHandler sigTERM sigintHandler Nothing

        void $ waitCatch rssPollHandle
        stmtLogger LogDebug "RSS polling thread finished"
      BackfillMode { .. } -> do
        idsStrs <- lines <$> readFile inputFilePath
        let ids = read <$> idsStrs
        runSqlMonad $ flip runMetricsT metrics $
          forM_ ids $ \habrId -> do
            refetchPost $ HabrId habrId
            liftIO $ threadDelay 100000
  stmtLogger LogDebug "bye-bye!"
  loggerCleanup
