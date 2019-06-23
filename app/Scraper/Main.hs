{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, BangPatterns, RecordWildCards #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Database.PostgreSQL.Simple as PGS
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Data.List
import Data.Time.Clock.POSIX
import Data.Time.LocalTime
import Data.Proxy
import Network.HTTP.Client(HttpException(..), HttpExceptionContent(..))
import Network.HTTP.Conduit hiding(Proxy)
import Network.HTTP.Types.Status(statusCode)
import Options.Applicative
import Text.HTML.DOM(parseLBS)
import Text.XML.Cursor(fromDocument)
import Time.Repeatedly
import System.Remote.Monitoring

import Cohabr.Db
import Cohabr.Db.Inserts
import Cohabr.Db.HelperTypes
import Cohabr.Db.Queries
import Cohabr.Db.SqlMonad
import Cohabr.Db.Updates
import Cohabr.Metrics
import Habr.Parser
import Habr.RSS
import Habr.Util

withConnection :: (PGS.Connection -> IO c) -> IO c
withConnection = bracket
  (PGS.connect PGS.defaultConnectInfo { PGS.connectDatabase = "habr" })
  PGS.close

runSqlMonad :: (forall m. (SqlMonad m, MonadCatch m) => m a) -> IO a
runSqlMonad act = withConnection $ \c -> runReaderT act SqlEnv { conn = c, stmtLogger = logger }
  where
    logger LogSqlStmt _ = pure ()
    logger _ msg = liftIO $ putStrLn msg

time :: MonadIO m => m a -> m (Double, a)
time act = do
  start <- liftIO $ realToFrac <$> getPOSIXTime
  result <- act
  end <- liftIO $ realToFrac <$> getPOSIXTime
  let !delta = end - start
  pure (delta, result)

trackLogging :: forall m name. (SqlMonad m, MonadMetrics m, KnownSymbol name) => Metric Distribution name -> Double -> m ()
trackLogging metric t = do
  writeLog LogDebug $ "Done " <> symbolVal (Proxy :: Proxy name) <> " in " <> show t
  track metric t

timed :: (SqlMonad m, MonadMetrics m, KnownSymbol name) => Metric Distribution name -> m a -> m a
timed metric act = do
  (t, res) <- time act
  trackLogging metric $ t * 1000
  pure res

timedAvg :: (SqlMonad m, MonadMetrics m, KnownSymbol name) => Metric Distribution name -> Int -> m a -> m a
timedAvg metric len act = do
  (t, res) <- time act
  trackLogging metric $ t * 1000 / if len == 0 then 1 else fromIntegral len
  pure res

refetchPost :: (SqlMonad m, MonadCatch m, MonadMetrics m) => PostHabrId -> m ()
refetchPost habrPostId = handleJust selector handler $ do
  writeLog LogDebug $ "fetching post " <> show habrPostId
  now <- liftIO $ zonedTimeToLocalTime <$> getZonedTime
  postPage <- timed PageFetchTime $ simpleHttp $ urlForPostId $ getHabrId habrPostId
  let root = fromDocument $ parseLBS postPage
  let parseResult = runExcept $ runReaderT ((,) <$> parsePost root <*> parseComments root) ParseContext { currentTime = now }
  case parseResult of
    Left errs -> writeLog LogError $ unlines $ "Unable to parse " <> show habrPostId : errs
    Right (post, comments) -> do
      writeLog LogDebug "fetched!"
      trackLogging FetchedCommentsCount $ genericLength comments
      maybeStoredInfo <- timed StoredPostInfoRetrievalTime $ getStoredPostInfo habrPostId
      case maybeStoredInfo of
        Nothing -> timed TotalInsertTime $ do
          writeLog LogDebug "inserting new one"
          dbId <- timed PostInsertTime $ insertPost habrPostId post
          timedAvg PerCommentInsertTime (length comments) $ insertCommentTree dbId comments
        Just storedInfo -> timed TotalUpdateTime $ do
          writeLog LogDebug "updating"
          timed PostUpdateTime $ updatePost $ postUpdateActions storedInfo post
          timedAvg PerCommentUpdateTime (length comments) $ updateComments $ commentsUpdatesActions storedInfo comments
  writeLog LogDebug $ "done processing " <> show habrPostId
  where
    selector (HttpExceptionRequest _ (StatusCodeException resp contents))
      | statusCode (responseStatus resp) == 403
      , "<a href=\"https://habr.com/ru/users/" `BS.isInfixOf` contents = Just ()
    selector _ = Nothing

    handler _ = do
      writeLog LogError $ "Post is unavailable: " <> show habrPostId
      track DeniedPagesCount

pollRSS :: (SqlMonad m, MonadCatch m, MonadMetrics m) => m ()
pollRSS = do
  rss <- simpleHttp "https://habr.com/ru/rss/all/all/?fl=ru%2Cen"
  let maybeIds = recentArticles rss
  case maybeIds of
    Nothing -> undefined
    Just ids -> do
      recs <- selectMissingPosts $ HabrId <$> ids
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

main :: IO ()
main = do
  Options { .. } <- execParser $ info (options <**> helper) $ fullDesc <> progDesc "Habr scraper"
  ekgServer <- forkServer monitoringHost monitoringPort
  withMetricsStore ekgServer $ \metrics ->
    case executionMode of
      PollingMode { .. } -> do
        rssPollHandle <- asyncRepeatedly (1 / pollInterval) $ runSqlMonad $ runMetricsT pollRSS metrics
        wait rssPollHandle
      BackfillMode { .. } -> do
        idsStrs <- lines <$> readFile inputFilePath
        let ids = read <$> idsStrs
        runSqlMonad $ flip runMetricsT metrics $
          forM_ ids $ \habrId -> do
            refetchPost $ HabrId habrId
            liftIO $ threadDelay 100000
