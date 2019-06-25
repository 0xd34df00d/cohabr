{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, RecordWildCards, ViewPatterns #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Database.PostgreSQL.Simple as PGS
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Options.Applicative
import Time.Repeatedly
import System.Log.FastLogger
import System.Posix.Signals
import System.Remote.Monitoring

import Cohabr.Db.HelperTypes
import Cohabr.Db.SqlMonad
import Cohabr.Fetch
import Cohabr.Metrics

withConnection :: (MonadMask m, MonadIO m) => String -> (PGS.Connection -> m c) -> m c
withConnection dbName = bracket
  (liftIO $ do
    conn <- PGS.connect PGS.defaultConnectInfo { PGS.connectDatabase = dbName }
    -- I really regret doing this, but the rest of the DB has been created assuming
    -- Moscow locale, and let's at least be consistent.
    void $ PGS.execute_ conn "SET timezone = 'Europe/Moscow'"
    pure conn)
  (liftIO . PGS.close)

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
