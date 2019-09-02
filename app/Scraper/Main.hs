{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Default
import Options.Applicative
import Time.Repeatedly
import System.Log.FastLogger
import System.Posix.Signals
import System.Remote.Monitoring

import Cohabr.AppEnv
import Cohabr.Db(PostHabrId)
import Cohabr.Db.HelperTypes
import Cohabr.Db.SqlMonad
import Cohabr.Db.Utils
import Cohabr.Fetch
import Cohabr.Logger
import Cohabr.Metrics

data ExecutionMode
  = BackfillMode { inputFilePath :: String }
  | PollingMode
    { pollInterval :: Rational
    , blacklistPath :: Maybe String
    , updatesConfig :: UpdatesConfig
    }
  deriving (Eq, Show)

data Options = Options
  { logFilePath :: String
  , sqlFilePath :: Maybe String
  , monitoringHost :: BS.ByteString
  , monitoringPort :: Int
  , dbName :: String
  , httpConfig :: HttpConfig
  , executionMode :: ExecutionMode
  } deriving (Eq, Show)

options :: Parser Options
options = Options
  <$> strOption (long "log-file" <> short 'l' <> help "Log file name")
  <*> optional (strOption $ long "sql-log-file" <> help "Log file for the SQL statements")
  <*> strOption (long "monitoring-host" <> help "Monitoring server bind host name" <> value "localhost" <> showDefault)
  <*> option auto (long "monitoring-port" <> help "Monitoring server bind port" <> value 8000 <> showDefault)
  <*> strOption (long "dbname" <> short 'd' <> help "Database name" <> value "habr" <> showDefault)
  <*> httpConfigParser
  <*> executionMode
  where
    executionMode = subparser $ mconcat
      [ command "backfill" $ info (backfill <**> helper) $ progDesc "Execute the scraper in backfill mode"
      , command "polling" $ info (polling <**> helper) $ progDesc "Execute the scraper in polling mode"
      ]
    backfill = BackfillMode <$> strOption (long "input-path" <> short 'i' <> help "\\n-separated post IDs file")
    polling = PollingMode
            <$> option auto (long "poll-interval" <> help "Polling interval (in seconds)" <> value 60 <> showDefault)
            <*> optional (strOption $ long "blacklist" <> help "Blacklist file with \\n-separated post IDs")
            <*> updatesConfigParser
    updatesConfigParser = UpdatesConfig
                        <$> option auto (long "update-fetch-jobs" <> help "Max parallel fetch jobs for the existing posts updates" <> value 4 <> showDefault)
                        <*> option auto (long "max-updates-queue-size" <> help "Max queue size for the update jobs" <> value (pendingUpdatesQueueSize def) <> showDefault)
    httpConfigParser = HttpConfig
                     <$> option auto (long "http-timeout" <> help "Timeout for HTTP connections, s" <> value 120 <> showDefault)

mkLoggers :: Options -> IO (LoggerHolder, SqlLoggerHolder, IO ())
mkLoggers Options { .. } = do
  cache <- newTimeCache simpleTimeFormat
  (sqlLoggerRaw, sqlCleanup) <- newTimedFastLogger cache sqlLoggerType
  (restLoggerRaw, restCleanup) <- newTimedFastLogger cache normalLoggerType
  let sqlLogger msg = liftIO $ sqlLoggerRaw $ \ts -> toLogStr ts <> " " <> toLogStr msg <> "\n\n"
  let restLogger level msg = liftIO $ restLoggerRaw logStr
        where logStr ts = toLogStr ts <> " [" <> levelStr level <> "] " <> toLogStr msg <> "\n\n"
  pure (LoggerHolder restLogger, SqlLoggerHolder sqlLogger, sqlCleanup >> restCleanup)
  where
    normalLoggerType | logFilePath == "-" = LogStdout defaultBufSize
                     | otherwise = LogFileNoRotate logFilePath defaultBufSize
    sqlLoggerType | Just path <- sqlFilePath = LogFileNoRotate path $ defaultBufSize * 10
                  | otherwise = LogNone
    levelStr LogDebug = "debug"
    levelStr LogInfo = "info"
    levelStr LogWarn = "warn"
    levelStr LogError = "error"

parseBlacklist :: Maybe String -> IO [PostHabrId]
parseBlacklist Nothing = pure []
parseBlacklist (Just path) = map (HabrId . read . head . words) . lines <$> readFile path

main :: IO ()
main = do
  opts@Options { .. } <- execParser $ info (options <**> helper) $ fullDesc <> progDesc "Habr scraper"

  (loggerHolder, getSqlLogger -> stmtLogger, loggerCleanup) <- mkLoggers opts
  let logger = getLogger loggerHolder

  ekgServer <- forkServer monitoringHost monitoringPort
  withMetricsStore ekgServer $ \metrics -> do
    let appEnv conn = AppEnv { sqlEnvPart = SqlEnv { .. }, httpConfigPart = httpConfig, loggerHolder = loggerHolder }

    let runFullMonad act = withConnection dbName $ \conn -> runReaderT (runMetricsT act metrics) $ appEnv conn

    case executionMode of
      PollingMode { .. } -> withUpdatesThread updatesConfig runFullMonad $ \ut -> do
        let rssPoller = runFullMonad pollRSS
        rssPoller
        rssPollHandle <- asyncRepeatedly (1 / pollInterval) rssPoller

        blacklist <- parseBlacklist blacklistPath

        let updatesChecker = runFullMonad $ checkUpdates blacklist ut
        updatesChecker
        checkUpdatesHandle <- asyncRepeatedly (1 / 60) updatesChecker

        let sigintHandler = Catch $ do
              logger LogDebug "shutting down..."
              mapM_ cancel [rssPollHandle, checkUpdatesHandle]

        void $ installHandler sigINT sigintHandler Nothing
        void $ installHandler sigTERM sigintHandler Nothing

        mapM_ waitCatch [rssPollHandle, checkUpdatesHandle]
        logger LogDebug "Threads finished"
      BackfillMode { .. } -> do
        idsStrs <- lines <$> readFile inputFilePath
        let ids = read <$> idsStrs
        runFullMonad $ do
          track BackfillQueueSize $ SetCountdown $ length ids
          forM_ ids $ \habrId -> do
            track BackfillQueueSize DecCountdown
            refetchPost $ HabrId habrId
            liftIO $ threadDelay 100000
  logger LogDebug "bye-bye!"
  loggerCleanup
