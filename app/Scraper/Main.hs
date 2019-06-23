{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, BangPatterns #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Data.List
import Data.Time.Clock.POSIX
import Data.Time.LocalTime
import Data.Proxy
import Network.HTTP.Client(HttpException(..), HttpExceptionContent(..))
import Network.HTTP.Conduit hiding(Proxy)
import Network.HTTP.Types.Status(statusCode)
import Text.HTML.DOM(parseLBS)
import Text.XML.Cursor(fromDocument)
import Time.Repeatedly
import System.Environment
import System.Remote.Monitoring

import Database.PostgreSQL.Util
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

runSqlMonad :: (forall m. SqlMonad m => m a) -> IO a
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

refetchPost :: MetricsStore -> PostHabrId -> IO ()
refetchPost metrics habrPostId = handleJust selector handler $ runSqlMonad $ flip runMetricsT metrics $ do
  writeLog LogDebug $ "fetching post " <> show habrPostId
  now <- liftIO $ zonedTimeToLocalTime <$> getZonedTime
  postPage <- timed PageFetchTime $ simpleHttp $ urlForPostId $ getHabrId habrPostId
  let root = fromDocument $ parseLBS postPage
  let parseResult = runExcept $ runReaderT ((,) <$> parsePost root <*> parseComments root) ParseContext { currentTime = now }
  case parseResult of
    Left errs -> writeLog LogError $ unlines errs
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
      putStrLn "Post is unavailable"
      runMetricsT (track DeniedPagesCount) metrics

pollRSS :: IO ()
pollRSS = do
  rss <- simpleHttp "https://habr.com/ru/rss/all/all/?fl=ru%2Cen"
  let maybeIds = recentArticles rss
  case maybeIds of
    Nothing -> undefined
    Just ids -> do
      recs <- runSqlMonad $ selectMissingPosts $ HabrId <$> ids
      mapM_ (refetchPost undefined) [head recs]

main :: IO ()
main = do
  ekgServer <- forkServer "localhost" 8000
{-
  rssPollHandle <- asyncRepeatedly (1 / 60) pollRSS
  wait rssPollHandle
  -}
  [filename] <- getArgs
  idsStrs <- take 100 . lines <$> readFile filename
  let ids = read <$> idsStrs
  withMetricsStore ekgServer $ \metrics ->
    forM_ ids $ \habrId -> do
      refetchPost metrics $ HabrId habrId
      threadDelay 100000
