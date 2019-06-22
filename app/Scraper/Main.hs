{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, BangPatterns #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Except
import Control.Monad.Reader
import Data.Time.Clock.POSIX
import Data.Time.LocalTime
import Data.Proxy
import Network.HTTP.Conduit hiding(Proxy)
import Text.HTML.DOM(parseLBS)
import Text.XML.Cursor(fromDocument)
import Time.Repeatedly
import System.Environment
import System.IO
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

timed :: forall a m name. (MonadIO m, KnownSymbol name) => MetricsStore -> Metric Distribution name -> m a -> m a
timed store metric act = do
  (t, res) <- time act
  liftIO $ putStrLn $ "Done " <> symbolVal (Proxy :: Proxy name) <> " in " <> show (t * 1000)
  liftIO $ track store metric $ t * 1000
  pure res

timedAvg :: forall a m name. (MonadIO m, KnownSymbol name) => MetricsStore -> Metric Distribution name -> Int -> m a -> m a
timedAvg store metric len act = do
  (t, res) <- time act
  liftIO $ putStrLn $ "Done " <> symbolVal (Proxy :: Proxy name) <> "/" <> show len <> " in " <> show (t * 1000)
  liftIO $ track store metric $ t * 1000 / if len == 0 then 1 else fromIntegral len
  pure res

refetchPost :: MetricsStore -> PostHabrId -> IO ()
refetchPost metrics habrPostId = do
  putStrLn $ "fetching post " <> show habrPostId
  now <- zonedTimeToLocalTime <$> getZonedTime
  postPage <- timed metrics PageFetchTime $ simpleHttp $ urlForPostId $ getHabrId habrPostId
  let root = fromDocument $ parseLBS postPage
  let parseResult = runExcept $ runReaderT ((,) <$> parsePost root <*> parseComments root) ParseContext { currentTime = now }
  case parseResult of
    Left errs -> do
      hPutStr stderr $ unlines errs
      error $ show errs
    Right (post, comments) -> do
      putStrLn "fetched!"
      maybeStoredInfo <- timed metrics StoredPostInfoRetrievalTime $ runSqlMonad $ getStoredPostInfo habrPostId
      case maybeStoredInfo of
        Nothing -> timed metrics TotalInsertTime $ runSqlMonad $ do
          liftIO $ putStrLn "inserting new one"
          dbId <- timed metrics PostInsertTime $ insertPost habrPostId post
          timedAvg metrics PerCommentInsertTime (length comments) $ insertCommentTree dbId comments
        Just storedInfo -> runSqlMonad $ do
          liftIO $ putStrLn "updating"
          updatePost $ postUpdateActions storedInfo post
          updateComments $ commentsUpdatesActions storedInfo comments
  putStrLn $ "done processing " <> show habrPostId

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
