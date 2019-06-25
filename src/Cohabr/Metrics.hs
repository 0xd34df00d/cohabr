{-# LANGUAGE GADTs, RankNTypes, DataKinds, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving, BangPatterns #-}

module Cohabr.Metrics
( Metric(..)
, trackLogging
, timed
, timedAvg
, module SMM
) where

import Control.Monad.IO.Class
import Data.Time.Clock.POSIX
import Data.Proxy
import GHC.TypeLits
import System.Metrics.Counter as TC
import System.Metrics.Distribution as TD
import Type.Reflection

import Cohabr.Db.SqlMonad
import System.Metrics.Monad as SMM

data Metric tracker name where
  PageContentsParseTime       :: Metric Distribution "page.parse.contents_ms"
  PageXMLParseTime            :: Metric Distribution "page.parse.xml_ms"
  PageFetchTime               :: Metric Distribution "page.fetch_ms"
  NumPagesFetched             :: Metric Counter      "page.fetches_count"
  DeniedPagesCount            :: Metric Counter      "page.denied_count"
  FetchedCommentsCount        :: Metric Distribution "page.comments_count"

  NewPostsCount               :: Metric Distribution "rss.newposts_count"

  StoredPostInfoRetrievalTime :: Metric Distribution "db.fetch.stored_post_ms"

  PostInsertTime              :: Metric Distribution "db.insert.post_ms"
  PerCommentInsertTime        :: Metric Distribution "db.insert.percomment_ms"
  TotalInsertTime             :: Metric Distribution "db.insert.total_ms"

  PostUpdateTime              :: Metric Distribution "db.update.post_ms"
  PerCommentUpdateTime        :: Metric Distribution "db.update.percomment_ms"
  TotalUpdateTime             :: Metric Distribution "db.update.total_ms"

  UpdatesCandidatesQueryTime  :: Metric Distribution "db.fetch.updates_candidates_ms"
  LastCommentDateQueryTime    :: Metric Distribution "db.fetch.last_comment_date_ms"
  deriving (Typeable)

deriving instance Eq (Metric tracker name)
deriving instance Ord (Metric tracker name)

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
