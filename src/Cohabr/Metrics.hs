{-# LANGUAGE GADTs, RankNTypes, DataKinds, FlexibleContexts, ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving, RecordWildCards, OverloadedStrings #-}

module Cohabr.Metrics
( Metric(..)
, trackLogging
, timed
, timedAvg
, module SME
) where

import Control.Monad.IO.Class
import Data.Time.Clock.POSIX
import Data.Proxy
import GHC.TypeLits
import System.Metrics
import System.Metrics.Counter as TC
import System.Metrics.Distribution as TD
import System.Metrics.Gauge as TG
import System.CPUTime
import Type.Reflection

import Cohabr.Db.SqlMonad
import System.Metrics.Extensible as SME

data Metric tracker name where
  PageContentsParseTime       :: Metric Distribution "page.parse.contents_ms"
  PageXMLParseTime            :: Metric Distribution "page.parse.xml_ms"
  PageFetchTime               :: Metric Distribution "page.fetch_ms"
  NumPagesFetched             :: Metric Counter      "page.fetches_count"
  DeniedPagesCount            :: Metric Counter      "page.denied_count"
  FetchedCommentsCount        :: Metric Distribution "page.fetched_comments_count"
  UpdatedCommentsCount        :: Metric Distribution "page.updated_comments_count"

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

  UpdateCheckQueueSize        :: Metric Gauge        "cohabr.updater.queue_size"
  OutdatedItemsCount          :: Metric Gauge        "cohabr.updater.outdated_count"
  deriving (Typeable)

deriving instance Eq (Metric tracker name)
deriving instance Ord (Metric tracker name)

data Timing = Timing
  { wallTime :: !Double
  , cpuTime :: !Double
  } deriving (Eq, Ord, Show)

data Timer = Timer
  { wallTimer :: Distribution
  , cpuTimer :: Distribution
  }

instance TrackerLike Timer where
  type TrackAction Timer m = Timing -> m ()
  track metric Timing { .. } = do
    Timer { .. } <- getTracker metric
    liftIO $ do
      TD.add wallTimer wallTime
      TD.add cpuTimer cpuTime
  createTracker name store = do
    wallTimer <- createDistribution (name <> ".wall_ms") store
    cpuTimer <- createDistribution (name <> ".cpu_ms") store
    pure $ Timer { .. }

time :: MonadIO m => m a -> m (Timing, a)
time act = do
  startWall <- liftIO $ realToFrac <$> getPOSIXTime
  startCpu <- liftIO getCPUTime
  result <- act
  endCpu <- liftIO getCPUTime
  endWall <- liftIO $ realToFrac <$> getPOSIXTime
  let wallTime = endWall - startWall
  let cpuTime = fromIntegral (endCpu - startCpu) / 1e9
  pure (Timing { .. }, result)

trackLogging :: forall r m name. (SqlMonad r m, MonadMetrics m, KnownSymbol name) => Metric Distribution name -> Double -> m ()
trackLogging metric t = do
  writeLog LogDebug $ "Done " <> symbolVal (Proxy :: Proxy name) <> " in " <> show t
  track metric t

timed :: (SqlMonad r m, MonadMetrics m, KnownSymbol name) => Metric Distribution name -> m a -> m a
timed metric act = do
  (t, res) <- time act
  trackLogging metric $ wallTime t * 1000
  pure res

timedAvg :: (SqlMonad r m, MonadMetrics m, KnownSymbol name) => Metric Distribution name -> Int -> m a -> m a
timedAvg metric len act = do
  (t, res) <- time act
  trackLogging metric $ wallTime t * 1000 / if len == 0 then 1 else fromIntegral len
  pure res
