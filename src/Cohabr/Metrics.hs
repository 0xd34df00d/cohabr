{-# LANGUAGE GADTs, RankNTypes, DataKinds, FlexibleContexts, ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving, RecordWildCards, OverloadedStrings #-}

module Cohabr.Metrics
( Metric(..)

, CountdownAction(..)

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
  PageContentsParseTime       :: Metric Timer        "page.parse.contents"
  PageXMLParseTime            :: Metric Timer        "page.parse.xml"
  PageFetchTime               :: Metric Timer        "page.fetch"
  NumPagesFetched             :: Metric Counter      "page.fetches_count"

  DeniedPagesCount            :: Metric Counter      "page.denied_count"

  TimeoutHttpCount            :: Metric Counter      "http.timeout_count"
  HardTimeoutHttpCount        :: Metric Counter      "http.hard_timeout_count"

  FetchedCommentsCount        :: Metric Distribution "page.fetched_comments_count"
  UpdatedCommentsCount        :: Metric Distribution "page.updated_comments_count"

  NewPostsCount               :: Metric Distribution "rss.newposts_count"

  StoredPostInfoRetrievalTime :: Metric Timer        "db.fetch.stored_post"

  PostInsertTime              :: Metric Timer        "db.insert.post"
  PerCommentInsertTime        :: Metric Timer        "db.insert.percomment"
  TotalInsertTime             :: Metric Timer        "db.insert.total"

  PostUpdateTime              :: Metric Timer        "db.update.post"
  PerCommentUpdateTime        :: Metric Timer        "db.update.percomment"
  TotalUpdateTime             :: Metric Timer        "db.update.total"

  UpdatesCandidatesQueryTime  :: Metric Timer        "db.fetch.updates_candidates"
  LastCommentDateQueryTime    :: Metric Timer        "db.fetch.last_comment_date"

  UpdateCheckQueueSize        :: Metric Gauge        "cohabr.updater.queue_size"
  OutdatedItemsCount          :: Metric Gauge        "cohabr.updater.outdated_count"

  BackfillQueueSize           :: Metric Countdown    "cohabr.backfill.queue_size"
  deriving (Typeable)

deriving instance Eq (Metric tracker name)
deriving instance Ord (Metric tracker name)

newtype Countdown = Countdown Gauge

data CountdownAction = SetCountdown Int | DecCountdown

instance TrackerLike Countdown where
  type TrackAction Countdown m = CountdownAction -> m ()
  track metric act = do
    Countdown g <- getTracker metric
    liftIO $ case act of
                  DecCountdown -> TG.dec g
                  SetCountdown n -> TG.set g $ fromIntegral n
  createTracker name store = Countdown <$> createGauge name store

data Timing = Timing
  { wallTime :: !Double
  , cpuTime :: !Double
  } deriving (Eq, Ord, Show)

adjustAvg :: Timing -> Int -> Timing
adjustAvg t 0 = t
adjustAvg Timing { .. } count = Timing { wallTime = wallTime / count', cpuTime = cpuTime / count' }
  where count' = fromIntegral count

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
  let wallTime = (endWall - startWall) * 1e3
  let cpuTime = fromIntegral (endCpu - startCpu) / 1e9
  pure (Timing { .. }, result)

trackLogging :: forall r m name trackerTy valTy.
                (SqlMonad r m, MonadMetrics m, KnownSymbol name, TrackerLike trackerTy, Show valTy, TrackAction trackerTy m ~ (valTy -> m ()))
                => Metric trackerTy name -> valTy -> m ()
trackLogging metric t = do
  writeLog LogDebug $ "Done " <> symbolVal (Proxy :: Proxy name) <> ": " <> show t
  track metric t

timed :: (SqlMonad r m, MonadMetrics m, KnownSymbol name) => Metric Timer name -> m a -> m a
timed metric act = do
  (t, res) <- time act
  trackLogging metric t
  pure res

timedAvg :: (SqlMonad r m, MonadMetrics m, KnownSymbol name) => Metric Timer name -> Int -> m a -> m a
timedAvg metric len act = do
  (t, res) <- time act
  trackLogging metric $ t `adjustAvg` len
  pure res
