{-# LANGUAGE DataKinds, RankNTypes, GADTs, TypeFamilies, PolyKinds, TypeFamilyDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Cohabr.Metrics
( MetricsStore
, newMetricsStore
, withMetricsStore

, Metric(..)
, track
, KnownSymbol
, symbolVal

, Counter
, Distribution

, MonadMetrics(..)
, MetricsT
, runMetricsT
, Metrics
) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Control.Monad.Catch
import Control.Monad.Identity
import Control.Monad.IO.Class
import Data.Time.Clock.POSIX
import Control.Monad.Reader
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TQueue
import Data.Proxy
import GHC.TypeLits
import Lens.Micro
import Lens.Micro.TH
import System.Remote.Monitoring
import System.Metrics
import System.Metrics.Counter as TC
import System.Metrics.Distribution as TD
import Type.Reflection

data Dyn ctx where
  Dyn :: ctx a => TypeRep a -> a -> Dyn ctx

toDyn :: (Typeable a, ctx a) => a -> Dyn ctx
toDyn val = Dyn (typeOf val) val

withDyns :: Dyn ctx -> Dyn ctx ->
            (forall a. ctx a => a -> a -> b) ->
            (SomeTypeRep -> SomeTypeRep -> b) -> b
withDyns (Dyn ty1 v1) (Dyn ty2 v2) f def = case eqTypeRep ty1 ty2 of
  Nothing -> def (SomeTypeRep ty1) (SomeTypeRep ty2)
  Just HRefl -> f v1 v2

instance Eq (Dyn Ord) where
  d1 == d2 = withDyns d1 d2 (==) (\_ _ -> False)

instance Ord (Dyn Ord) where
  compare d1 d2 = withDyns d1 d2 compare compare

type DynOrd = Dyn Ord

data MetricsState = MetricsState
  { _server :: Server
  , _counters :: M.Map DynOrd Counter
  , _distributions :: M.Map DynOrd Distribution
  }

$(makeLenses 'MetricsState)

data MetricRequest where
  MetricRequest :: (TrackerLike tracker, KnownSymbol name, Typeable metric, Ord (metric tracker name))
                => metric tracker name
                -> MVar tracker
                -> MetricRequest

newtype MetricsStore = MetricsStore { mReqQueue :: TQueue MetricRequest }

newMetricsStore :: Server -> IO (MetricsStore, IO ())
newMetricsStore srv = do
  queue <- newTQueueIO
  threadId <- forkIO $ act queue $ MetricsState srv mempty mempty
  pure (MetricsStore queue, killThread threadId)
  where
    act queue state = do
      req <- atomically (readTQueue queue)
      state' <- (\(MetricRequest metric mvar) -> handleReq state metric mvar) req
      act queue state'

    handleReq :: forall metric tracker name. (TrackerLike tracker, KnownSymbol name, Typeable metric, Ord (metric tracker name))
              => MetricsState
              -> metric tracker name
              -> MVar tracker
              -> IO MetricsState
    handleReq state metric mvar = do
      let metricDyn = toDyn metric
      (tracker, state') <- case M.lookup metricDyn $ state ^.trackerMap of
        Just existing -> pure (existing, state)
        Nothing -> do
          let trackerName = symbolVal (Proxy :: Proxy name)
          newTracker <- createTracker (T.pack trackerName) $ serverMetricStore $ state^.server
          pure (newTracker, state & trackerMap %~ M.insert metricDyn newTracker)
      putMVar mvar tracker
      pure state'

withMetricsStore :: Server -> (MetricsStore -> IO a) -> IO a
withMetricsStore srv f = bracket
  (newMetricsStore srv)
  snd
  (f . fst)

class Typeable tracker => TrackerLike tracker where
  type TrackAction tracker (m :: * -> *) = r | r -> m
  track :: (MonadMetrics m, KnownSymbol name, Typeable metric, Ord (metric tracker name)) => metric tracker name -> TrackAction tracker m
  trackerMap :: Lens' MetricsState (M.Map DynOrd tracker)
  createTracker :: T.Text -> Store -> IO tracker

instance TrackerLike Counter where
  type TrackAction Counter m = m ()
  track metric = getMetric metric >>= liftIO . TC.inc
  trackerMap = counters
  createTracker = createCounter

instance TrackerLike Distribution where
  type TrackAction Distribution m = Double -> m ()
  track metric val = getMetric metric >>= \distr -> liftIO $ TD.add distr val
  trackerMap = distributions
  createTracker = createDistribution

data Metric tracker name where
  PageContentsParseTime       :: Metric Distribution "page.parse.contents_ms"
  PageXMLParseTime            :: Metric Distribution "page.parse.xml_ms"
  PageFetchTime               :: Metric Distribution "page.fetch_ms"
  NumPagesFetched             :: Metric Counter      "page.fetches_count"
  DeniedPagesCount            :: Metric Counter      "page.denied_count"
  FetchedCommentsCount        :: Metric Distribution "page.comments_count"

  NewPostsCount               :: Metric Distribution "rss.newposts_count"

  StoredPostInfoRetrievalTime :: Metric Distribution "db.fetch.storedpost_ms"

  PostInsertTime              :: Metric Distribution "db.insert.post_ms"
  PerCommentInsertTime        :: Metric Distribution "db.insert.percomment_ms"
  TotalInsertTime             :: Metric Distribution "db.insert.total_ms"

  PostUpdateTime              :: Metric Distribution "db.update.post_ms"
  PerCommentUpdateTime        :: Metric Distribution "db.update.percomment_ms"
  TotalUpdateTime             :: Metric Distribution "db.update.total_ms"
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
getMetricStore :: (TrackerLike tracker, KnownSymbol name, Typeable metric, Ord (metric tracker name))
               => MetricsStore
               -> metric tracker name
               -> IO tracker
getMetricStore store metric = do
  mvar <- newEmptyMVar
  atomically $ writeTQueue (mReqQueue store) $ MetricRequest metric mvar
  takeMVar mvar

class MonadIO m => MonadMetrics m where
  getMetric :: (TrackerLike tracker, KnownSymbol name, Typeable metric, Ord (metric tracker name))
            => metric tracker name -> m tracker

newtype MetricsT (m :: k -> *) (a :: k) = MetricsT { runMetricsT :: MetricsStore -> m a }
type Metrics = MetricsT Identity

instance Functor m => Functor (MetricsT m) where
  fmap f (MetricsT m) = MetricsT $ fmap f . m

instance Applicative m => Applicative (MetricsT m) where
  pure = MetricsT . const . pure
  (MetricsT fun) <*> (MetricsT val) = MetricsT $ \store -> fun store <*> val store

instance Monad m => Monad (MetricsT m) where
  (MetricsT val) >>= f = MetricsT $ \store -> val store >>= \a -> runMetricsT (f a) store

instance MonadIO m => MonadMetrics (MetricsT m) where
  getMetric metric = MetricsT $ \store -> liftIO $ getMetricStore store metric


instance MonadTrans MetricsT where
  lift m = MetricsT $ const m


instance MonadIO m => MonadIO (MetricsT m) where
  liftIO act = MetricsT $ const $ liftIO act

instance MonadReader r m => MonadReader r (MetricsT m) where
  ask = MetricsT $ const ask
  reader f = MetricsT $ const $ reader f
  local m (MetricsT rFun) = MetricsT $ local m . rFun

instance MonadThrow m => MonadThrow (MetricsT m) where
  throwM ex = MetricsT $ const $ throwM ex

instance MonadCatch m => MonadCatch (MetricsT m) where
  catch (MetricsT act) handler = MetricsT $ \store -> catch (act store) $ \ex -> runMetricsT (handler ex) store
