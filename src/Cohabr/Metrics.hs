{-# LANGUAGE DataKinds, RankNTypes, GADTs, TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Cohabr.Metrics
( MetricsStore
, newMetricsStore
, withMetricsStore
, getMetric
, Metric(..)
, track
) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TQueue
import Control.Exception
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
  MetricRequest :: forall tracker name. (TrackerLike tracker, KnownSymbol name)
                => Metric tracker name
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

    handleReq :: forall tracker name. (TrackerLike tracker, KnownSymbol name)
              => MetricsState
              -> Metric tracker name
              -> MVar tracker
              -> IO MetricsState
    handleReq state metric mvar = do
      let tMap = state^.trackerMap
      let metricDyn = toDyn metric
      (tracker, state') <- case M.lookup metricDyn tMap of
        Just existing -> pure (existing, state)
        Nothing -> do
          let trackerName = symbolVal (Proxy :: Proxy name)
          newTracker <- createTracker (T.pack trackerName) $ serverMetricStore $ state^.server
          pure (newTracker, state & trackerMap .~ M.insert metricDyn newTracker tMap)
      putMVar mvar tracker
      pure state'

withMetricsStore :: Server -> (MetricsStore -> IO a) -> IO a
withMetricsStore srv f = bracket
  (newMetricsStore srv)
  snd
  (f . fst)

class Typeable tracker => TrackerLike tracker where
  type TrackAction tracker :: *
  track :: KnownSymbol name => MetricsStore -> Metric tracker name -> TrackAction tracker
  trackerMap :: Lens' MetricsState (M.Map DynOrd tracker)
  createTracker :: T.Text -> Store -> IO tracker

instance TrackerLike Counter where
  type TrackAction Counter = IO ()
  track store metric = getMetric store metric >>= TC.inc
  trackerMap = counters
  createTracker = createCounter

instance TrackerLike Distribution where
  type TrackAction Distribution = Double -> IO ()
  track store metric val = getMetric store metric >>= flip TD.add val
  trackerMap = distributions
  createTracker = createDistribution

data Metric tracker name where
  PageParseTime   :: Metric Distribution "page.parse_ms"
  PageFetchTime   :: Metric Distribution "page.fetch_ms"
  NumPagesFetched :: Metric Counter      "page.fetches_count"
  deriving (Typeable)

deriving instance Eq (Metric tracker name)
deriving instance Ord (Metric tracker name)

getMetric :: forall tracker name. (TrackerLike tracker, KnownSymbol name)
          => MetricsStore
          -> Metric tracker name
          -> IO tracker
getMetric store metric = do
  mvar <- newEmptyMVar
  atomically $ writeTQueue (mReqQueue store) $ MetricRequest metric mvar
  takeMVar mvar
