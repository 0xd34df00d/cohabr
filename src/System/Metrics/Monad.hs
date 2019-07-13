{-# LANGUAGE DataKinds, RankNTypes, GADTs, PolyKinds, TypeFamilyDependencies, TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Metrics.Monad
( MetricsStore
, newMetricsStore
, withMetricsStore

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

import qualified Data.Dependent.Map as DM
import qualified Data.Text as T
import Control.Monad.Catch
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TQueue
import Data.GADT.Compare
import Data.Proxy
import Data.Typeable(eqT)
import Data.Type.Equality
import GHC.Int
import GHC.TypeLits
import System.Remote.Monitoring
import System.Metrics
import System.Metrics.Counter as TC
import System.Metrics.Distribution as TD
import System.Metrics.Gauge as TG
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

data SomeMetric tracker where
  MkSomeMetric :: (Typeable metric, TrackerLike tracker, KnownSymbol name, Ord (metric tracker name))
               => metric tracker name -> SomeMetric tracker

deriving instance Typeable (SomeMetric t)

instance GEq SomeMetric where
  geq (MkSomeMetric _) (MkSomeMetric _) = eqT

instance GCompare SomeMetric where
  gcompare sm1@(MkSomeMetric (m1 :: mTy1 tTy1 nTy1)) sm2@(MkSomeMetric (m2 :: mTy2 tTy2 nTy2)) =
    case eqT :: Maybe (tTy1 :~: tTy2) of
      Just Refl -> case compare (toDyn m1 :: DynOrd) (toDyn m2) of
        LT -> GLT
        EQ -> GEQ
        GT -> GGT
      Nothing -> case compare (someTypeRep sm1) (someTypeRep sm2) of
        LT -> GLT
        EQ -> error "SomeTypeReps are equal though eqT proved them wrong"
        GT -> GGT

data MetricsState = MetricsState
  { server :: Server
  , metrics :: DM.DMap SomeMetric Identity
  }

data MetricRequest where
  MetricRequest :: (TrackerLike tracker, KnownSymbol name, Typeable metric, Ord (metric tracker name))
                => metric tracker name
                -> MVar tracker
                -> MetricRequest

newtype MetricsStore = MetricsStore { mReqQueue :: TQueue MetricRequest }

newMetricsStore :: Server -> IO (MetricsStore, IO ())
newMetricsStore srv = do
  queue <- newTQueueIO
  threadId <- forkIO $ act queue $ MetricsState srv mempty
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
      let asSome = MkSomeMetric metric
      (tracker, state') <- case DM.lookup asSome $ metrics state of
        Just existing -> pure (runIdentity existing, state)
        Nothing -> do
          let trackerName = symbolVal (Proxy :: Proxy name)
          newTracker <- createTracker (T.pack trackerName) $ serverMetricStore $ server state
          pure (newTracker, state { metrics = DM.insert asSome (Identity newTracker) $ metrics state })
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
  createTracker :: T.Text -> Store -> IO tracker

instance TrackerLike Counter where
  type TrackAction Counter m = m ()
  track metric = getMetric metric >>= liftIO . TC.inc
  createTracker = createCounter

instance TrackerLike Distribution where
  type TrackAction Distribution m = Double -> m ()
  track metric val = getMetric metric >>= \distr -> liftIO $ TD.add distr val
  createTracker = createDistribution

instance TrackerLike Gauge where
  type TrackAction Gauge m = Int64 -> m ()
  track metric val = getMetric metric >>= \gauge -> liftIO $ TG.set gauge val
  createTracker = createGauge

newtype DistrGauge = DistrGauge (Distribution, Gauge)

instance TrackerLike DistrGauge where
  type TrackAction DistrGauge m = Int64 -> m ()
  track metric val = do
    DistrGauge (distr, gauge) <- getMetric metric
    liftIO $ do
      TG.add gauge val
      TD.add distr $ fromIntegral val
  createTracker name store = do
    d <- createDistribution (name <> "_distr") store
    g <- createGauge (name <> "_total") store
    pure $ DistrGauge (d, g)

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
