{-# LANGUAGE ConstraintKinds, RankNTypes, FlexibleContexts, ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE DeriveDataTypeable, DeriveAnyClass, DeriveFunctor, StandaloneDeriving #-}

module Cohabr.Fetch.ErrorHandling
( HandlerMaybe
, handler
, catchesMaybe

, httpForbiddenHandler
, httpTimeoutHandler
, httpGenericHandler
, httpHardTimeoutHandler

, handleHttpExceptionPost

, httpWithTimeout

, MetricableSqlMonad
, MetricableSqlMonadRunner

, HttpConfig(..)
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception(throw)
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Functor
import Data.String.Interpolate
import Data.Typeable
import Network.HTTP.Client(HttpException(..), HttpExceptionContent(..))
import Network.HTTP.Conduit hiding(Proxy)
import Network.HTTP.Types.Status(statusCode)

import Cohabr.Db(PostHabrId)
import Cohabr.Metrics
import Cohabr.Db.SqlMonad

data HandlerMaybe m a = forall e. Exception e => HandlerMaybe (e -> Maybe (m a))
deriving instance Functor m => Functor (HandlerMaybe m)

handler :: Exception e => (e -> Maybe (m a)) -> HandlerMaybe m a
handler = HandlerMaybe

catchesMaybe :: (MonadCatch m) => [HandlerMaybe m a] -> m a -> m a
catchesMaybe handlers act = act `catch` tryHandlers
  where
    tryHandlers e | Just hAct <- msum $ tryHandler e <$> handlers = hAct
                  | otherwise = throwM e
    tryHandler e (HandlerMaybe handler)
      | Just e' <- fromException e = handler e'
      | otherwise = Nothing

httpForbiddenHandler :: MetricableSqlMonad r m => PostHabrId -> BS.ByteString -> HttpException -> Maybe (m ())
httpForbiddenHandler habrPostId marker (HttpExceptionRequest _ (StatusCodeException resp respContents))
  | sc == 403 && marker `BS.isInfixOf` respContents = Just act
  | sc == 404 = Just act
  where
    sc = statusCode $ responseStatus resp
    act = track DeniedPagesCount >> writeLog LogInfo [i|Post is unavailable: #{habrPostId}|]
httpForbiddenHandler _ _ _ = Nothing

httpTimeoutHandler :: (MetricableSqlMonad r m, Show ctx) => ctx -> HttpException -> Maybe (m ())
httpTimeoutHandler ctx (HttpExceptionRequest _ ResponseTimeout) = Just $ track TimeoutHttpCount >> writeLog LogWarn ("Request timeout: " <> show ctx)
httpTimeoutHandler _ _ = Nothing

httpGenericHandler :: (MetricableSqlMonad r m, Show ctx) => ctx -> HttpException -> Maybe (m ())
httpGenericHandler ctx ex = Just $ track FailedHttpRequestsCount >> writeLog LogError [i|Generic HTTP error for #{ctx}: #{ex}|]

httpHardTimeoutHandler :: (MetricableSqlMonad r m, Show ctx) => ctx -> HttpTimeout -> Maybe (m ())
httpHardTimeoutHandler ctx _ = Just $ track HardTimeoutHttpCount >> writeLog LogError [i|Hard timeout for #{ctx}|]

handleHttpExceptionPost :: MetricableSqlMonad r m => PostHabrId -> BS.ByteString -> a -> [HandlerMaybe m a]
handleHttpExceptionPost habrPostId marker defVal = ($> defVal) <$>
  [ HandlerMaybe $ httpForbiddenHandler habrPostId marker
  , HandlerMaybe $ httpTimeoutHandler habrPostId
  , HandlerMaybe $ httpGenericHandler habrPostId
  , HandlerMaybe $ httpHardTimeoutHandler habrPostId
  ]

data HttpTimeout = HttpTimeout deriving (Show, Typeable, Exception)

newtype HttpConfig = HttpConfig
  { httpTimeout :: Int
  } deriving (Eq, Ord, Show)

type MetricableSqlMonad r m = (SqlMonad r m, Has HttpConfig r, MonadCatch m, MonadMetrics m)

type MetricableSqlMonadRunner = forall a. (forall r m. MetricableSqlMonad r m => m a) -> IO a

httpWithTimeout :: (MonadReader r m, Has HttpConfig r, MonadIO m) => String -> m LBS.ByteString
httpWithTimeout url = do
  timeout <- asks $ httpTimeout . extract
  either (const $ throw HttpTimeout) id <$> liftIO (threadDelay (timeout * 1000000) `race` simpleHttp url)
