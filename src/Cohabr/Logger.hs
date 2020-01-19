{-# LANGUAGE RankNTypes, FlexibleContexts, ConstraintKinds #-}

module Cohabr.Logger where

import Control.Monad.IO.Class
import Control.Monad.Reader.Has
import GHC.Stack

data LogMessageContext = LogDebug | LogInfo | LogWarn | LogError
  deriving (Eq, Ord, Show)

type Logger = forall m. (HasCallStack, MonadIO m) => LogMessageContext -> String -> m ()

newtype LoggerHolder = LoggerHolder { getLogger :: Logger }

type LoggerMonad r m = (MonadReader r m, Has LoggerHolder r, MonadIO m)

writeLog :: (HasCallStack, LoggerMonad r m) => LogMessageContext -> String -> m ()
writeLog ctx str = reader getLogger >>= \l -> l ctx str
