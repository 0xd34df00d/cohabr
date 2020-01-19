{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes, ConstraintKinds, FlexibleContexts #-}

module Cohabr.Db.SqlMonad
( SqlEnv(..)
, LogMessageContext(..)
, SqlMonad
, SqlLoggerHolder(..)
, withTransactionPg
, runPg
, inReader

, module X
) where

import qualified Database.PostgreSQL.Simple as PGS
import qualified Control.Monad.Reader as R
import Control.Monad.Reader.Has as X hiding(update)
import Database.Beam.Postgres
import GHC.Stack

import Cohabr.Logger

type SqlLogger = forall m. (HasCallStack, MonadIO m) => String -> m ()

newtype SqlLoggerHolder = SqlLoggerHolder { getSqlLogger :: SqlLogger }

data SqlEnv = SqlEnv
  { conn :: Connection
  , stmtLogger :: SqlLogger
  }

type SqlMonad r m = (MonadReader r m, Has SqlEnv r, MonadIO m)

withTransactionPg :: SqlMonad r m => Pg a -> m a
withTransactionPg pg = do
  SqlEnv { .. } <- ask
  liftIO $ PGS.withTransaction conn $ runBeamPostgresDebug stmtLogger conn pg

runPg :: SqlMonad r m => Pg a -> m a
runPg pg = do
  SqlEnv { .. } <- ask
  liftIO $ runBeamPostgresDebug stmtLogger conn pg

inReader :: MonadReader r mr => (m a -> mr b) -> ReaderT r m a -> mr b
inReader runner act = R.ask >>= \env -> runner $ runReaderT act env
