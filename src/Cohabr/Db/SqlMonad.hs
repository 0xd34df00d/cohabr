{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes, ConstraintKinds, FlexibleContexts #-}

module Cohabr.Db.SqlMonad
( SqlEnv(..)
, LogMessageContext(..)
, SqlMonad
, withTransactionPg
, runPg
, inReader
) where

import qualified Database.PostgreSQL.Simple as PGS
import Control.Monad.Reader
import Database.Beam.Postgres
import GHC.Stack

data LogMessageContext = LogSqlStmt | LogDebug | LogWarn
  deriving (Eq, Ord, Show)

data SqlEnv = SqlEnv
  { conn :: Connection
  , stmtLogger :: forall m. (HasCallStack, MonadIO m) => LogMessageContext -> String -> m ()
  }

type SqlMonad m = (MonadReader SqlEnv m, MonadIO m)

withTransactionPg :: SqlMonad m => Pg a -> m a
withTransactionPg pg = do
  SqlEnv { .. } <- ask
  liftIO $ PGS.withTransaction conn $ runBeamPostgresDebug (stmtLogger LogSqlStmt) conn pg

runPg :: SqlMonad m => Pg a -> m a
runPg pg = do
  SqlEnv { .. } <- ask
  liftIO $ runBeamPostgresDebug (stmtLogger LogSqlStmt) conn pg

inReader :: MonadReader r mr => (m a -> mr b) -> ReaderT r m a -> mr b
inReader runner act = ask >>= \env -> runner $ runReaderT act env
