{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes, ConstraintKinds, FlexibleContexts #-}

module Cohabr.Db.SqlMonad
( SqlEnv(..)
, LogMessageContext(..)
, Logger
, LoggerHolder(..)
, SqlMonad
, withTransactionPg
, runPg
, inReader
, writeLog
) where

import qualified Database.PostgreSQL.Simple as PGS
import Control.Monad.Reader
import Database.Beam.Postgres
import GHC.Stack

data LogMessageContext = LogSqlStmt | LogDebug | LogWarn | LogError
  deriving (Eq, Ord, Show)

type Logger = forall m. (HasCallStack, MonadIO m) => LogMessageContext -> String -> m ()

newtype LoggerHolder = LoggerHolder { getLogger :: Logger }

data SqlEnv = SqlEnv
  { conn :: Connection
  , stmtLogger :: Logger
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

writeLog :: (HasCallStack, SqlMonad m) => LogMessageContext -> String -> m ()
writeLog ctx str = reader stmtLogger >>= \logger -> logger ctx str
