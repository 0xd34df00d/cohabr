{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes, ConstraintKinds, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances #-}

module Cohabr.Db.SqlMonad
( SqlEnv(..)
, LogMessageContext(..)
, Logger
, LoggerHolder(..)
, Has(..)
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

class Has part r where
  getPart :: r -> part


type SqlMonad r m = (MonadReader r m, Has SqlEnv r, MonadIO m)

withTransactionPg :: SqlMonad r m => Pg a -> m a
withTransactionPg pg = do
  SqlEnv { .. } <- asks getPart
  liftIO $ PGS.withTransaction conn $ runBeamPostgresDebug (stmtLogger LogSqlStmt) conn pg

runPg :: SqlMonad r m => Pg a -> m a
runPg pg = do
  SqlEnv { .. } <- asks getPart
  liftIO $ runBeamPostgresDebug (stmtLogger LogSqlStmt) conn pg

inReader :: MonadReader r mr => (m a -> mr b) -> ReaderT r m a -> mr b
inReader runner act = ask >>= \env -> runner $ runReaderT act env

writeLog :: (HasCallStack, SqlMonad r m) => LogMessageContext -> String -> m ()
writeLog ctx str = reader (stmtLogger . getPart) >>= \logger -> logger ctx str
