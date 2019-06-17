{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes, ConstraintKinds, FlexibleContexts #-}

module Cohabr.Db.SqlMonad
( SqlEnv(..)
, LogMessageContext(..)
, SqlMonad
, withTransactionPg
, runPg
) where

import qualified Database.PostgreSQL.Simple as PGS
import Control.Monad.Reader
import Database.Beam.Postgres
import GHC.Stack

data LogMessageContext = LogSqlStmt | LogDebug | LogWarn
  deriving (Eq, Ord, Show)

data SqlEnv = SqlEnv
  { conn :: Connection
  , stmtLogger :: HasCallStack => LogMessageContext -> String -> IO ()
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
