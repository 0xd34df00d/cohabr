{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module Cohabr.Db.Utils
( expectSingleResult
, runInsertReturningOne
, runUpdateReturningOne
, conflictIgnore

, ensureHubsExist

, SqlInvariantException
, throwSql
, (||^)
) where

import Control.Exception
import Control.Monad
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres.Full hiding(insert)
import qualified Database.Beam.Postgres.Full as BPG
import GHC.Stack

import Cohabr.Db
import Cohabr.Db.Conversions
import Cohabr.Db.SqlMonad
import qualified Habr.Types as HT

expectSingleResult :: (HasCallStack, Monad m) => [a] -> m a
expectSingleResult [e] = pure e
expectSingleResult lst = throwSql $ "Expected single result, got " <> show (length lst) <> "instead"

runInsertReturningOne :: (HasCallStack,
                          MonadBeamInsertReturning be m,
                          Beamable table,
                          Projectible be (table (QExpr be ())),
                          FromBackendRow be (table Identity))
                      => SqlInsert be table -> m (table Identity)
runInsertReturningOne f = runInsertReturningList f >>= expectSingleResult

runUpdateReturningOne :: (HasCallStack,
                          MonadBeamUpdateReturning be m,
                          Beamable table,
                          Projectible be (table (QExpr be ())),
                          FromBackendRow be (table Identity))
                      => SqlUpdate be table -> m (table Identity)
runUpdateReturningOne f = runUpdateReturningList f >>= expectSingleResult

ensureHubsExist :: SqlMonad m => [HT.Hub] -> m ()
ensureHubsExist hubs = runPg $ runInsert $ BPG.insert (cHubs cohabrDb) query conflictIgnore
  where
    query = insertValues $ (\h -> Hub { hId = makeHubId h, hName = HT.hubName h }) <$> hubs

conflictIgnore :: Beamable tbl => PgInsertOnConflict tbl
conflictIgnore = onConflict anyConflict onConflictDoNothing

data SqlInvariantException = SqlInvariantException
  { stack :: CallStack
  , description :: String
  } deriving (Show, Typeable, Exception)

throwSql :: HasCallStack => String -> a
throwSql = throw . SqlInvariantException callStack

infix 1 ||^
(||^) :: (Applicative f, HasCallStack) => Bool -> String -> f ()
(||^) cond str = unless cond $ throwSql str
