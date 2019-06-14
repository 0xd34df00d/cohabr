{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}

module Cohabr.Db.Utils
( expectSingleResult
, runInsertReturningOne
, runUpdateReturningOne
, ensureHubsExist
, makeHubId
, fromStoredHub
, conflictIgnore
) where

import qualified Data.Text as T
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres
import Database.Beam.Postgres.Full hiding(insert)
import qualified Database.Beam.Postgres.Full as BPG
import GHC.Stack

import Cohabr.Db
import qualified Habr.Types as HT

expectSingleResult :: (HasCallStack, Monad m) => [a] -> m a
expectSingleResult [e] = pure e
expectSingleResult _ = error $ "Expected single ID at:\n" <> prettyCallStack callStack -- TODO error handling

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

ensureHubsExist :: Connection -> [HT.Hub] -> IO ()
ensureHubsExist conn hubs = runBeamPostgres conn $ runInsert $ BPG.insert (cHubs cohabrDb) query conflictIgnore
  where
    query = insertValues $ (\h -> Hub { hId = makeHubId h, hName = HT.hubName h }) <$> hubs

makeHubId :: HT.Hub -> T.Text
makeHubId HT.Hub { .. } = prefix hubKind <> hubCode
  where
    prefix HT.NormalHub = mempty
    prefix HT.CompanyHub = "company-"

fromStoredHub :: (PostHub, Hub) -> HT.Hub
fromStoredHub (PostHub { .. }, Hub { .. }) = HT.Hub code hName kind
  where
    (code, kind) | not $ cmpPref `T.isPrefixOf` hId = (hId, HT.NormalHub)
                 | otherwise = (T.drop (T.length cmpPref) hId, HT.CompanyHub)
    cmpPref = "company-"

conflictIgnore :: Beamable tbl => PgInsertOnConflict tbl
conflictIgnore = onConflict anyConflict onConflictDoNothing
