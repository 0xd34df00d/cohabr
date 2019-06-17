{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module Cohabr.Db.Utils
( expectSingleResult
, runInsertReturningOne
, runUpdateReturningOne
, conflictIgnore

, ensureHubsExist
, makeHubId

, fromStoredHub
, fromStoredTag
, flagToStr
, strToFlag

, SqlInvariantException
, throwSql
, (||^)
) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Control.Exception
import Control.Monad
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres.Full hiding(insert)
import qualified Database.Beam.Postgres.Full as BPG
import GHC.Stack

import Cohabr.Db
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

fromStoredTag :: PostTag -> HT.Tag
fromStoredTag = HT.Tag . ptTag

conflictIgnore :: Beamable tbl => PgInsertOnConflict tbl
conflictIgnore = onConflict anyConflict onConflictDoNothing

flagToStr :: HT.Flag -> T.Text
flagToStr flag = case flag of
                      HT.RssFeed      -> "rss_feed"
                      HT.Draftbox     -> "draftbox"
                      HT.News         -> "news"
                      HT.Recovery     -> "recovery"
                      HT.Tutorial     -> "tutorial"
                      HT.Translation  -> "translation"
                      HT.Sandbox      -> "sandbox"

strToFlag :: T.Text -> Maybe HT.Flag
strToFlag str = HM.lookup str strToFlagMap

strToFlagMap :: HM.HashMap T.Text HT.Flag
strToFlagMap = HM.fromList [ (flagToStr flag, flag)
                           | flag <- [minBound .. maxBound]
                           ]

data SqlInvariantException = SqlInvariantException
  { stack :: CallStack
  , description :: String
  } deriving (Show, Typeable, Exception)

throwSql :: HasCallStack => String -> a
throwSql = throw . SqlInvariantException callStack

infix 1 ||^
(||^) :: (Applicative f, HasCallStack) => Bool -> String -> f ()
(||^) cond str = unless cond $ throwSql str
