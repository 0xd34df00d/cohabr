{-# LANGUAGE ScopedTypeVariables, RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE RankNTypes, FlexibleContexts, GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Cohabr.Db.Updates
( UpdateField(..)
, RawPostVersion(..)
, ListDiff(..)

, PostUpdateActions(..)
, updatePost
, insertPost
) where

import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PGS
import Control.Monad
import Data.Maybe
import Database.Beam hiding(timestamp)
import Database.Beam.Backend.SQL
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres
import Database.Beam.Postgres.Full hiding(insert)
import Database.Beam.Postgres.Syntax
import qualified Database.Beam.Postgres.Full as BPG
import GHC.Stack

import Cohabr.Db
import Cohabr.Db.HelperTypes
import qualified Habr.Types as HT

data UpdateField table where
  UpdateField :: HasSqlValueSyntax PgValueSyntax a =>
      { accessor :: forall f. table f -> Columnar f a
      , newVal :: a
      } -> UpdateField table

data RawPostVersion = RawPostVersion
  { rawPVTitle :: T.Text
  , rawPVText :: T.Text
  } deriving (Eq, Ord, Show)

data ListDiff a = ListDiff
  { added :: [a]
  , removed :: [a]
  } deriving (Eq, Ord, Show)

data PostUpdateActions = PostUpdateActions
  { postId :: PKeyId
  , postUpdates :: [UpdateField PostT]
  , hubsDiff :: ListDiff HT.Hub
  , newPostVersion :: Maybe RawPostVersion
  }

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

updatePost :: Connection -> PostUpdateActions -> IO ()
updatePost conn PostUpdateActions { .. } = do
  ensureHubsExisting conn $ added hubsDiff

  PGS.withTransaction conn $ runBeamPostgres conn $ do
    maybeNewVersionId <- updatePostVersion postId newPostVersion
    let postUpdates' = postUpdates <> catMaybes [
            (\verId -> UpdateField { accessor = pCurrentVersion, newVal = verId }) <$> maybeNewVersionId
          ]
    currentRow <- runUpdateReturningOne $ update
                    (cPosts cohabrDb)
                    (toUpdaterConcat postUpdates')
                    (\post -> pId post ==. val_ postId)
    updateVersionHubs (pCurrentVersion currentRow) hubsDiff

toUpdater :: Beamable table => UpdateField table -> (forall s. table (QField s) -> QAssignment Postgres s)
toUpdater UpdateField { .. } = \table -> accessor table <-. val_ newVal

toUpdaterConcat :: Beamable table => [UpdateField table] -> (forall s. table (QField s) -> QAssignment Postgres s)
toUpdaterConcat = foldMap toUpdater

updatePostVersion :: MonadBeamInsertReturning Postgres m => PKeyId -> Maybe RawPostVersion -> m (Maybe PKeyId)
updatePostVersion _ Nothing = pure Nothing
updatePostVersion postId (Just RawPostVersion { .. }) =
  fmap (Just . pvId) $ runInsertReturningOne $ insert (cPostsVersions cohabrDb) $ insertExpressions $ pure
    PostVersion
      { pvId = default_
      , pvPostId = val_ postId
      , pvAdded = default_
      , pvTitle = val_ $ Just rawPVTitle
      , pvContent = val_ rawPVText
      }

updateVersionHubs :: MonadBeam Postgres m => PKeyId -> ListDiff HT.Hub -> m ()
updateVersionHubs postVersionId ListDiff { .. } = do
  remCnt <- remove removed
  add added
  unless (remCnt == length removed) $ error "Unexpected removed items count" -- TODO
  where
    remove [] = pure 0
    remove hubs = expectSingleResult =<< runPgDeleteReturningList
                    (deleteReturning
                      (cPostsHubs cohabrDb)
                      (\h -> phHub h `in_` (val_ . makeHubId <$> hubs) &&. phPostVersion h ==. val_ postVersionId)
                      (const countAll_))

    add [] = pure ()
    add hubs = runInsert $ BPG.insert (cPostsHubs cohabrDb) query conflictIgnore
      where query = insertValues $ (\h -> PostHub { phPostVersion = postVersionId, phHub = makeHubId h }) <$> hubs

insertPost :: Connection -> HabrId -> HT.Post -> IO PKeyId
insertPost conn habrId post@HT.Post { .. } = PGS.withTransaction conn $ runBeamPostgres conn $ do
  versionId <- fmap pvId $ runInsertReturningOne $ insert (cPostsVersions cohabrDb) $
                  insertExpressions $ pure $ makePostVersionRecord post
  postId <- fmap pId $ runInsertReturningOne $ insert (cPosts cohabrDb) $
                  insertExpressions $ pure $ makePostRecord habrId versionId post
  updates <- runUpdateReturningList $ update
              (cPostsVersions cohabrDb)
              (\pv -> pvPostId pv <-. val_ postId)
              (\pv -> pvId pv ==. val_ versionId)
  unless (length updates == 1) $ error "Expected one row to be affected by update" -- TODO error handling
  pure postId

makePostVersionRecord :: HT.Post -> forall s. PostVersionT (QExpr Postgres s)
makePostVersionRecord HT.Post { .. } = PostVersion
  { pvId = default_
  , pvPostId = val_ 0
  , pvAdded = default_
  , pvTitle = val_ $ Just title
  , pvContent = val_ body
  }

makePostRecord :: HabrId -> PKeyId -> HT.Post -> forall s. PostT (QExpr Postgres s)
makePostRecord habrId versionId HT.Post { .. } = Post
  { pId = default_
  , pSourceId = val_ habrId
  , pUser = val_ $ Just $ HT.username user
  , pPublished = val_ $ timestamp
  , pLink = val_ $ HT.linkUrl <$> link
  , pLinkName = val_ $ HT.linkName <$> link
  , pScorePlus = val_ $ Just pos
  , pScoreMinus = val_ $ Just neg
  , pOrigViews = val_ $ Just $ HT.viewsCount views
  , pOrigViewsNearly = val_ $ Just $ not $ HT.isExactCount views
  , pCurrentVersion = val_ versionId
  , pAuthor = val_ Nothing
  }
  where
    HT.PostStats { votes = HT.Votes { .. }, .. } = postStats

ensureHubsExisting :: Connection -> [HT.Hub] -> IO ()
ensureHubsExisting conn hubs = runBeamPostgres conn $ runInsert $ BPG.insert (cHubs cohabrDb) query conflictIgnore
  where
    query = insertValues $ (\h -> Hub { hId = makeHubId h, hName = HT.hubName h }) <$> hubs

makeHubId :: HT.Hub -> T.Text
makeHubId HT.Hub { .. } = prefix hubKind <> hubCode
  where
    prefix HT.NormalHub = mempty
    prefix HT.CompanyHub = "company-"

conflictIgnore :: Beamable tbl => PgInsertOnConflict tbl
conflictIgnore = onConflict anyConflict onConflictDoNothing
