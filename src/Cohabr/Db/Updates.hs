{-# LANGUAGE ScopedTypeVariables, RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE RankNTypes, FlexibleContexts, GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Cohabr.Db.Updates
( UpdateField(..)
, RawPostVersion(..)
, ListDiff(..)

, PostUpdateActions(..)
, updatePost
) where

import qualified Data.Text as T
import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import Database.Beam hiding(timestamp)
import Database.Beam.Backend.SQL
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres
import Database.Beam.Postgres.Full hiding(insert)
import Database.Beam.Postgres.Syntax

import Cohabr.Db
import Cohabr.Db.Inserts
import Cohabr.Db.HelperTypes
import Cohabr.Db.Utils
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
  , allNew :: [a]
  } deriving (Eq, Ord, Show)

data PostUpdateActions = PostUpdateActions
  { postId :: PKeyId
  , postUpdates :: [UpdateField PostT]
  , hubsDiff :: ListDiff HT.Hub
  , tagsDiff :: ListDiff HT.Tag
  , newPostVersion :: Maybe RawPostVersion
  }

updatePost :: SqlMonad m => PostUpdateActions -> m ()
updatePost PostUpdateActions { .. } = do
  ensureHubsExist $ added hubsDiff
  env <- ask
  withTransactionPg $ flip runReaderT env $ do
    maybeNewVersionId <- updatePostVersion postId newPostVersion
    let isNewVersion = isJust maybeNewVersionId
    let postUpdates' = postUpdates <> catMaybes [
            (\verId -> UpdateField { accessor = pCurrentVersion, newVal = verId }) <$> maybeNewVersionId
          ]
    currentRow <- runUpdateReturningOne $ update
                    (cPosts cohabrDb)
                    (toUpdaterConcat postUpdates')
                    (\post -> pId post ==. val_ postId)
    let curVerId = pCurrentVersion currentRow
    updateVersionHubs curVerId isNewVersion hubsDiff
    updateVersionTags curVerId isNewVersion tagsDiff

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

updateVersionHubs :: (SqlMonad m, MonadBeam Postgres m) => PKeyId -> Bool -> ListDiff HT.Hub -> m ()
updateVersionHubs postVersionId isNewVersion ListDiff { .. } | isNewVersion = insertVersionHubs postVersionId allNew
                                                             | otherwise = do
  insertVersionHubs postVersionId added
  remCnt <- remove removed
  unless (remCnt == length removed) $ error "Unexpected removed items count" -- TODO
  where
    remove [] = pure 0
    remove hubs = length <$> runPgDeleteReturningList
                    (deleteReturning
                      (cPostsHubs cohabrDb)
                      (\h -> phHub h `in_` (val_ . makeHubId <$> hubs) &&. phPostVersion h ==. val_ postVersionId)
                      (phPostVersion))      -- TODO if we can count better

updateVersionTags :: (SqlMonad m, MonadBeam Postgres m) => PKeyId -> Bool -> ListDiff HT.Tag -> m ()
updateVersionTags postVersionId isNewVersion ListDiff { .. } | isNewVersion = insertVersionTags postVersionId allNew
                                                             | otherwise = do
  insertVersionTags postVersionId added
  remCnt <- remove removed
  unless (remCnt == length removed) $ error "Unexpected removed items count" -- TODO
  where
    remove [] = pure 0
    remove tags = length <$> runPgDeleteReturningList
                    (deleteReturning
                      (cPostsTags cohabrDb)
                      (\h -> ptTag h `in_` (val_ . HT.name <$> tags) &&. ptPostVersion h ==. val_ postVersionId)
                      (ptId))
