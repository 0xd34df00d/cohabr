{-# LANGUAGE ScopedTypeVariables, RecordWildCards, OverloadedStrings, QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE FlexibleContexts, GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Cohabr.Db.Updates
( ListDiff(..)

, PostUpdateActions(..)
, updatePost
, postUpdateActions

, StoredPostInfo(..)
, getStoredPostInfo
) where

import qualified Data.HashSet as S
import qualified Data.Text as T
import Control.Monad
import Control.Monad.Reader
import Data.Hashable
import Data.Maybe
import Data.String.Interpolate
import Database.Beam hiding(timestamp)
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres
import Database.Beam.Postgres.Full hiding(insert)

import Cohabr.Db
import Cohabr.Db.Conversions
import Cohabr.Db.Inserts
import Cohabr.Db.Queries
import Cohabr.Db.SqlMonad
import Cohabr.Db.UpdateField
import Cohabr.Db.Utils
import qualified Habr.Types as HT

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
  { postId :: PostPKey
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

updatePostVersion :: MonadBeamInsertReturning Postgres m => PostPKey -> Maybe RawPostVersion -> m (Maybe PostVersionPKey)
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

updateVersionHubs :: (SqlMonad m, MonadBeam Postgres m) => PostVersionPKey -> Bool -> ListDiff HT.Hub -> m ()
updateVersionHubs postVersionId isNewVersion ListDiff { .. } | isNewVersion = insertVersionHubs postVersionId allNew
                                                             | otherwise = do
  insertVersionHubs postVersionId added
  remCnt <- remove removed
  unless (remCnt == length removed) $ reader stmtLogger >>=
      \logger -> liftIO $ logger LogWarn [i|Unexpected removed hubs count for post version #{postVersionId} #{isNewVersion}|]
  where
    remove [] = pure 0
    remove hubs = length <$> runPgDeleteReturningList
                    (deleteReturning
                      (cPostsHubs cohabrDb)
                      (\h -> phHub h `in_` (val_ . makeHubId <$> hubs) &&. phPostVersion h ==. val_ postVersionId)
                      phPostVersion)      -- TODO if we can count better

updateVersionTags :: (SqlMonad m, MonadBeam Postgres m) => PostVersionPKey -> Bool -> ListDiff HT.Tag -> m ()
updateVersionTags postVersionId isNewVersion ListDiff { .. } | isNewVersion = insertVersionTags postVersionId allNew
                                                             | otherwise = do
  insertVersionTags postVersionId added
  remCnt <- remove removed
  unless (remCnt == length removed) $ reader stmtLogger >>=
      \logger -> liftIO $ logger LogWarn [i|Unexpected removed tags count for post version #{postVersionId} #{isNewVersion}|]
  where
    remove [] = pure 0
    remove tags = length <$> runPgDeleteReturningList
                    (deleteReturning
                      (cPostsTags cohabrDb)
                      (\h -> ptTag h `in_` (val_ . HT.name <$> tags) &&. ptPostVersion h ==. val_ postVersionId)
                      ptId)

data StoredPostInfo = StoredPostInfo
  { storedPost :: Post
  , storedCurrentVersion :: PostVersion
  , storedPostHubs :: [HT.Hub]
  , storedPostTags :: [HT.Tag]
  }

getStoredPostInfo :: SqlMonad m => PostHabrId -> m (Maybe StoredPostInfo)
getStoredPostInfo habrId = do
  maybePPV <- findPostByHabrId habrId
  case maybePPV of
    Nothing -> pure Nothing
    Just (storedPost, storedCurrentVersion) -> do
      let postVerId = pvId storedCurrentVersion
      storedPostHubs <- fmap fromStoredHub <$> getPostVersionHubs postVerId
      storedPostTags <- fmap fromStoredTag <$> getPostVersionTags postVerId
      pure $ Just StoredPostInfo { .. }

postUpdateActions :: StoredPostInfo -> HT.Post -> PostUpdateActions
postUpdateActions StoredPostInfo { .. } HT.Post { .. } = PostUpdateActions { .. }
  where
    postId = pId storedPost
    hubsDiff = calcDiff storedPostHubs hubs
    tagsDiff = calcDiff storedPostTags tags

    HT.PostStats { HT.votes = HT.Votes { .. }, .. } = postStats

    postUpdates = catMaybes [upScorePlus, upScoreMinus, upOrigViews, upOrigViewsNearly]
    upScorePlus = produceUpdateField storedPost pScorePlus pos
    upScoreMinus = produceUpdateField storedPost pScoreMinus neg
    upOrigViews = produceUpdateField storedPost pOrigViews $ HT.viewsCount views
    upOrigViewsNearly = produceUpdateField storedPost pOrigViewsNearly $ not $ HT.isExactCount views

    newPostVersion | Just title == pvTitle storedCurrentVersion &&
                     body == pvContent storedCurrentVersion = Nothing
                   | otherwise = Just RawPostVersion { rawPVTitle = title, rawPVText = body }

calcDiff :: (Eq a, Hashable a) => [a] -> [a] -> ListDiff a
calcDiff (S.fromList -> stored) allNew@(S.fromList -> parsed) = ListDiff { .. }
  where
    added = S.toList $ parsed `S.difference` stored
    removed = S.toList $ stored `S.difference` parsed
