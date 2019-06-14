{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module Cohabr.Db.Inserts
( insertPost
, insertComment
, insertVersionHubs
, insertVersionTags
) where

import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PGS
import Control.Monad
import Data.Maybe
import Database.Beam hiding(timestamp)
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres
import qualified Database.Beam.Postgres.Full as BPG
import System.FilePath

import Cohabr.Db
import Cohabr.Db.HelperTypes
import Cohabr.Db.Queries
import Cohabr.Db.Utils
import qualified Habr.Types as HT

insertPost :: Connection -> HabrId -> HT.Post -> IO PKeyId
insertPost conn habrId post@HT.Post { .. } = do
  userId <- ensureUserExists conn user
  PGS.withTransaction conn $ runBeamPostgresDebug putStrLn conn $ do
    versionId <- fmap pvId $ runInsertReturningOne $ insert (cPostsVersions cohabrDb) $
                    insertExpressions [makePostVersionRecord post]
    postId <- fmap pId $ runInsertReturningOne $ insert (cPosts cohabrDb) $
                    insertExpressions [makePostRecord habrId versionId userId post]
    updates <- runUpdateReturningList $ update
                (cPostsVersions cohabrDb)
                (\pv -> pvPostId pv <-. val_ postId)
                (\pv -> pvId pv ==. val_ versionId)
    unless (length updates == 1) $ error "Expected one row to be affected by update" -- TODO error handling

    insertVersionHubs versionId hubs
    insertVersionTags versionId tags

    pure postId

makePostVersionRecord :: HT.Post -> forall s. PostVersionT (QExpr Postgres s)
makePostVersionRecord HT.Post { .. } = PostVersion
  { pvId = default_
  , pvPostId = val_ 0
  , pvAdded = default_
  , pvTitle = val_ $ Just title
  , pvContent = val_ body
  }

makePostRecord :: HabrId -> PKeyId -> PKeyId -> HT.Post -> forall s. PostT (QExpr Postgres s)
makePostRecord habrId versionId userId HT.Post { .. } = Post
  { pId = default_
  , pSourceId = val_ habrId
  , pUser = val_ $ Just $ HT.username user
  , pPublished = val_ timestamp
  , pLink = val_ $ HT.linkUrl <$> link
  , pLinkName = val_ $ HT.linkName <$> link
  , pScorePlus = val_ $ Just pos
  , pScoreMinus = val_ $ Just neg
  , pOrigViews = val_ $ Just $ HT.viewsCount views
  , pOrigViewsNearly = val_ $ Just $ not $ HT.isExactCount views
  , pCurrentVersion = val_ versionId
  , pAuthor = val_ $ Just userId
  }
  where
    HT.PostStats { votes = HT.Votes { .. }, .. } = postStats

ensureUserExists :: Connection -> HT.UserInfo -> IO PKeyId
ensureUserExists conn HT.UserInfo { .. } = runBeamPostgresDebug putStrLn conn $ do
  maybeId <- runSelectReturningOne $ select query
  case maybeId of
    Just userId -> pure userId
    Nothing -> do
      newUid <- fmap uId $ runInsertReturningOne $
                  insert (cUsers cohabrDb) $ insertExpressions [makeUserRecord username]
      case avatar of
        HT.DefaultAvatar {} -> pure ()
        HT.CustomAvatar { .. } -> do
          avatarId <- fmap uaId $ runInsertReturningOne $
                        insert (cUserAvatars cohabrDb) $ insertExpressions [makeAvatarRecord newUid avatarLink]
          updates <- runUpdateReturningList $ update
                        (cUsers cohabrDb)
                        (\u -> uCurrentAvatar u <-. val_ (Just avatarId))
                        (\u -> uId u ==. val_ newUid)
          unless (length updates == 1) $ error "Expected one row to be affected by update" -- TODO error handling
      pure newUid
  where
    query = fmap uId $ filter_ (\u -> uUsername u ==. val_ username) $ all_ $ cUsers cohabrDb

makeUserRecord :: T.Text -> forall s. UserT (QExpr Postgres s)
makeUserRecord username = User
  { uId = default_
  , uUsername = val_ username
  , uSourceId = val_ Nothing
  , uName = val_ Nothing
  , uNameLastUpdated = val_ Nothing
  , uSpecialization = val_ Nothing
  , uSpecializationLastUpdated = val_ Nothing
  , uKarma = val_ Nothing
  , uKarmaVotes = val_ Nothing
  , uKarmaLastUpdated = val_ Nothing
  , uRating = val_ Nothing
  , uRatingLastUpdated = val_ Nothing
  , uCurrentAvatar = val_ Nothing
  , uCurrentAvatarLastCheck = val_ Nothing
  , uDeleted = val_ False
  }

makeAvatarRecord :: PKeyId -> T.Text -> forall s. UserAvatarT (QExpr Postgres s)
makeAvatarRecord userId link = UserAvatar
  { uaId = default_
  , uaUser = val_ userId
  , uaBigImageUrl = val_ link
  , uaSmallImageUrl = val_ smallLink
  , discoveredDate = default_
  }
  where
    linkStr = T.unpack link
    smallLink = T.pack $ replaceBaseName linkStr $ takeBaseName linkStr <> "_small"

insertComment :: Connection -> PKeyId -> HT.Comment -> IO PKeyId
insertComment conn postId comment = do
  parentCommentId <- case HT.parentId comment of
    0 -> pure Nothing
    cid -> do
      found <- findCommentIdByHabrId conn $ HabrId cid
      guard $ isJust found             -- TODO error handling
      pure found
  userId <- case HT.contents comment of
    HT.CommentDeleted -> pure Nothing
    HT.CommentExisting { .. } -> Just <$> ensureUserExists conn user
  let rec = makeCommentRecord postId parentCommentId userId comment
  runBeamPostgresDebug putStrLn conn $ fmap cId $ runInsertReturningOne $
    insert (cComments cohabrDb) $ insertExpressions [rec]

makeCommentRecord :: PKeyId -> Maybe PKeyId -> Maybe PKeyId -> HT.Comment -> forall s. CommentT (QExpr Postgres s)
makeCommentRecord postId parentCommentId userId HT.Comment { .. } = case contents of
  HT.CommentDeleted -> common
    { cDeleted = val_ $ Just True
    }
  HT.CommentExisting { .. } -> common
    { cDeleted = val_ $ Just False
    , cUser = val_ $ Just $ HT.username user
    , cDate = val_ $ Just timestamp
    , cText = val_ $ Just commentText
    , cChanged = val_ $ Just False
    , cScorePlus = val_ $ Just $ HT.pos votes
    , cScoreMinus = val_ $ Just $ HT.neg votes
    , cAuthor = val_ userId
    }
  where
    common :: forall s. CommentT (QExpr Postgres s)
    common = Comment
      { cId = default_
      , cSourceId = val_ $ HabrId commentId
      , cParent = val_ parentCommentId
      , cPostId = val_ postId
      , cUser = val_ Nothing
      , cDate = val_ Nothing
      , cText = val_ Nothing
      , cChanged = val_ Nothing
      , cScorePlus = val_ Nothing
      , cScoreMinus = val_ Nothing
      , cDeleted = val_ Nothing
      , cAuthor = val_ Nothing
      }

insertVersionHubs :: MonadBeam Postgres m => PKeyId -> [HT.Hub] -> m ()
insertVersionHubs postVersionId hubs = runInsert $ BPG.insert (cPostsHubs cohabrDb) query conflictIgnore
  where query = insertValues $ (\h -> PostHub { phPostVersion = postVersionId, phHub = makeHubId h }) <$> hubs

insertVersionTags :: MonadBeam Postgres m => PKeyId -> [HT.Tag] -> m ()
insertVersionTags postVersionId tags = runInsert $ BPG.insert (cPostsTags cohabrDb) (insertExpressions $ mkTag <$> tags) conflictIgnore
  where
    mkTag :: HT.Tag -> forall s. PostTagT (QExpr Postgres s)
    mkTag tag = PostTag
      { ptId = default_
      , ptPostVersion = val_ postVersionId
      , ptTag = val_ $ HT.name tag
      }
