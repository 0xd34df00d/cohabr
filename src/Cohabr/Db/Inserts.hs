{-# LANGUAGE RecordWildCards, QuasiQuotes #-}
{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module Cohabr.Db.Inserts
( insertPost

, insertVersionHubs
, insertVersionTags

, insertSingleComment
, insertCommentTree
) where

import qualified Data.Text as T
import Data.Maybe
import Data.String.Interpolate
import Data.Tree
import Database.Beam hiding(timestamp)
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres
import qualified Database.Beam.Postgres.Full as BPG
import System.FilePath

import Cohabr.Db
import Cohabr.Db.Conversions
import Cohabr.Db.HelperTypes
import Cohabr.Db.Queries
import Cohabr.Db.SqlMonad
import Cohabr.Db.Utils
import qualified Habr.Types as HT

insertPost :: SqlMonad m => PostHabrId -> HT.Post -> m PostPKey
insertPost habrId post@HT.Post { .. } = do
  userId <- withTransactionPg $ ensureUserExists user
  ensureHubsExist hubs
  withTransactionPg $ do
    versionId <- fmap pvId $ runInsertReturningOne $ insert (cPostsVersions cohabrDb) $
                    insertExpressions [makePostVersionRecord post]
    postId <- fmap pId $ runInsertReturningOne $ insert (cPosts cohabrDb) $
                    insertExpressions [makePostRecord habrId versionId userId post]
    updates <- runUpdateReturningList $ update
                (cPostsVersions cohabrDb)
                (\pv -> pvPostId pv <-. val_ postId)
                (\pv -> pvId pv ==. val_ versionId)
    length updates == 1 ||^
      [i|#{length updates} rows affected by update for post #{habrId}, postId #{postId}, version #{versionId}|]

    insertVersionHubs versionId hubs
    insertVersionTags versionId tags

    insertPostFlags postId flags

    pure postId

makePostVersionRecord :: HT.Post -> forall s. PostVersionT (QExpr Postgres s)
makePostVersionRecord HT.Post { .. } = PostVersion
  { pvId = default_
  , pvPostId = val_ 0
  , pvAdded = default_
  , pvTitle = val_ $ Just title
  , pvContent = val_ body
  }

makePostRecord :: PostHabrId -> PostVersionPKey -> UserPKey -> HT.Post -> forall s. PostT (QExpr Postgres s)
makePostRecord habrId versionId userId HT.Post { .. } = Post
  { pId = default_
  , pSourceId = val_ habrId
  , pUser = val_ $ Just $ HT.username user
  , pPublished = val_ timestamp
  , pLink = val_ $ HT.getUrl . HT.linkUrl <$> link
  , pLinkName = val_ $ HT.linkName <$> link
  , pScorePlus = val_ $ Just pos
  , pScoreMinus = val_ $ Just neg
  , pOrigViews = val_ $ Just $ HT.viewsCount views
  , pOrigViewsNearly = val_ $ Just $ not $ HT.isExactCount views
  , pCurrentVersion = val_ versionId
  , pAuthor = val_ $ Just userId
  , pLastQueried = default_
  }
  where
    HT.PostStats { votes = HT.Votes { .. }, .. } = postStats

ensureUserExists :: HT.UserInfo -> Pg UserPKey
ensureUserExists HT.UserInfo { .. } = do
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
          length updates == 1 ||^ [i|#{length updates} rows affected by avatar update for user #{newUid}|]
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

makeAvatarRecord :: UserPKey -> HT.URL -> forall s. UserAvatarT (QExpr Postgres s)
makeAvatarRecord userId link = UserAvatar
  { uaId = default_
  , uaUser = val_ userId
  , uaBigImageUrl = val_ $ HT.getUrl link
  , uaSmallImageUrl = val_ smallLink
  , uaDiscoveredDate = default_
  }
  where
    linkStr = T.unpack $ HT.getUrl link
    smallLink = T.pack $ replaceBaseName linkStr $ takeBaseName linkStr <> "_small"

insertSingleComment :: SqlMonad m => PostPKey -> HT.Comment -> m CommentPKey
insertSingleComment postId comment = do
  parentCommentId <- case HT.parentId comment of
    0 -> pure Nothing
    cid -> do
      found <- findCommentIdByHabrId postId $ HabrId cid
      isJust found ||^
        [i|Parent comment not found for post #{postId}, comment #{HT.commentId comment} parent comment #{cid}|]
      pure found
  insertSingleCommentWParent postId comment parentCommentId

insertSingleCommentWParent :: SqlMonad m => PostPKey -> HT.Comment -> Maybe CommentPKey -> m CommentPKey
insertSingleCommentWParent postId comment parentCommentId = do
  userId <- case HT.contents comment of
    HT.CommentDeleted -> pure Nothing
    HT.CommentExisting { .. } -> Just <$> runPg (ensureUserExists user)
  let rec = makeCommentRecord postId parentCommentId userId comment
  runPg $ fmap cId $ runInsertReturningOne $
    insert (cComments cohabrDb) $ insertExpressions [rec]

insertCommentTree :: SqlMonad m => PostPKey -> HT.Comments -> m ()
insertCommentTree postId = mapM_ $ \Node { .. } -> do
  thisId <- insertSingleComment postId rootLabel
  insertCommentTreeWParent postId thisId subForest

insertCommentTreeWParent :: SqlMonad m => PostPKey -> CommentPKey -> HT.Comments -> m ()
insertCommentTreeWParent postId parentId = mapM_ $ \Node { .. } -> do
  thisId <- insertSingleCommentWParent postId rootLabel (Just parentId)
  insertCommentTreeWParent postId thisId subForest

makeCommentRecord :: PostPKey -> Maybe CommentPKey -> Maybe UserPKey -> HT.Comment -> forall s. CommentT (QExpr Postgres s)
makeCommentRecord postId parentCommentId userId HT.Comment { .. } = case contents of
  HT.CommentDeleted -> common
    { cDeleted = val_ True
    , cChanged = val_ False
    }
  HT.CommentExisting { .. } -> common
    { cDeleted = val_ False
    , cUser = val_ $ Just $ HT.username user
    , cDate = val_ $ Just timestamp
    , cText = val_ $ Just commentText
    , cChanged = val_ commentChanged
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
      , cChanged = undefined
      , cScorePlus = val_ Nothing
      , cScoreMinus = val_ Nothing
      , cDeleted = undefined
      , cAuthor = val_ Nothing
      }

insertVersionHubs :: MonadBeam Postgres m => PostVersionPKey -> [HT.Hub] -> m ()
insertVersionHubs postVersionId hubs = runInsert $ BPG.insert (cPostsHubs cohabrDb) query conflictIgnore
  where query = insertValues $ (\h -> PostHub { phPostVersion = postVersionId, phHub = makeHubId h }) <$> hubs

insertVersionTags :: MonadBeam Postgres m => PostVersionPKey -> [HT.Tag] -> m ()
insertVersionTags postVersionId tags = runInsert $ BPG.insert (cPostsTags cohabrDb) (insertExpressions $ mkTag <$> tags) conflictIgnore
  where
    mkTag :: HT.Tag -> forall s. PostTagT (QExpr Postgres s)
    mkTag tag = PostTag
      { ptId = default_
      , ptPostVersion = val_ postVersionId
      , ptTag = val_ $ HT.name tag
      }

insertPostFlags :: MonadBeam Postgres m => PostPKey -> [HT.Flag] -> m ()
insertPostFlags postId flags = runInsert $ insert (cPostsFlags cohabrDb) $ insertExpressions $ mkFlag <$> flags
  where
    mkFlag :: HT.Flag -> forall s. PostFlagT (QExpr Postgres s)
    mkFlag flag = PostFlag
                    { pfPost = val_ postId
                    , pfFlag = val_ $ flagToStr flag
                    }
