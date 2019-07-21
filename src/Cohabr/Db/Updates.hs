{-# LANGUAGE ScopedTypeVariables, RecordWildCards, QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts, GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Cohabr.Db.Updates
( ListDiff(..)

, updatePost
, postUpdateActions

, updateComments
, commentsUpdatesActions
, newCommentsCount

, StoredPostInfo(..)
, getStoredPostInfo

, bumpPostQueryTime

, bodyFuzzyMatch
) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Control.Monad
import Control.Monad.Reader
import Data.Char
import Data.Maybe
import Data.Monoid
import Data.String.Interpolate
import Data.Tree
import Database.Beam hiding(timestamp)
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres
import Database.Beam.Postgres.Full hiding(insert)

import Cohabr.Db
import Cohabr.Db.Conversions
import Cohabr.Db.HelperTypes
import Cohabr.Db.Inserts
import Cohabr.Db.ListDiff
import Cohabr.Db.Queries
import Cohabr.Db.SqlMonad
import Cohabr.Db.UpdateField
import Cohabr.Db.Utils
import qualified Habr.Types as HT

data RawPostVersion = RawPostVersion
  { rawPVTitle :: T.Text
  , rawPVText :: T.Text
  } deriving (Eq, Ord, Show)

data PostUpdateActions = PostUpdateActions
  { postId :: PostPKey
  , postUpdates :: [UpdateField PostT]
  , hubsDiff :: ListDiff HT.Hub
  , tagsDiff :: ListDiff HT.Tag
  , newPostVersion :: Maybe RawPostVersion
  }

isNullPostUpdate :: PostUpdateActions -> Bool
isNullPostUpdate PostUpdateActions { .. } = null postUpdates
                                         && isNullDiff hubsDiff && isNullDiff tagsDiff
                                         && isNothing newPostVersion

updatePost :: SqlMonad r m => PostUpdateActions -> m ()
updatePost pua@PostUpdateActions { .. } | isNullPostUpdate pua = pure ()
                                        | otherwise = do
  ensureHubsExist $ added hubsDiff
  withTransactionPg `inReader` do
    maybeNewVersionId <- updatePostVersion postId newPostVersion
    let isNewVersion = isJust maybeNewVersionId
    let postUpdates' = postUpdates <> catMaybes [
            (\verId -> UpdateField { accessor = pCurrentVersion, newVal = verId }) <$> maybeNewVersionId
          ]
    unless (null postUpdates') $ void $ runUpdateReturningOne $ update
        (cPosts cohabrDb)
        (toUpdaterConcat postUpdates')
        (\post -> pId post ==. val_ postId)
    Just curVerId <- getCurrentPostVersion postId
    updateVersionHubs curVerId isNewVersion hubsDiff
    updateVersionTags curVerId isNewVersion tagsDiff

bumpPostQueryTime :: SqlMonad r m => PostPKey -> m ()
bumpPostQueryTime postId = runPg $ runUpdate $ update
  (cPosts cohabrDb)
  (\post -> pLastQueried post <-. now_)
  (\post -> pId post ==. val_ postId)

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

updateVersionHubs :: (SqlMonad r m, MonadBeam Postgres m) => PostVersionPKey -> Bool -> ListDiff HT.Hub -> m ()
updateVersionHubs postVersionId isNewVersion ListDiff { .. } | isNewVersion = insertVersionHubs postVersionId allNew
                                                             | otherwise = do
  insertVersionHubs postVersionId added
  remCnt <- remove removed
  unless (remCnt == length removed) $ reader (stmtLogger . extract) >>=
      \logger -> logger LogWarn [i|Unexpected removed hubs count for post version #{postVersionId} #{isNewVersion}|]
  where
    remove [] = pure 0
    remove hubs = length <$> runPgDeleteReturningList
                    (deleteReturning
                      (cPostsHubs cohabrDb)
                      (\h -> phHub h `in_` (val_ . makeHubId <$> hubs) &&. phPostVersion h ==. val_ postVersionId)
                      phPostVersion)      -- TODO if we can count better

updateVersionTags :: (SqlMonad r m, MonadBeam Postgres m) => PostVersionPKey -> Bool -> ListDiff HT.Tag -> m ()
updateVersionTags postVersionId isNewVersion ListDiff { .. } | isNewVersion = insertVersionTags postVersionId allNew
                                                             | otherwise = do
  insertVersionTags postVersionId added
  remCnt <- remove removed
  unless (remCnt == length removed) $ reader (stmtLogger . extract) >>=
      \logger -> logger LogWarn [i|Unexpected removed tags count for post version #{postVersionId} #{isNewVersion}|]
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
  , storedCommentShorts :: [(CommentHabrId, ShortCommentInfo)]
  }

getStoredPostInfo :: SqlMonad r m => PostHabrId -> m (Maybe StoredPostInfo)
getStoredPostInfo habrId = do
  maybePPV <- findPostByHabrId habrId
  case maybePPV of
    Nothing -> pure Nothing
    Just (storedPost, storedCurrentVersion) -> do
      let postVerId = pvId storedCurrentVersion
      storedPostHubs <- fmap fromStoredHub <$> getPostVersionHubs postVerId
      storedPostTags <- fmap fromStoredTag <$> getPostVersionTags postVerId
      storedCommentShorts <- getPostCommentsShorts $ pId storedPost
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
                     body `bodyFuzzyMatch` pvContent storedCurrentVersion = Nothing
                   | otherwise = Just RawPostVersion { rawPVTitle = title, rawPVText = body }

bodyFuzzyMatch :: T.Text -> T.Text -> Bool
bodyFuzzyMatch new old = normalize old == normalize new
  where
    normalize = T.filter isAlphaNum . removeEntities
    safeDrop t = if T.null t' then t' else T.tail t'
      where t' = T.dropWhile (/= ';') t
    removeEntities t = mconcat $ h : fmap safeDrop rest
      where (h:rest) = T.split (== '&') t

data CommentsUpdatesActions = CommentsUpdatesActions
  { commentsPostId :: PostPKey
  , newCommentsSubtrees :: HT.Comments
  , possiblyChangedComments :: [(CommentPKey, T.Text)]
  , updatedVotes :: [(CommentPKey, HT.Votes)]
  , deletedComments :: [CommentPKey]
  }

newCommentsCount :: CommentsUpdatesActions -> Int
newCommentsCount cua = getSum $ mconcat $ Sum . length <$> newCommentsSubtrees cua

updateComments :: forall r m. SqlMonad r m => CommentsUpdatesActions -> m ()
updateComments CommentsUpdatesActions { .. } = withTransactionPg `inReader` do
  insertCommentTree commentsPostId newCommentsSubtrees
  currentCommentsContents <- fmap HM.fromList $ getCommentsContents $ fst <$> possiblyChangedComments
  forM_ possiblyChangedComments $ \(commentId, body) ->
    case HM.lookup commentId currentCommentsContents of
      Just savedText -> when (savedText /= Just body) $ updateCommentText commentId body
      Nothing -> throwSql [i|Unable to find comment text for comment #{commentId}|]
  forM_ updatedVotes $ \(commentId, HT.Votes { .. }) -> runUpdate $ update
    (cComments cohabrDb)
    (\comm -> (cScorePlus comm <-. val_ (Just pos))
           <> (cScoreMinus comm <-. val_ (Just neg)))
    (\comm -> cId comm ==. val_ commentId)
  runUpdate $ update
    (cComments cohabrDb)
    (\comm -> cDeleted comm <-. val_ True)
    (\comm -> cId comm `in_` fmap val_ deletedComments)

updateCommentText :: (MonadBeam Postgres m, SqlMonad r m) => CommentPKey -> T.Text -> m ()
updateCommentText commentId body = runUpdate $ update
                                    (cComments cohabrDb)
                                    (\comm -> (cText comm <-. val_ (Just body)) <> (cChanged comm <-. val_ True))
                                    (\comm -> cId comm ==. val_ commentId)

commentsUpdatesActions :: StoredPostInfo -> HT.Comments -> CommentsUpdatesActions
commentsUpdatesActions spi parsedComments = CommentsUpdatesActions
  { commentsPostId = pId $ storedPost spi
  , newCommentsSubtrees = concatMap (findSubroots $ not . (`HM.member` storedIdsMap) . HabrId . HT.commentId) parsedComments
  , updatedVotes = concatMap (foldl votesUpdater []) parsedComments
  , possiblyChangedComments = mapMaybe makeChanged commentsList
  , deletedComments = mapMaybe makeDeleted commentsList
  }
  where
    makeChanged comment | HT.CommentExisting { .. } <- HT.contents comment
                        , commentChanged
                        , Just info <- HM.lookup (HabrId $ HT.commentId comment) storedIdsMap = Just (commentPKey info, commentText)
                        | otherwise = Nothing
    makeDeleted comment | HT.CommentDeleted <- HT.contents comment
                        , Just ShortCommentInfo { .. } <- HM.lookup (HabrId $ HT.commentId comment) storedIdsMap
                        , not isDeleted = Just commentPKey
                        | otherwise = Nothing
    votesUpdater acc HT.Comment { .. } | HT.CommentExisting { .. } <- contents
                                       , Just ShortCommentInfo { .. } <- HM.lookup (HabrId commentId) storedIdsMap
                                       , (posVotes, negVotes) /= (Just $ HT.pos votes, Just $ HT.neg votes) = (commentPKey, votes) : acc
                                       | otherwise = acc
    storedIdsMap = HM.fromList $ storedCommentShorts spi
    commentsList = concatMap flatten parsedComments

findSubroots :: (a -> Bool) -> Tree a -> [Tree a]
findSubroots p t@Node { .. } | p rootLabel = [t]
                             | otherwise = concat $ findSubroots p <$> subForest
