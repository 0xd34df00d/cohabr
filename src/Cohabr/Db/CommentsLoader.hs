{-# LANGUAGE RankNTypes, FlexibleContexts, ScopedTypeVariables, PolyKinds #-}
{-# LANGUAGE QuasiQuotes, RecordWildCards #-}

module Cohabr.Db.CommentsLoader
( loadComments
, LoadedComments(..)
) where

import qualified Data.IntMap.Strict as IM
import qualified Data.HashSet as HS
import Control.Monad.Except
import Data.List.Extra
import Data.Maybe
import Data.String.Interpolate

import qualified Habr.Types as HT
import Habr.Util
import Cohabr.Db
import Cohabr.Db.HelperTypes
import Cohabr.Db.SqlMonad
import Cohabr.Db.Queries
import Cohabr.Db.Utils

data LoadedComments = LoadedComments
  { commentsTree :: HT.Comments
  , deletedSet :: HS.HashSet CommentHabrId
  } deriving (Eq, Show)

loadComments :: SqlMonad r m => HT.CommentContents -> PostPKey -> m LoadedComments
loadComments defaultContents postId = do
  storedComments <- getPostComments postId
  let authors = nubOrd $ mapMaybe cAuthor storedComments
  avatars <- catMaybes <$> mapM getUserAvatar authors
  let avatarsByUid = IM.fromList [ (getPKeyId $ uaUser avatar, fromStoredAvatar avatar) | avatar <- avatars ]
  case runExcept $ fromStoredComments defaultContents avatarsByUid storedComments of
       Right comments -> pure comments
       Left err -> throwSql err

fromStoredComments :: forall m. MonadError String m => HT.CommentContents -> IM.IntMap HT.Avatar -> [Comment] -> m LoadedComments
fromStoredComments defaultContents avatars comments = do
  commentsTree <- buildCommentsTree <$> mapM convSingle comments
  pure LoadedComments { .. }
  where
    convSingle :: Comment -> m HT.Comment
    convSingle Comment { .. } = do
      parentId <- getParentId (cParent :: Maybe CommentPKey)
      let commentId = getHabrId cSourceId
      let avatar = fromMaybe HT.DefaultAvatar $ cAuthor >>= (`IM.lookup` avatars) . getPKeyId
      let contents
            | Just text <- cText = HT.CommentExisting
                { HT.commentText = text
                , HT.commentChanged = cChanged
                , HT.user = fromMaybe user $ HT.UserInfo <$> cUser <*> pure avatar
                , HT.votes = fromMaybe votes $ HT.Votes <$> cScorePlus <*> cScoreMinus
                , HT.timestamp = fromMaybe timestamp cDate
                }
            | otherwise = HT.CommentDeleted
      pure HT.Comment { .. }

    getParentId Nothing = pure 0
    getParentId (Just parentPKey) | Just val <- IM.lookup (getPKeyId parentPKey) pkey2habrId = pure $ getHabrId val
                                  | otherwise = throwError [i|Unable to find parent comment with id #{parentPKey}|]
    pkey2habrId = IM.fromList [ (getPKeyId cId, cSourceId)
                              | Comment { .. } <- comments
                              ]
    HT.CommentExisting { .. } = defaultContents
    deletedSet = HS.fromList [ cSourceId | Comment { .. } <- comments, cDeleted ]

fromStoredAvatar :: UserAvatar -> HT.Avatar
fromStoredAvatar = HT.CustomAvatar . HT.URL . uaBigImageUrl
