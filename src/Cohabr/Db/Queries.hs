{-# LANGUAGE FlexibleContexts #-}

module Cohabr.Db.Queries where

import Data.List
import Database.Beam

import Cohabr.Db
import Cohabr.Db.SqlMonad

selectMissingPosts :: SqlMonad m => [PostHabrId] -> m [PostHabrId]
selectMissingPosts candidates = (candidates \\) <$> runPg (runSelectReturningList $ select query)
  where query = filter_ (`in_` (val_ <$> candidates)) $ fmap pSourceId $ all_ $ cPosts cohabrDb

findPostByHabrId :: SqlMonad m => PostHabrId -> m (Maybe (Post, PostVersion))
findPostByHabrId habrId = runPg $ runSelectReturningOne $ select query
  where
    query = do
      post <- filter_ (\post -> pSourceId post ==. val_ habrId) $ all_ $ cPosts cohabrDb
      postVersion <- filter_ (\postVersion -> pvId postVersion ==. pCurrentVersion post) $ all_ $ cPostsVersions cohabrDb
      pure (post, postVersion)

getPostVersionHubs :: SqlMonad m => PostVersionPKey -> m [(PostHub, Hub)]
getPostVersionHubs postVersion = runPg $ runSelectReturningList $ select query
  where
    query = do
      postHub <- filter_ (\ph -> phPostVersion ph ==. val_ postVersion) $ all_ $ cPostsHubs cohabrDb
      hub <- filter_ (\h -> hId h ==. phHub postHub) $ all_ $ cHubs cohabrDb
      pure (postHub, hub)

getPostVersionTags :: SqlMonad m => PostVersionPKey -> m [PostTag]
getPostVersionTags postVersion = runPg $ runSelectReturningList $ select query
  where query = filter_ (\pt -> ptPostVersion pt ==. val_ postVersion) $ all_ $ cPostsTags cohabrDb

getPostFlags :: SqlMonad m => PostPKey -> m [PostFlag]
getPostFlags postId = runPg $ runSelectReturningList $ select query
  where query = filter_ (\pf -> pfPost pf ==. val_ postId) $ all_ $ cPostsFlags cohabrDb

findCommentIdByHabrId :: SqlMonad m => CommentHabrId -> m (Maybe CommentPKey)
findCommentIdByHabrId habrId = runPg $ runSelectReturningOne $ select query
  where query = fmap cId $ filter_ (\comm -> cSourceId comm ==. val_ habrId) $ all_ $ cComments cohabrDb

getPostComments :: SqlMonad m => PostPKey -> m [Comment]
getPostComments postId = runPg $ runSelectReturningList $ select query
  where query = filter_ (\comm -> cPostId comm ==. val_ postId) $ all_ $ cComments cohabrDb

getUserAvatar :: SqlMonad m => UserPKey -> m (Maybe UserAvatar)
getUserAvatar userId = runPg $ runSelectReturningOne $ select query
  where query = filter_ (\ua -> uaUser ua ==. val_ userId) $ all_ $ cUserAvatars cohabrDb
