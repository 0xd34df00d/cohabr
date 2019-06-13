{-# LANGUAGE FlexibleContexts #-}

module Cohabr.Db.Queries where

import Data.List
import Database.Beam
import Database.Beam.Postgres

import Cohabr.Db
import Cohabr.Db.HelperTypes

selectMissingPosts :: [HabrId] -> Connection -> IO [HabrId]
selectMissingPosts candidates conn = (candidates \\) <$> runBeamPostgres conn (runSelectReturningList $ select query)
  where query = filter_ (`in_` (val_ <$> candidates)) $ fmap pSourceId $ all_ $ cPosts cohabrDb

findPostByHabrId :: HabrId -> Connection -> IO (Maybe (Post, PostVersion))
findPostByHabrId habrId conn = runBeamPostgres conn $ runSelectReturningOne $ select query
  where
    query = do
      post <- filter_ (\post -> pSourceId post ==. val_ habrId) $ all_ $ cPosts cohabrDb
      postVersion <- filter_ (\postVersion -> pvId postVersion ==. pCurrentVersion post) $ all_ $ cPostsVersions cohabrDb
      pure (post, postVersion)

findCommentIdByHabrId :: HabrId -> Connection -> IO (Maybe PKeyId)
findCommentIdByHabrId habrId conn = runBeamPostgres conn $ runSelectReturningOne $ select query
  where query = fmap cId $ filter_ (\comm -> cSourceId comm ==. val_ habrId) $ all_ $ cComments cohabrDb
