{-# LANGUAGE FlexibleContexts #-}

module Cohabr.Db.Queries where

import Data.List
import Database.Beam
import Database.Beam.Postgres

import Cohabr.Db
import Cohabr.Db.HelperTypes

selectMissingPosts :: Connection -> [HabrId] -> IO [HabrId]
selectMissingPosts conn candidates = (candidates \\) <$> runBeamPostgresDebug putStrLn conn (runSelectReturningList $ select query)
  where query = filter_ (`in_` (val_ <$> candidates)) $ fmap pSourceId $ all_ $ cPosts cohabrDb

findPostByHabrId :: Connection -> HabrId -> IO (Maybe (Post, PostVersion))
findPostByHabrId conn habrId = runBeamPostgresDebug putStrLn conn $ runSelectReturningOne $ select query
  where
    query = do
      post <- filter_ (\post -> pSourceId post ==. val_ habrId) $ all_ $ cPosts cohabrDb
      postVersion <- filter_ (\postVersion -> pvId postVersion ==. pCurrentVersion post) $ all_ $ cPostsVersions cohabrDb
      pure (post, postVersion)

getPostVersionHubs :: Connection -> PKeyId -> IO [(PostHub, Hub)]
getPostVersionHubs conn postVersion = runBeamPostgresDebug putStrLn conn $ runSelectReturningList $ select query
  where
    query = do
      postHub <- filter_ (\ph -> phPostVersion ph ==. val_ postVersion) $ all_ $ cPostsHubs cohabrDb
      hub <- filter_ (\h -> hId h ==. phHub postHub) $ all_ $ cHubs cohabrDb
      pure (postHub, hub)

findCommentIdByHabrId :: Connection -> HabrId -> IO (Maybe PKeyId)
findCommentIdByHabrId conn habrId = runBeamPostgresDebug putStrLn conn $ runSelectReturningOne $ select query
  where query = fmap cId $ filter_ (\comm -> cSourceId comm ==. val_ habrId) $ all_ $ cComments cohabrDb
