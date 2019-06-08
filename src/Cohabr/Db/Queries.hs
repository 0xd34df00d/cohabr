{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cohabr.Db.Queries
( selectMissingPosts
, findPostByHabrId
) where

import qualified Database.PostgreSQL.Simple as PGS
import Control.Arrow
import Data.List
import Opaleye as O
import Opaleye.TypeFamilies

import qualified Cohabr.Db.Tables.Post as P
import qualified Cohabr.Db.Tables.PostVersion as PV
import Cohabr.Db.HelperTypes

selectKnownPostsQ :: [HabrId] -> Select (Field SqlInt4)
selectKnownPostsQ candidates = proc () -> do
  P.Post { .. } <- selectTable P.postsTable -< ()
  restrict -< in_ (toFields <$> candidates) postId
  returnA -< postId

selectMissingPosts :: [HabrId] -> PGS.Connection -> IO [HabrId]
selectMissingPosts candidates conn = (candidates \\) <$> runSelect conn (selectKnownPostsQ candidates)

selectPostWVersionByHabrIdQ :: HabrId -> Select (P.Post O, PV.PostVersion O)
selectPostWVersionByHabrIdQ habrId = proc () -> do
  post <- selectTable P.postsTable -< ()
  restrict -< P.sourceId post .== toFields habrId
  postVersion <- selectTable PV.postsVersionsTable -< ()
  restrict -< PV.versionId postVersion .== P.currentVersion post
  returnA -< (post, postVersion)

findPostByHabrId :: HabrId -> PGS.Connection -> IO (Maybe (P.Post H, PV.PostVersion H))
findPostByHabrId habrId conn = handleList <$> runSelect conn (selectPostWVersionByHabrIdQ habrId)
  where
    handleList [] = Nothing
    handleList [pair] = Just pair
    handleList _ = error $ "Multiple posts for habr ID " <> show habrId
