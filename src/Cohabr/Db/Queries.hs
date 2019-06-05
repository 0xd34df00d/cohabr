{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}

module Cohabr.Db.Queries
( selectMissingPosts
) where

import qualified Database.PostgreSQL.Simple as PGS
import Control.Arrow
import Data.List
import Opaleye as O

import qualified Cohabr.Db.Tables.Post as P

selectKnownPostsQ :: [Int] -> Select (Field SqlInt4)
selectKnownPostsQ candidates = proc () -> do
  P.Post { .. } <- selectTable P.postsTable -< ()
  restrict -< in_ (pgInt4 <$> candidates) postId
  returnA -< postId

selectMissingPosts :: [Int] -> PGS.Connection -> IO [Int]
selectMissingPosts candidates conn = (candidates \\) <$> runSelect conn (selectKnownPostsQ candidates)
