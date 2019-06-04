{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}

module Cohabr.Db.Queries where

import Control.Arrow
import Opaleye as O

import qualified Cohabr.Db.Tables.Post as P

selectKnownPosts :: [Int] -> Select (Field SqlInt4)
selectKnownPosts candidates = proc () -> do
  P.Post { .. } <- selectTable P.postsTable -< ()
  restrict -< in_ (pgInt4 <$> candidates) postId
  returnA -< postId
