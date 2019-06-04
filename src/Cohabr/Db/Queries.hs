{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}

module Cohabr.Db.Queries where

import Control.Arrow
import Opaleye as O

import qualified Cohabr.Db.Tables.Post as P

selectMissingPosts :: [Int] -> Select (Field SqlInt4)
selectMissingPosts candidates = proc () -> do
  P.Post { .. } <- selectTable P.postsTable -< ()
  restrict -< O.not $ in_ (pgInt4 <$> candidates) postId
  returnA -< postId
