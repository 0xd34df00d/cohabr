{-# LANGUAGE RecordWildCards #-}

module Habr.Util
( buildCommentsTree
) where

import qualified Data.IntMap as IM
import Data.Maybe

import Habr.Types

buildCommentsTree :: [Comment] -> [Comment]
buildCommentsTree comments = go 0
  where
    cid2comment = IM.fromList [(commentId c, c) | c <- comments]
    idsTree = IM.fromListWith (<>) [(parentId, [commentId]) | Comment { .. } <- comments]
    go pid = [ (cid2comment IM.! thisId) { children = go thisId }
             | thisId <- fromMaybe [] $ IM.lookup pid idsTree
             ]
