{-# LANGUAGE RecordWildCards, TransformListComp, QuasiQuotes #-}

module Habr.Util
( buildCommentsTree
, urlForPostId
) where

import qualified Data.IntMap as IM
import Data.List
import Data.Maybe
import Data.Tree
import Data.String
import Data.String.Interpolate.IsString

import Habr.Types

buildCommentsTree :: [Comment] -> Comments
buildCommentsTree comments = go 0
  where
    cid2comment = IM.fromList [(commentId c, c) | c <- comments]
    idsTree = IM.fromListWith (<>) [(parentId, [commentId]) | Comment { .. } <- comments]
    go pid = [ Node { rootLabel = cid2comment IM.! thisId, subForest = go thisId }
             | thisId <- fromMaybe [] $ IM.lookup pid idsTree
             , then sortOn by thisId
             ]

urlForPostId :: IsString a => Int -> a
urlForPostId pgId = [i|https://habr.com/ru/post/#{pgId}/|]
