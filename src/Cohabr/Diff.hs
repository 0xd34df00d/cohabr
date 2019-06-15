{-# LANGUAGE RecordWildCards, ViewPatterns #-}
{-# LANGUAGE FlexibleContexts, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cohabr.Diff where

import qualified Data.HashSet as S
import Data.Hashable
import Data.Maybe
import Database.Beam hiding(timestamp)
import Database.Beam.Backend.SQL
import Database.Beam.Postgres.Syntax

import Cohabr.Db as Db
import Cohabr.Db.HelperTypes
import Cohabr.Db.Updates
import Habr.Types as HT

data StoredPostInfo = StoredPostInfo
  { storedPost :: Db.Post
  , storedCurrentVersion :: Db.PostVersion
  , storedPostHubs :: [HT.Hub]
  , storedPostTags :: [HT.Tag]
  }

postUpdateActions :: PKeyId -> StoredPostInfo -> HT.Post -> PostUpdateActions
postUpdateActions postId StoredPostInfo { .. } HT.Post { .. } = PostUpdateActions { .. }
  where
    hubsDiff = calcDiff storedPostHubs hubs
    tagsDiff = calcDiff storedPostTags tags

    PostStats { votes = Votes { .. }, .. } = postStats

    postUpdates = catMaybes [upScorePlus, upScoreMinus, upOrigViews, upOrigViewsNearly]
    upScorePlus = produceUpdateField storedPost pScorePlus pos
    upScoreMinus = produceUpdateField storedPost pScoreMinus neg
    upOrigViews = produceUpdateField storedPost pOrigViews $ viewsCount views
    upOrigViewsNearly = produceUpdateField storedPost pOrigViewsNearly $ not $ isExactCount views

    newPostVersion | Just title == pvTitle storedCurrentVersion &&
                     body == pvContent storedCurrentVersion = Nothing
                   | otherwise = Just RawPostVersion { rawPVTitle = title, rawPVText = body }

produceUpdateField :: (Eq a, HasSqlValueSyntax PgValueSyntax a)
                   => table Identity
                   -> (forall f. table f -> Columnar f (Maybe a))
                   -> a
                   -> Maybe (UpdateField table)
produceUpdateField stored acc parsed | Just val <- acc stored
                                     , val == parsed = Nothing
                                     | otherwise = Just $ UpdateField acc (Just parsed)

calcDiff :: (Eq a, Hashable a) => [a] -> [a] -> ListDiff a
calcDiff (S.fromList -> stored) allNew@(S.fromList -> parsed) = ListDiff { .. }
  where
    added = S.toList $ parsed `S.difference` stored
    removed = S.toList $ stored `S.difference` parsed
