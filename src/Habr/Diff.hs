{-# LANGUAGE RecordWildCards, ViewPatterns, DuplicateRecordFields, DerivingVia #-}
{-# LANGUAGE PolyKinds, FlexibleContexts, DataKinds, RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes, TypeApplications #-}

module Habr.Diff where

import qualified Data.HashSet as S
import Data.Hashable
import Data.Maybe
import Data.Monoid
import Data.Profunctor.Product.Default
import Lens.Micro
import Opaleye hiding(not)
import Opaleye.TypeFamilies

import qualified Cohabr.Db.Tables.Post as P
import qualified Cohabr.Db.Tables.PostVersion as PV
import Habr.Types

data ListDiff a = ListDiff
  { added :: [a]
  , removed :: [a]
  } deriving (Eq, Ord, Show)

data StoredPostInfo = StoredPostInfo
  { storedPost :: P.Post H
  , storedCurrentVersion :: PV.PostVersion H
  , storedPostHubs :: [Hub]
  }

newtype UpdateField tableRec = UpdateField { getUpdate :: tableRec O -> tableRec O }
  deriving (Semigroup, Monoid) via Endo (tableRec O)

data PostUpdateActions = PostUpdateActions
  { postUpdates :: [UpdateField P.Post]
  , hubsDiff :: ListDiff Hub
  , newPostVersion :: Maybe (PV.PostVersion W)
  }

postUpdateActions :: StoredPostInfo -> Post -> PostUpdateActions
postUpdateActions StoredPostInfo { .. } Post { .. } = PostUpdateActions { .. }
  where
    PostStats { votes = Votes { .. }, .. } = postStats
    hubsDiff = calcDiff storedPostHubs hubs
    postUpdates = catMaybes [upScorePlus, upScoreMinus, upOrigViews, upOrigViewsNearly]
    upScorePlus = produceUpdateField @SqlInt4 P.accScorePlus pos
    upScoreMinus = produceUpdateField @SqlInt4 P.accScoreMinus neg
    upOrigViews = produceUpdateField @SqlInt4 P.accOrigViews $ viewsCount views
    upOrigViewsNearly = produceUpdateField @SqlBool P.accOrigViewsNearly $ not $ isExactCount views

    produceUpdateField :: forall ty a. (Eq a, Default Constant a (Column ty)) =>
                          (forall f. Lens' (P.Post f) (TableField f a ty N Req)) ->
                          a -> Maybe (UpdateField P.Post)
    produceUpdateField acc parsed | Just val <- storedPost^.acc
                                  , val == parsed = Nothing
                                  | otherwise = Just $ UpdateField (& acc .~ toFields (Just parsed))

calcDiff :: (Eq a, Hashable a) => [a] -> [a] -> ListDiff a
calcDiff (S.fromList -> stored) (S.fromList -> parsed) = ListDiff { .. }
  where
    added = S.toList $ parsed `S.difference` stored
    removed = S.toList $ stored `S.difference` parsed
