{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module Cohabr.Db.ListDiff
( ListDiff(..)
, calcDiff
) where

import qualified Data.HashSet as S
import Data.Hashable

data ListDiff a = ListDiff
  { added :: [a]
  , removed :: [a]
  , allNew :: [a]
  } deriving (Eq, Ord, Show)

calcDiff :: (Eq a, Hashable a) => [a] -> [a] -> ListDiff a
calcDiff (S.fromList -> stored) allNew@(S.fromList -> parsed) = ListDiff { .. }
  where
    added = S.toList $ parsed `S.difference` stored
    removed = S.toList $ stored `S.difference` parsed
