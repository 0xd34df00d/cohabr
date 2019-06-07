{-# LANGUAGE PolyKinds, DataKinds #-}
{-# LANGUAGE DerivingVia #-}

module Cohabr.Db.Updates where

import qualified Data.Text as T
import Data.Monoid
import Opaleye.TypeFamilies

import qualified Cohabr.Db.Tables.Post as P
import qualified Cohabr.Db.Tables.PostVersion as PV
import Habr.Types

newtype UpdateField tableRec = UpdateField { getUpdate :: tableRec O -> tableRec O }
  deriving (Semigroup, Monoid) via Endo (tableRec O)

data RawPostVersion = RawPostVersion
  { rawPVTitle :: T.Text
  , rawPVText :: T.Text
  } deriving (Eq, Ord, Show)

data ListDiff a = ListDiff
  { added :: [a]
  , removed :: [a]
  } deriving (Eq, Ord, Show)

data PostUpdateActions = PostUpdateActions
  { postUpdates :: [UpdateField P.Post]
  , hubsDiff :: ListDiff Hub
  , newPostVersion :: Maybe RawPostVersion
  }
