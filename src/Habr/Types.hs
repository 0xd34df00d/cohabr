module Habr.Types where

import qualified Data.Text.Lazy as TL

data Votes = Votes
  { pos :: Int
  , neg :: Int
  } deriving (Eq, Ord, Show)

data UserInfo = UserInfo
  { username :: TL.Text
  , avatarLink :: TL.Text
  } deriving (Eq, Ord, Show)

data Comment = Comment
  { commentId :: Int
  , user :: UserInfo
  , votes :: Votes
  , commentText :: TL.Text
  , children :: [Comment]
  } deriving (Eq, Ord, Show)
