module Habr.Types where

import qualified Data.Text as T

data Votes = Votes
  { pos :: Int
  , neg :: Int
  } deriving (Eq, Ord, Show)

data UserInfo = UserInfo
  { username :: T.Text
  , avatar :: T.Text
  } deriving (Eq, Ord, Show)

data Comment = Comment
  { commentId :: Int
  , parentId :: Int
  , user :: UserInfo
  , votes :: Votes
  , commentText :: T.Text
  , children :: [Comment]
  } deriving (Eq, Ord, Show)
