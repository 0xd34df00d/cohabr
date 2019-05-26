{-# LANGUAGE DuplicateRecordFields #-}

module Habr.Types where

import qualified Data.Text as T
import Data.Time.Clock

data Votes = Votes
  { pos :: Int
  , neg :: Int
  } deriving (Eq, Ord, Show)

data Avatar
  = DefaultAvatar { svgElem :: T.Text }
  | CustomAvatar { avatarLink :: T.Text }
  deriving (Eq, Ord, Show)

data UserInfo = UserInfo
  { username :: T.Text
  , avatar :: Avatar
  } deriving (Eq, Ord, Show)

data Comment = Comment
  { commentId :: Int
  , parentId :: Int
  , user :: UserInfo
  , votes :: Votes
  , commentText :: T.Text
  , timestamp :: UTCTime
  , children :: [Comment]
  } deriving (Eq, Ord, Show)

data Classifier = Classifier
  { name :: T.Text
  , link :: T.Text
  } deriving (Eq, Ord, Show)

data PostStats = PostStats
  { votes :: Votes
  , bookmarks :: Int
  , views :: Int
  } deriving (Eq, Ord, Show)

data Post = Post
  { title :: T.Text
  , body :: T.Text
  , hubs :: [Classifier]
  , tags :: [Classifier]
  , user :: UserInfo
  , timestamp :: UTCTime
  , postStats :: PostStats
  } deriving (Eq, Ord, Show)
