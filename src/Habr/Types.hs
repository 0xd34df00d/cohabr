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

data CommentContents
  = CommentDeleted
  | CommentExisting
    { user :: UserInfo
    , votes :: Votes
    , commentText :: T.Text
    , timestamp :: UTCTime
    }
  deriving (Eq, Ord, Show)

data Comment = Comment
  { commentId :: Int
  , parentId :: Int
  , contents :: CommentContents
  , children :: [Comment]
  } deriving (Eq, Ord, Show)

data Classifier = Classifier
  { name :: T.Text
  , link :: T.Text
  } deriving (Eq, Ord, Show)

data PostViews = ExactViews Int | ApproxViews Int
  deriving (Eq, Ord, Show)

data PostStats = PostStats
  { votes :: Votes
  , bookmarks :: Int
  , views :: PostViews
  } deriving (Eq, Ord, Show)

data Tag = Tag
  { name :: T.Text
  , link :: T.Text
  } deriving (Eq, Ord, Show)

data HubKind = NormalHub | CompanyHub deriving (Eq, Ord, Show)

data Hub = Hub
  { hubCode :: T.Text
  , hubName :: T.Text
  , hubKind :: HubKind
  } deriving (Eq, Ord, Show)

data Post = Post
  { title :: T.Text
  , body :: T.Text
  , hubs :: [Hub]
  , tags :: [Tag]
  , user :: UserInfo
  , timestamp :: UTCTime
  , postStats :: PostStats
  } deriving (Eq, Ord, Show)
