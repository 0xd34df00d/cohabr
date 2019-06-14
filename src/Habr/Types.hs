{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Habr.Types where

import qualified Data.Text as T
import Data.Hashable
import Data.Time.LocalTime
import GHC.Generics

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
    , timestamp :: LocalTime
    }
  deriving (Eq, Ord, Show)

data Comment = Comment
  { commentId :: Int
  , parentId :: Int
  , contents :: CommentContents
  , children :: [Comment]
  } deriving (Eq, Ord, Show)

data PostViews = PostViews
  { isExactCount :: Bool
  , viewsCount :: Int
  } deriving (Eq, Ord, Show)

data PostStats = PostStats
  { votes :: Votes
  , bookmarks :: Int
  , views :: PostViews
  } deriving (Eq, Ord, Show)

data Flag = RssFeed | Draftbox | News | Recovery | Tutorial | Translation | Sandbox
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, Hashable)

newtype Tag = Tag
  { name :: T.Text
  } deriving (Eq, Ord, Show, Generic, Hashable)

data HubKind = NormalHub | CompanyHub deriving (Eq, Ord, Show, Generic, Hashable)

data Hub = Hub
  { hubCode :: T.Text
  , hubName :: T.Text
  , hubKind :: HubKind
  } deriving (Eq, Ord, Show, Generic, Hashable)

data Link = Link
  { linkUrl :: T.Text
  , linkName :: T.Text
  } deriving (Eq, Ord, Show)

data Post = Post
  { title :: T.Text
  , body :: T.Text
  , hubs :: [Hub]
  , tags :: [Tag]
  , flags :: [Flag]
  , link :: Maybe Link
  , user :: UserInfo
  , timestamp :: LocalTime
  , postStats :: PostStats
  } deriving (Eq, Ord, Show)
