{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveAnyClass #-}

module Habr.Types where

import qualified Data.Text as T
import Data.Data
import Data.Hashable
import Data.Time.LocalTime
import Data.Tree
import GHC.Generics

data Votes = Votes
  { pos :: Int
  , neg :: Int
  } deriving (Eq, Ord, Show, Data)

data Avatar
  = DefaultAvatar
  | CustomAvatar { avatarLink :: T.Text }
  deriving (Eq, Ord, Show, Data)

data UserInfo = UserInfo
  { username :: T.Text
  , avatar :: Avatar
  } deriving (Eq, Ord, Show, Data)

data CommentContents
  = CommentDeleted
  | CommentExisting
    { user :: UserInfo
    , votes :: Votes
    , commentText :: T.Text
    , commentChanged :: Bool
    , timestamp :: LocalTime
    }
  deriving (Eq, Ord, Show, Data)

data Comment = Comment
  { commentId :: Int
  , parentId :: Int
  , contents :: CommentContents
  } deriving (Eq, Ord, Show, Data)

type Comments = Forest Comment

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
