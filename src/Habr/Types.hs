{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveAnyClass, DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict #-}

module Habr.Types where

import qualified Data.Text as T
import Data.Data
import Data.Hashable
import Data.String
import Data.Time.LocalTime
import Data.Tree
import GHC.Generics

newtype URL = URL { getUrl :: T.Text }
  deriving (Eq, Ord, Show, Data)
  deriving newtype (IsString, Semigroup, Monoid)

data Votes = Votes
  { pos :: Int
  , neg :: Int
  } deriving (Eq, Ord, Show, Data)

data Avatar
  = DefaultAvatar
  | CustomAvatar { avatarLink :: URL }
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
  } deriving (Eq, Ord, Show, Data)

data PostStats = PostStats
  { votes :: Votes
  , bookmarks :: Int
  , views :: PostViews
  } deriving (Eq, Ord, Show, Data)

data Flag = RssFeed | Draftbox | News | Recovery | Tutorial | Translation | Sandbox
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, Hashable, Data)

newtype Tag = Tag { name :: T.Text }
  deriving (Eq, Ord, Show, Generic, Data)
  deriving anyclass Hashable

data HubKind = NormalHub | CompanyHub deriving (Eq, Ord, Show, Generic, Hashable, Data)

data Hub = Hub
  { hubCode :: T.Text
  , hubName :: T.Text
  , hubKind :: HubKind
  } deriving (Eq, Ord, Show, Generic, Hashable, Data)

data Link = Link
  { linkUrl :: URL
  , linkName :: T.Text
  } deriving (Eq, Ord, Show, Data)

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
  } deriving (Eq, Ord, Show, Data)
