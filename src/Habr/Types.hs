{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveAnyClass, DerivingStrategies, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# LANGUAGE Strict #-}

module Habr.Types where

import qualified Data.Text as T
import Control.DeepSeq
import Data.Data
import Data.Hashable
import Data.String
import Data.Time.LocalTime
import Data.Tree
import GHC.Generics

newtype URL = URL { getUrl :: T.Text }
  deriving (Eq, Ord, Show, Data, Generic)
  deriving newtype (IsString, Semigroup, Monoid)
  deriving anyclass (NFData)

data Votes = Votes
  { pos :: Int
  , neg :: Int
  } deriving (Eq, Ord, Show, Data, Generic, NFData)

data Avatar
  = DefaultAvatar
  | CustomAvatar { avatarLink :: URL }
  deriving (Eq, Ord, Show, Data, Generic, NFData)

data UserInfo = UserInfo
  { username :: T.Text
  , avatar :: Avatar
  } deriving (Eq, Ord, Show, Data, Generic, NFData)

data CommentContents
  = CommentDeleted
  | CommentExisting
    { user :: UserInfo
    , votes :: Votes
    , commentText :: T.Text
    , commentChanged :: Bool
    , timestamp :: LocalTime
    }
  deriving (Eq, Ord, Show, Data, Generic, NFData)

data Comment = Comment
  { commentId :: Int
  , parentId :: Int
  , contents :: CommentContents
  } deriving (Eq, Ord, Show, Data, Generic, NFData)

type Comments = Forest Comment

data PostViews = PostViews
  { isExactCount :: Bool
  , viewsCount :: Int
  } deriving (Eq, Ord, Show, Data, Generic, NFData)

data PostStats = PostStats
  { votes :: Votes
  , bookmarks :: Int
  , views :: PostViews
  } deriving (Eq, Ord, Show, Data, Generic, NFData)

data Flag = RssFeed | Draftbox | News | Recovery | Tutorial | Translation | Sandbox
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, Hashable, Data, NFData)

newtype Tag = Tag { name :: T.Text }
  deriving (Eq, Ord, Show, Generic, Data)
  deriving anyclass (Hashable, NFData)

data HubKind = NormalHub | CompanyHub deriving (Eq, Ord, Show, Generic, Hashable, Data, NFData)

data Hub = Hub
  { hubCode :: T.Text
  , hubName :: T.Text
  , hubKind :: HubKind
  } deriving (Eq, Ord, Show, Generic, Hashable, Data, NFData)

data Link = Link
  { linkUrl :: URL
  , linkName :: T.Text
  } deriving (Eq, Ord, Show, Data, Generic, NFData)

data PostType = TyPost | TyArticle | TyNews deriving (Eq, Ord, Show, Enum, Bounded, Data, Generic, NFData)

data Source = Parsed | FromDb

type family MapSource s ty where
  MapSource 'Parsed ty = ty
  MapSource 'FromDb ty = Maybe ty

data PostT src = Post
  { title :: MapSource src T.Text
  , body :: T.Text
  , hubs :: [Hub]
  , tags :: [Tag]
  , flags :: [Flag]
  , link :: Maybe Link
  , user :: UserInfo
  , timestamp :: LocalTime
  , postStats :: PostStats
  , postType :: PostType
  } deriving (Generic)

type Post = PostT 'Parsed
type PostFromDb = PostT 'FromDb

deriving instance Eq Post
deriving instance Data Post
deriving instance NFData Post

deriving instance Eq PostFromDb
deriving instance Data PostFromDb
deriving instance NFData PostFromDb
