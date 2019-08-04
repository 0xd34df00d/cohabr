{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -O0 #-}

module Cohabr.Db where

import Data.Char
import Data.Text(Text)
import Data.Time.LocalTime
import Database.Beam
import Database.Beam.Backend.Types

import Cohabr.Db.HelperTypes

data PostType = TyPost | TyArticle | TyNews deriving (Eq, Ord, Show, Enum, Bounded)

instance (BeamBackend be, FromBackendRow be String) => FromBackendRow be PostType where
  fromBackendRow = do
    val <- fromBackendRow
    case lookup val tys of
      Just ty -> pure ty
      _ -> fail $ "invalid value for PostType: " <> val
    where tys = [ (drop 2 $ toLower <$> show ty, ty) | ty <- [minBound .. maxBound] ]

data PostT f = Post
  { pId              :: Columnar f PostPKey
  , pSourceId        :: Columnar f PostHabrId
  , pUser            :: Columnar f (Maybe Text)
  , pPublished       :: Columnar f LocalTime
  , pLink            :: Columnar f (Maybe Text)
  , pLinkName        :: Columnar f (Maybe Text)
  , pScorePlus       :: Columnar f (Maybe Int)
  , pScoreMinus      :: Columnar f (Maybe Int)
  , pOrigViews       :: Columnar f (Maybe Int)
  , pOrigViewsNearly :: Columnar f (Maybe Bool)
  , pCurrentVersion  :: Columnar f PostVersionPKey
  , pAuthor          :: Columnar f (Maybe UserPKey)
  , pLastQueried     :: Columnar f LocalTime
  } deriving (Generic, Beamable)

type Post = PostT Identity
type PostPKey = PKeyId PostT
type PostHabrId = HabrId PostT

instance Table PostT where
  data PrimaryKey PostT f = PostId (Columnar f PostPKey) deriving (Generic, Beamable)
  primaryKey = PostId . pId


data PostVersionT f = PostVersion
  { pvId      :: Columnar f PostVersionPKey
  , pvPostId  :: Columnar f PostPKey
  , pvAdded   :: Columnar f LocalTime
  , pvTitle   :: Columnar f (Maybe Text)
  , pvContent :: Columnar f Text
  } deriving (Generic, Beamable)

type PostVersion = PostVersionT Identity
type PostVersionPKey = PKeyId PostVersionT

instance Table PostVersionT where
  data PrimaryKey PostVersionT f = PostVersionId (Columnar f PostVersionPKey) deriving (Generic, Beamable)
  primaryKey = PostVersionId . pvId


data HubT f = Hub
  { hId   :: Columnar f Text
  , hName :: Columnar f Text
  } deriving (Generic, Beamable)

type Hub = HubT Identity

instance Table HubT where
  data PrimaryKey HubT f = HubId (Columnar f Text) deriving (Generic, Beamable)
  primaryKey = HubId . hId


data PostHubT f = PostHub
  { phPostVersion :: Columnar f PostVersionPKey
  , phHub         :: Columnar f Text
  } deriving (Generic, Beamable)

type PostHub = PostHubT Identity

instance Table PostHubT where
  data PrimaryKey PostHubT f = PostHubPKey (Columnar f PostVersionPKey) (Columnar f Text) deriving (Generic, Beamable)
  primaryKey PostHub { .. } = PostHubPKey phPostVersion phHub


data FlagT f = Flag
  { fId      :: Columnar f Text
  , fText    :: Columnar f Text
  , fTooltip :: Columnar f Text
  } deriving (Generic, Beamable)

type Flag = FlagT Identity

instance Table FlagT where
  data PrimaryKey FlagT f = FlagId (Columnar f Text) deriving (Generic, Beamable)
  primaryKey = FlagId . fId


data PostFlagT f = PostFlag
  { pfPost :: Columnar f PostPKey
  , pfFlag :: Columnar f Text
  } deriving (Generic, Beamable)

type PostFlag = PostFlagT Identity

instance Table PostFlagT where
  data PrimaryKey PostFlagT f = PostFlagId (Columnar f PostPKey) (Columnar f Text) deriving (Generic, Beamable)
  primaryKey PostFlag { .. } = PostFlagId pfPost pfFlag

data PostTagT f = PostTag
  { ptId          :: Columnar f PostTagPKey
  , ptTag         :: Columnar f Text
  , ptPostVersion :: Columnar f PostVersionPKey
  } deriving (Generic, Beamable)

type PostTag = PostTagT Identity
type PostTagPKey = PKeyId PostTagT

instance Table PostTagT where
  data PrimaryKey PostTagT f = PostTagId (Columnar f PostTagPKey) deriving (Generic, Beamable)
  primaryKey = PostTagId . ptId


data CommentT f = Comment
  { cId         :: Columnar f CommentPKey
  , cSourceId   :: Columnar f CommentHabrId
  , cParent     :: Columnar f (Maybe CommentPKey)
  , cPostId     :: Columnar f PostPKey
  , cUser       :: Columnar f (Maybe Text)
  , cDate       :: Columnar f (Maybe LocalTime)
  , cText       :: Columnar f (Maybe Text)
  , cChanged    :: Columnar f Bool
  , cScorePlus  :: Columnar f (Maybe Int)
  , cScoreMinus :: Columnar f (Maybe Int)
  , cDeleted    :: Columnar f Bool
  , cAuthor     :: Columnar f (Maybe UserPKey)
  } deriving (Generic, Beamable)

type Comment = CommentT Identity
type CommentPKey = PKeyId CommentT
type CommentHabrId = HabrId CommentT

instance Table CommentT where
  data PrimaryKey CommentT f = CommentId (Columnar f CommentPKey) deriving (Generic, Beamable)
  primaryKey = CommentId . cId


data UserT f = User
  { uId                         :: Columnar f UserPKey
  , uUsername                   :: Columnar f Text
  , uSourceId                   :: Columnar f (Maybe UserHabrId)
  , uName                       :: Columnar f (Maybe Text)
  , uNameLastUpdated            :: Columnar f (Maybe LocalTime)
  , uSpecialization             :: Columnar f (Maybe Text)
  , uSpecializationLastUpdated  :: Columnar f (Maybe LocalTime)
  , uKarma                      :: Columnar f (Maybe Double)
  , uKarmaVotes                 :: Columnar f (Maybe Int)
  , uKarmaLastUpdated           :: Columnar f (Maybe LocalTime)
  , uRating                     :: Columnar f (Maybe Double)
  , uRatingLastUpdated          :: Columnar f (Maybe LocalTime)
  , uCurrentAvatar              :: Columnar f (Maybe AvatarPKey)
  , uCurrentAvatarLastCheck     :: Columnar f (Maybe LocalTime)
  , uDeleted                    :: Columnar f Bool
  } deriving (Generic, Beamable)

type User = UserT Identity
type UserPKey = PKeyId UserT
type UserHabrId = HabrId UserT

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f UserPKey) deriving (Generic, Beamable)
  primaryKey = UserId . uId


data UserAvatarT f = UserAvatar
  { uaId            :: Columnar f AvatarPKey
  , uaUser          :: Columnar f UserPKey
  , uaBigImageUrl   :: Columnar f Text
  , uaSmallImageUrl :: Columnar f Text
  , uaDiscoveredDate  :: Columnar f (Maybe LocalTime)
  } deriving (Generic, Beamable)

type UserAvatar = UserAvatarT Identity
type AvatarPKey = PKeyId UserAvatarT

instance Table UserAvatarT where
  data PrimaryKey UserAvatarT f = UserAvatarId (Columnar f AvatarPKey) deriving (Generic, Beamable)
  primaryKey = UserAvatarId . uaId


data CohabrDb f = CohabrDb
  { cPosts          :: f (TableEntity PostT)
  , cPostsVersions  :: f (TableEntity PostVersionT)
  , cHubs           :: f (TableEntity HubT)
  , cPostsHubs      :: f (TableEntity PostHubT)
  , cFlags          :: f (TableEntity FlagT)
  , cPostsFlags     :: f (TableEntity PostFlagT)
  , cPostsTags      :: f (TableEntity PostTagT)
  , cComments       :: f (TableEntity CommentT)
  , cUsers          :: f (TableEntity UserT)
  , cUserAvatars    :: f (TableEntity UserAvatarT)
  } deriving (Generic, Database be)

cohabrDb :: DatabaseSettings be CohabrDb
cohabrDb = defaultDbSettings
