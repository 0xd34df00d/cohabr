{-# LANGUAGE DeriveGeneric, DeriveAnyClass, TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Cohabr.Db where

import Data.Text(Text)
import Data.Time.LocalTime
import Database.Beam

import Cohabr.Db.HelperTypes

data PostT f = Post
  { pId              :: Columnar f PKeyId
  , pSourceId        :: Columnar f HabrId
  , pUser            :: Columnar f (Maybe Text)
  , pPublished       :: Columnar f LocalTime
  , pLink            :: Columnar f (Maybe Text)
  , pLinkName        :: Columnar f (Maybe Text)
  , pScorePlus       :: Columnar f (Maybe Int)
  , pScoreMinus      :: Columnar f (Maybe Int)
  , pOrigViews       :: Columnar f (Maybe Int)
  , pOrigViewsNearly :: Columnar f (Maybe Bool)
  , pCurrentVersion  :: Columnar f PKeyId
  , pAuthor          :: Columnar f (Maybe PKeyId)
  } deriving (Generic, Beamable)

type Post = PostT Identity

instance Table PostT where
  data PrimaryKey PostT f = PostId (Columnar f PKeyId) deriving (Generic, Beamable) 
  primaryKey = PostId . pId


data PostVersionT f = PostVersion
  { pvId      :: Columnar f PKeyId
  , pvPostId  :: Columnar f PKeyId
  , pvAdded   :: Columnar f LocalTime
  , pvTitle   :: Columnar f (Maybe Text)
  , pvContent :: Columnar f Text
  } deriving (Generic, Beamable)

type PostVersion = PostVersionT Identity

instance Table PostVersionT where
  data PrimaryKey PostVersionT f = PostVersionId (Columnar f PKeyId) deriving (Generic, Beamable)
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
  { phPostVersion :: Columnar f PKeyId
  , phHub         :: Columnar f Text
  } deriving (Generic, Beamable)

type PostHub = PostHubT Identity

instance Table PostHubT where
  data PrimaryKey PostHubT f = PostHubPKey (Columnar f PKeyId) (Columnar f Text) deriving (Generic, Beamable)
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
  { pfPost :: Columnar f PKeyId
  , pfFlag :: Columnar f Text
  } deriving (Generic, Beamable)

type PostFlag = PostFlagT Identity

instance Table PostFlagT where
  data PrimaryKey PostFlagT f = PostFlagId (Columnar f PKeyId) (Columnar f Text) deriving (Generic, Beamable)
  primaryKey PostFlag { .. } = PostFlagId pfPost pfFlag


data CommentT f = Comment
  { cId         :: Columnar f PKeyId
  , cSourceId   :: Columnar f HabrId
  , cParent     :: Columnar f HabrId
  , cPostId     :: Columnar f PKeyId
  , cUser       :: Columnar f String
  , cDate       :: Columnar f LocalTime
  , cText       :: Columnar f Text
  , cChanged    :: Columnar f Bool
  , cScorePlus  :: Columnar f Double
  , cScoreMinus :: Columnar f Double
  , cDeleted    :: Columnar f Bool
  , cAuthor     :: Columnar f PKeyId
  } deriving (Generic, Beamable)

type Comment = CommentT Identity

instance Table CommentT where
  data PrimaryKey CommentT f = CommentId (Columnar f PKeyId) deriving (Generic, Beamable)
  primaryKey = CommentId . cId


data UserT f = User
  { uId                         :: Columnar f PKeyId
  , uUsername                   :: Columnar f Text
  , uSourceId                   :: Columnar f (Maybe HabrId)
  , uName                       :: Columnar f (Maybe Text)
  , uNameLastUpdated            :: Columnar f (Maybe LocalTime)
  , uSpecialization             :: Columnar f (Maybe Text)
  , uSpecializationLastUpdated  :: Columnar f (Maybe LocalTime)
  , uKarma                      :: Columnar f (Maybe Double)
  , uKarmaVotes                 :: Columnar f (Maybe Int)
  , uKarmaLastUpdated           :: Columnar f (Maybe LocalTime)
  , uRating                     :: Columnar f (Maybe Double)
  , uRatingLastUpdated          :: Columnar f (Maybe LocalTime)
  , uCurrentAvatar              :: Columnar f (Maybe Int)
  , uCurrentAvatarLastCheck     :: Columnar f (Maybe LocalTime)
  , uDeleted                    :: Columnar f Bool
  } deriving (Generic, Beamable)

type User = UserT Identity

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f PKeyId) deriving (Generic, Beamable)
  primaryKey = UserId . uId


data CohabrDb f = CohabrDb
  { cPosts          :: f (TableEntity PostT)
  , cPostsVersions  :: f (TableEntity PostVersionT)
  , cHubs           :: f (TableEntity HubT)
  , cPostsHubs      :: f (TableEntity PostHubT)
  , cFlags          :: f (TableEntity FlagT)
  , cPostsFlags     :: f (TableEntity PostFlagT)
  , cComments       :: f (TableEntity CommentT)
  , cUsers          :: f (TableEntity UserT)
  } deriving (Generic, Database be)

cohabrDb :: DatabaseSettings be CohabrDb
cohabrDb = defaultDbSettings
