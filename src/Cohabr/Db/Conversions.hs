{-# LANGUAGE RecordWildCards, OverloadedStrings, LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Cohabr.Db.Conversions
( makeHubId
, fromStoredHub
, fromStoredTag
, flagToStr
, strToFlag

, PostDenorm
, getPostDenorm
, ppvToPost
) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import qualified Habr.Types as HT
import Cohabr.Db
import Cohabr.Db.Queries
import Cohabr.Db.SqlMonad

makeHubId :: HT.Hub -> T.Text
makeHubId HT.Hub { .. } = prefix hubKind <> hubCode
  where
    prefix HT.NormalHub = mempty
    prefix HT.CompanyHub = "company-"

fromStoredHub :: (PostHub, Hub) -> HT.Hub
fromStoredHub (PostHub { .. }, Hub { .. }) = HT.Hub code hName kind
  where
    (code, kind) | not $ cmpPref `T.isPrefixOf` hId = (hId, HT.NormalHub)
                 | otherwise = (T.drop (T.length cmpPref) hId, HT.CompanyHub)
    cmpPref = "company-"

fromStoredTag :: PostTag -> HT.Tag
fromStoredTag = HT.Tag . ptTag

flagToStr :: HT.Flag -> T.Text
flagToStr flag = case flag of
                      HT.RssFeed      -> "rss_feed"
                      HT.Draftbox     -> "draftbox"
                      HT.News         -> "news"
                      HT.Recovery     -> "recovery"
                      HT.Tutorial     -> "tutorial"
                      HT.Translation  -> "translation"
                      HT.Sandbox      -> "sandbox"

strToFlag :: T.Text -> Maybe HT.Flag
strToFlag str = HM.lookup str strToFlagMap

strToFlagMap :: HM.HashMap T.Text HT.Flag
strToFlagMap = HM.fromList [ (flagToStr flag, flag)
                           | flag <- [minBound .. maxBound]
                           ]

data PostDenorm = PostDenorm
  { dbPost :: Post
  , dbPostVersion :: PostVersion
  , dbPostAuthor :: Maybe ShortUserInfo
  }

getPostDenorm :: SqlMonad r m => PostHabrId -> m (Maybe PostDenorm)
getPostDenorm habrId = findPostByHabrId habrId >>= \case
  Nothing -> pure Nothing
  Just (dbPost, dbPostVersion) -> do
    dbPostAuthor <- case pAuthor dbPost of
                         Nothing -> pure Nothing
                         Just authorId -> getShortUserInfo authorId
    pure $ Just PostDenorm { .. }

ppvToPost :: PostDenorm -> HT.PostFromDb
ppvToPost PostDenorm { .. } = HT.Post
  { title = pvTitle
  , body = pvContent
  , hubs = [] -- TODO
  , tags = [] -- TODO
  , flags = [] -- TODO
  , link = HT.Link <$> (HT.URL <$> pLink) <*> pLinkName
  , user = shortUser2info <$> dbPostAuthor
  , timestamp = pPublished
  , postStats = HT.PostStats <$> votes' <*> views'
  , postType = pType
  }
  where
    Post { .. } = dbPost
    PostVersion { .. } = dbPostVersion
    shortUser2info ShortUserInfo { .. } = HT.UserInfo
      { username = username
      , avatar = maybe HT.DefaultAvatar (HT.CustomAvatar . HT.URL) avatarBig
      }
    votes' = HT.Votes <$> pScorePlus <*> pScoreMinus
    views' = HT.PostViews <$> pOrigViewsNearly <*> pOrigViews
