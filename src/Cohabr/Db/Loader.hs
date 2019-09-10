{-# LANGUAGE RecordWildCards, LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Cohabr.Db.Loader
( PostDenorm
, getPostDenorm
, ppvToPost
) where

import qualified Habr.Types as HT
import Cohabr.Db
import Cohabr.Db.Queries
import Cohabr.Db.SqlMonad

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
