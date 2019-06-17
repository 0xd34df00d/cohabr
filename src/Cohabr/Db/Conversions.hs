{-# LANGUAGE RecordWildCards, OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Cohabr.Db.Conversions
( makeHubId
, fromStoredHub
, fromStoredTag
, flagToStr
, strToFlag
, fromStoredComments
) where

import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import Control.Monad.Except
import Data.Maybe
import Data.String.Interpolate

import qualified Habr.Types as HT
import Habr.Util
import Cohabr.Db
import Cohabr.Db.HelperTypes

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

-- This function does not have full info about users,
-- so it will not populate UserInfo besides setting the 'username' field.
fromStoredComments :: forall m. MonadError String m => HT.CommentContents -> [Comment] -> m [HT.Comment]
fromStoredComments defaultContents comments = buildCommentsTree <$> mapM convSingle comments
  where
    convSingle :: Comment -> m HT.Comment
    convSingle Comment { .. } = do
      parentId <- getParentId cParent
      let commentId = getHabrId cSourceId
      let contents
            | Just text <- cText = HT.CommentExisting
                { HT.commentText = text
                , HT.user = fromMaybe user $ HT.UserInfo <$> cUser <*> pure HT.DefaultAvatar
                , HT.votes = fromMaybe votes $ HT.Votes <$> cScorePlus <*> cScoreMinus
                , HT.timestamp = fromMaybe timestamp cDate
                }
            | otherwise = HT.CommentDeleted
      pure HT.Comment { children = [], .. }

    getParentId Nothing = pure 0
    getParentId (Just parentPKey) | Just val <- IM.lookup (getPKeyId parentPKey) pkey2habrId = pure $ getHabrId val
                                  | otherwise = throwError [i|Unable to find parent comment with id #{parentPKey}|]
    pkey2habrId = IM.fromList [ (getPKeyId cId, cSourceId)
                              | Comment { .. } <- comments
                              ]
    HT.CommentExisting { .. } = defaultContents
