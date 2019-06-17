{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Cohabr.Db.Conversions
( makeHubId
, fromStoredHub
, fromStoredTag
, flagToStr
, strToFlag
) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import qualified Habr.Types as HT
import Cohabr.Db

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
