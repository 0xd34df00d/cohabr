{-# LANGUAGE RecordWildCards #-}

module Habr.RSS
( recentArticles
) where

import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe
import Text.Feed.Import
import Text.Feed.Types
import Text.RSS.Syntax

import Habr.Internal.Util

recentArticles :: BSL.ByteString -> Maybe [Int]
recentArticles str = extractArticles <$> parseFeedSource str

extractArticles :: Feed -> [Int]
extractArticles (RSSFeed RSS { .. }) = extractRssChannel rssChannel
extractArticles (AtomFeed _) = error "Atom feeds are unsupported"
extractArticles (RSS1Feed _) = error "RSS1 feeds are unsupported"
extractArticles (XMLFeed _) = []

extractRssChannel :: RSSChannel -> [Int]
extractRssChannel RSSChannel { .. } = mapMaybe handleItem rssItems
  where
    handleItem item = do
      guid <- rssItemGuid item
      -- TODO more type safety
      let (_:postId:_) = reverseLinkParts $ rssGuidValue guid
      let Right val = readInt postId
      pure val
