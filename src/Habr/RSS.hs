{-# LANGUAGE RecordWildCards #-}

module Habr.RSS
( recentArticles
, lastCommentDate
) where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import Control.Monad
import Data.Maybe
import Data.Time.Format
import Data.Time.LocalTime
import Text.Feed.Import
import Text.Feed.Types
import Text.RSS.Syntax
import Safe.Foldable

import Habr.Internal.Util

recentArticles :: BSL.ByteString -> Maybe [Int]
recentArticles str = extract <$> parseFeedSource str
  where
    extract (RSSFeed RSS { .. }) = extractRssChannel rssChannel
    extract _ = error "Unsupported feed format"

extractRssChannel :: RSSChannel -> [Int]
extractRssChannel RSSChannel { .. } = mapMaybe handleItem rssItems
  where
    handleItem item = do
      guid <- rssItemGuid item
      -- TODO more type safety
      let (_:postId:_) = reverseLinkParts $ rssGuidValue guid
      let Right val = readInt postId
      pure val

lastCommentDate :: BSL.ByteString -> Maybe LocalTime
lastCommentDate str = parseFeedSource str >>= extract
  where
    extract (RSSFeed RSS { .. }) = extractDate rssChannel
    extract _ = error "Unsupported feed format"

extractDate :: RSSChannel -> Maybe LocalTime
extractDate RSSChannel { .. } = maximumMay =<<
  mapM (rssItemPubDate >=> parseTimeM True defaultTimeLocale rfc822DateFormat . T.unpack) rssItems
