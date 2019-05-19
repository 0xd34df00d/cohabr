{-# LANGUAGE RecordWildCards, OverloadedStrings, ApplicativeDo, QuasiQuotes #-}

module Habr.Parser where

import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.String.Interpolate
import Text.XML
import Text.XML.Cursor
import Text.XML.Scraping
import Text.XML.Selector.TH
import Text.XML.Selector.Types

import Habr.Types

parseComments :: Cursor -> Either String [Comment]
parseComments = undefined

(@@) :: Cursor -> Name -> Either String T.Text
cur @@ name | [val] <- attrs = pure val
            | otherwise = Left [i|unexpected attribute contents for #{name}: #{attrs}|]
  where attrs = attribute name cur

(@@^) :: Either String Cursor -> Name -> Either String T.Text
mcur @@^ name = mcur >>= (@@ name)

(@>) :: Cursor -> [JQSelector] -> Either String Cursor
cur @> expr | (sub:_) <- queryT expr cur = pure sub
            | otherwise = Left [i|nothing found for expression #{expr}|]

readInt :: T.Text -> Either String Int
readInt text = do
  (val, rest) <- T.decimal text
  if T.null rest
    then Left [i|unable to parse `#{text}` as int|]
    else pure val

parseSingleComment :: Cursor -> Either String Comment
parseSingleComment cur = do
  parentId <- cur @> [jq| span.parent_id |] @@^ "data-parent_id" >>= readInt
  commentId <- cur @@ "rel" >>= readInt
  commentText <- innerHtml <$> (cur @> [jq| div.comment__message |])
  let children = []
  pure Comment { .. }
