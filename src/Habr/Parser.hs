{-# LANGUAGE RecordWildCards, OverloadedStrings, ApplicativeDo, QuasiQuotes #-}

module Habr.Parser where

import qualified Data.IntMap as IM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Read as T
import Control.Monad
import Data.Either.Combinators
import Data.Maybe
import Data.String.Interpolate
import Text.XML
import Text.XML.Cursor
import Text.XML.Scraping
import Text.XML.Selector.TH
import Text.XML.Selector.Types

import Habr.Types

parseComments :: Cursor -> Either String [Comment]
parseComments root = buildTree <$> mapM parseSingleComment (queryT [jq| .js-comment |] root)

buildTree :: [Comment] -> [Comment]
buildTree comments = go 0
  where
    cid2comment = IM.fromList [(commentId c, c) | c <- comments]
    idsTree = IM.fromListWith (<>) [(parentId, [commentId]) | Comment { .. } <- comments]
    go pid = [ (cid2comment IM.! thisId) { children = go thisId }
             | thisId <- fromMaybe [] $ IM.lookup pid idsTree
             ]

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
    then pure val
    else Left [i|unable to parse `#{text}` as int|]

parseSingleComment :: Cursor -> Either String Comment
parseSingleComment cur = do
  parentId <- cur @> [jq| span.parent_id |] @@^ "data-parent_id" >>= readInt
  commentId <- cur @@ "rel" >>= readInt
  commentText <- TL.toStrict . innerHtml <$> (cur @> [jq| div.comment__message |])
  user <- parseCommentUser cur
  votes <- parseCommentVotes cur
  let children = []
  pure Comment { .. }

parseCommentUser :: Cursor -> Either String UserInfo
parseCommentUser cur = do
  username <- cur @> [jq| a.user_info |] @@^ "data-user-login"
  avatar <- maybeToRight "Unable to parse image" $ msum [defaultImg, userImg]
  pure UserInfo { .. }
  where
    defaultImg = DefaultAvatar . TL.toStrict . toHtml <$> rightToMaybe (cur @> [jq|svg|])
    userImg = CustomAvatar <$> rightToMaybe (cur @> [jq|img.user-info__image-pic|] @@^ "src")

parseCommentVotes :: Cursor -> Either String Votes
parseCommentVotes cur = do
  text <- cur @> [jq|span.voting-wjt__counter|] @@^ "title"
  let (negStr : _ : posStr : _) = reverse $ T.words text
  pos <- readInt $ T.tail posStr
  neg <- readInt $ T.tail negStr
  pure Votes { .. }
