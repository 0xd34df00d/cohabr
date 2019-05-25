{-# LANGUAGE RecordWildCards, OverloadedStrings, ApplicativeDo, QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Habr.Parser where

import qualified Data.IntMap as IM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Read as T
import Control.Monad
import Control.Monad.Except
import Data.Either.Combinators
import Data.Maybe
import Data.String.Interpolate
import Text.XML
import Text.XML.Cursor
import Text.XML.Scraping
import Text.XML.Selector.TH
import Text.XML.Selector.Types

import Habr.Types

parseComments :: MonadError String m => Cursor -> m [Comment]
parseComments root = buildTree <$> mapM parseSingleComment (queryT [jq| .js-comment |] root)

buildTree :: [Comment] -> [Comment]
buildTree comments = go 0
  where
    cid2comment = IM.fromList [(commentId c, c) | c <- comments]
    idsTree = IM.fromListWith (<>) [(parentId, [commentId]) | Comment { .. } <- comments]
    go pid = [ (cid2comment IM.! thisId) { children = go thisId }
             | thisId <- fromMaybe [] $ IM.lookup pid idsTree
             ]

(@@) :: MonadError String m => Cursor -> Name -> m T.Text
cur @@ name | [val] <- attrs = pure val
            | otherwise = throwError [i|unexpected attribute contents for #{name}: #{attrs}|]
  where attrs = attribute name cur

(@@^) :: MonadError String m => m Cursor -> Name -> m T.Text
mcur @@^ name = mcur >>= (@@ name)

(@>) :: MonadError String m => Cursor -> [JQSelector] -> m Cursor
cur @> expr | (sub:_) <- queryT expr cur = pure sub
            | otherwise = throwError [i|nothing found for expression #{expr}|]

readInt :: MonadError String m => T.Text -> m Int
readInt text = do
  (val, rest) <- liftEither $ T.decimal text
  if T.null rest
    then pure val
    else throwError [i|unable to parse `#{text}` as int|]

parseSingleComment :: MonadError String m => Cursor -> m Comment
parseSingleComment cur = do
  parentId <- cur @> [jq| span.parent_id |] @@^ "data-parent_id" >>= readInt
  commentId <- cur @@ "rel" >>= readInt
  commentText <- TL.toStrict . innerHtml <$> (cur @> [jq| div.comment__message |])
  user <- parseCommentUser cur
  votes <- parseCommentVotes cur
  let children = []
  pure Comment { .. }

parseCommentUser :: MonadError String m => Cursor -> m UserInfo
parseCommentUser cur = do
  username <- cur @> [jq| a.user-info |] @@^ "data-user-login"
  avatar <- liftEither $ maybeToRight "Unable to parse image" $ msum [defaultImg, userImg]
  pure UserInfo { .. }
  where
    defaultImg = DefaultAvatar . TL.toStrict . toHtml <$> rightToMaybe (cur @> [jq|svg|])
    userImg = CustomAvatar <$> rightToMaybe (cur @> [jq|img.user-info__image-pic|] @@^ "src")

parseCommentVotes :: MonadError String m => Cursor -> m Votes
parseCommentVotes cur = do
  text <- cur @> [jq|span.voting-wjt__counter|] @@^ "title"
  let (negStr : _ : posStr : _) = reverse $ T.words text
  pos <- readInt $ T.tail posStr
  neg <- readInt $ T.tail negStr
  pure Votes { .. }
