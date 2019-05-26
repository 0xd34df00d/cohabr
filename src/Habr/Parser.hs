{-# LANGUAGE RecordWildCards, OverloadedStrings, QuasiQuotes, TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Habr.Parser
( ParseContext(..)
, parseComments
, parsePost
) where

import qualified Data.IntMap as IM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Read as T
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Either.Combinators
import Data.Maybe
import Data.String.Interpolate
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format(defaultTimeLocale, months, parseTimeM, iso8601DateFormat)
import Text.XML
import Text.XML.Cursor
import Text.XML.Scraping
import Text.XML.Selector.TH
import Text.XML.Selector.Types

import Habr.Types

data ParseContext = ParseContext
  { currentTime :: UTCTime
  } deriving (Eq, Ord, Show)

parsePost :: (MonadReader ParseContext m, MonadError String m) => Cursor -> m Post
parsePost root = do
  title <- TL.toStrict . innerHtml <$> root @> [jq|span.post__title-text|]
  body <- TL.toStrict . innerHtml <$> root @> [jq|div.post__text|]
  hubs <- parseClassifier [jq|.hub-link|]
  tags <- parseClassifier [jq|.post__tag|]
  user <- root @> [jq|.post__meta|] >>= parseUser
  timestamp <- parsePostTime root
  pure Post { .. }
  where
    parseClassifier query = forM (queryT query root) $ \cur -> do
      link <- cur @@ "href"
      let name = TL.toStrict $ innerHtml cur
      pure Classifier { .. }

-- TODO migrate to Data.Time.Format.ISO8601 once time-1.9 is available in LTS
parsePostTime :: MonadError String m => Cursor -> m UTCTime
parsePostTime root = do
  timeText <- root @> [jq|.post__time|] @@^ "data-time_published"
  parseTimeM False defaultTimeLocale (iso8601DateFormat $ Just "%H:%MZ") $ T.unpack timeText

parseComments :: (MonadReader ParseContext m, MonadError String m) => Cursor -> m [Comment]
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

parseSingleComment :: (MonadReader ParseContext m, MonadError String m) => Cursor -> m Comment
parseSingleComment cur = do
  parentId <- cur @> [jq| span.parent_id |] @@^ "data-parent_id" >>= readInt
  commentId <- cur @@ "rel" >>= readInt
  commentText <- TL.toStrict . innerHtml <$> (cur @> [jq| div.comment__message |])
  user <- parseUser cur
  votes <- parseCommentVotes cur
  timestamp <- parseCommentTimestamp cur
  let children = []
  pure Comment { .. }

parseUser :: MonadError String m => Cursor -> m UserInfo
parseUser cur = do
  username <- TL.toStrict . innerHtml <$> cur @> [jq| span.user-info__nickname |]
  avatar <- liftEither $ maybeToRight "unable to parse image" $ msum [defaultImg, userImg]
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

parseCommentTimestamp :: (MonadReader ParseContext m, MonadError String m) => Cursor -> m UTCTime
parseCommentTimestamp cur = do
  text <- innerHtml <$> (cur @> [jq| time |])
  parse $ TL.unpack <$> TL.words text
  where
    parse [marker, _, timeStr] = do
      time <- parseTime timeStr
      diff <- case marker of
                   "сегодня" -> pure 0
                   "вчера"   -> pure $ negate 1
                   _         -> throwError [i|unknown date marker `#{marker}`|]
      curDay <- reader $ utctDay . currentTime
      pure UTCTime { utctDay = addDays diff curDay, utctDayTime = time }
    parse ws = parseTimeM False locale "%m %b %Y в %H:%M" $ unwords ws

    -- TODO use DiffTime parsing instance when time-1.9 is available in LTS
    parseTime [h1, h2, ':', m1, m2] = pure $ secondsToDiffTime $ read [h1, h2] * 60 + read [m1, m2]
    parseTime str = throwError [i|unable to parse time string `#{str}`|]

    locale = defaultTimeLocale
             { months = (, "") <$> [ "января"
                                   , "февраля"
                                   , "марта"
                                   , "апреля"
                                   , "мая"
                                   , "июня"
                                   , "июля"
                                   , "августа"
                                   , "сентября"
                                   , "октября"
                                   , "ноября"
                                   , "декабря"
                                   ]
             }
