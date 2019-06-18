{-# LANGUAGE RecordWildCards, OverloadedStrings, QuasiQuotes, TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Habr.Parser
( ParseContext(..)
, parseComments
, parsePost
) where

import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Either.Combinators
import Data.Functor
import Data.List
import Data.String.Interpolate
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format(defaultTimeLocale, months, parseTimeM)
import Text.XML
import Text.XML.Cursor
import Text.XML.Scraping
import Text.XML.Selector.TH
import Text.XML.Selector.Types

import Habr.Types
import Habr.Util
import Habr.Internal.Util

newtype ParseContext = ParseContext
  { currentTime :: LocalTime
  } deriving (Eq, Ord, Show)

type ParseError = [String]

throwParseError :: MonadError ParseError m => String -> m a
throwParseError = throwError . pure

parsePost :: (MonadReader ParseContext m, MonadError ParseError m) => Cursor -> m Post
parsePost root = do
  title <- T.strip <$> root @>. [jq|span.post__title-text|]
  body <- T.strip <$> root @>. [jq|div.post__text|]
  hubs <- parseHubs root
  tags <- parseTags root
  user <- root @> [jq|.post__meta|] >>= parseUser
  timestamp <- parsePostTime root
  postStats <- root @> [jq|.post-stats|] >>= parsePostStats
  flags <- parseFlags root
  link <- parseLink root
  pure Post { .. }

parseHubs :: MonadError ParseError m => Cursor -> m [Hub]
parseHubs root = forM (queryT [jq|.hub-link|] root) $ \cur -> do
  link <- cur @@ "href"
  let hubName = TL.toStrict $ innerHtml cur
  (hubCode, hubKind) <- analyzeLink $ reverseLinkParts link
  pure Hub { .. }
  where
    analyzeLink (_ : hubCode : kindStr : _) | kindStr == "company" = pure (hubCode, CompanyHub)
                                            | kindStr == "hub" = pure (hubCode, NormalHub)
                                            | otherwise = throwParseError [i|unknown hub type: #{kindStr}|]
    analyzeLink parts = throwParseError [i|unknown hub link format: #{parts}|]

parseTags :: MonadError ParseError m => Cursor -> m [Tag]
parseTags root = forM (queryT [jq|.post__tag|] root) $ \cur -> do
  let name = TL.toStrict $ innerHtml cur
  pure Tag { .. }

parseFlags :: MonadError ParseError m => Cursor -> m [Flag]
parseFlags root = do
  result <- mapM f $ queryT [jq|.post__type-label|] root
  pure $ nub result
  where
    f cur | Just res <- HM.lookup (innerHtml cur) theMap = pure res
          | otherwise = throwParseError [i|unknown post flag: `#{innerHtml cur}`|]
    theMap = HM.fromList [ ("В черновиках", Draftbox)
                         , ("Перевод", Translation)
                         , ("Из песочницы", Sandbox)
                         , ("Tutorial", Tutorial)
                         , ("Новость", News)
                         , ("Recovery Mode", Recovery)
                         , ("Из RSS", RssFeed)
                         ]

parseLink :: MonadError ParseError m => Cursor -> m (Maybe Link)
parseLink root = case runExcept $ root @> [jq|a.post__translatation-link|] of
  Left _ -> pure Nothing
  Right aElem -> do
    linkUrl <- aElem @@ "href"
    let linkText = TL.toStrict $ innerHtml aElem
    linkName <- if textPrefix `T.isPrefixOf` linkText
                   then pure $ T.drop (T.length textPrefix) linkText
                   else throwParseError [i|Unexpected link text: `#{linkText}`|]
    pure $ Just Link { .. }
  where textPrefix = "Автор оригинала: "

-- TODO migrate to Data.Time.Format.ISO8601 once time-1.9 is available in LTS
parsePostTime :: (MonadReader ParseContext m, MonadError ParseError m) => Cursor -> m LocalTime
parsePostTime root = root @>. [jq|.post__time|] >>= parseHumanReadableTimestamp

parsePostStats :: MonadError ParseError m => Cursor -> m PostStats
parsePostStats cur = do
  votes <- parseVotes cur
  bookmarks <- cur @>. [jq|.js-favs_count|] >>= readInt
  views <- cur @>. [jq|.post-stats__views-count|] >>= liftEither . runExcept . parseViews
  pure PostStats { .. }
  where parseViews t = PostViews True <$> readInt t <|> PostViews False <$> readApproxInt (T.unpack t)

readApproxInt :: MonadError ParseError m => String -> m Int
readApproxInt str | (ts, [',', hs, 'k']) <- break (== ',') str = pure $ read ts * 1000 + read (hs : "00")
                  | otherwise = throwParseError [i|#{str} is not in approximate format|]

parseComments :: (MonadReader ParseContext m, MonadError ParseError m) => Cursor -> m [Comment]
parseComments root = buildCommentsTree <$> mapM parseSingleComment (queryT [jq| .js-comment |] root)

(@@) :: MonadError ParseError m => Cursor -> Name -> m T.Text
cur @@ name | [val] <- attrs = pure val
            | otherwise = throwParseError [i|unexpected attribute contents for #{name}: #{attrs}|]
  where attrs = attribute name cur

(@@^) :: MonadError ParseError m => m Cursor -> Name -> m T.Text
mcur @@^ name = mcur >>= (@@ name)

(@>) :: MonadError ParseError m => Cursor -> [JQSelector] -> m Cursor
cur @> expr | (sub:_) <- queryT expr cur = pure sub
            | NodeElement el <- node cur
            , let el' = el { elementNodes = [] } = throwParseError [i|nothing found for expression #{expr} under element #{el'}|]
            | otherwise = throwParseError [i|nothing found for expression #{expr}|]

(@>.) :: MonadError ParseError m => Cursor -> [JQSelector] -> m T.Text
cur @>. expr = TL.toStrict . innerHtml <$> cur @> expr

parseSingleComment :: (MonadReader ParseContext m, MonadError ParseError m) => Cursor -> m Comment
parseSingleComment cur = do
  parentId <- cur @> [jq| span.parent_id |] @@^ "data-parent_id" >>= readInt
  commentId <- cur @@ "rel" >>= readInt
  contents <- cur @> [jq|div.comment|] >>= parseCommentContents
  let children = []
  pure Comment { .. }

parseCommentContents :: (MonadReader ParseContext m, MonadError ParseError m) => Cursor -> m CommentContents
parseCommentContents cur = runExceptT (parseExisting <|> parseDeleted) >>= liftEither
  where
    parseExisting = do
      commentText <- cur @>. [jq| div.comment__message |]
      user <- parseUser cur
      votes <- parseVotes cur
      timestamp <- parseCommentTimestamp cur
      pure CommentExisting { .. }
    parseDeleted = cur @> [jq|.js-comment > div.comment > div.comment__message_banned|] $> CommentDeleted

parseUser :: MonadError ParseError m => Cursor -> m UserInfo
parseUser cur = do
  username <- cur @>. [jq| span.user-info__nickname |]
  avatar <- liftEither $ maybeToRight ["unable to parse image"] $ msum [userImg, defaultImg]
  pure UserInfo { .. }
  where
    defaultImg = rightToMaybe (cur @> [jq|svg.default-image|]) $> DefaultAvatar
    userImg = CustomAvatar <$> rightToMaybe (cur @> [jq|img.user-info__image-pic|] @@^ "src")

parseVotes :: MonadError ParseError m => Cursor -> m Votes
parseVotes cur = do
  text <- cur @> [jq|span.voting-wjt__counter|] @@^ "title"
  let (negStr : _ : posStr : _) = reverse $ T.words text
  pos <- readInt $ T.tail posStr
  neg <- readInt $ T.tail negStr
  pure Votes { .. }

parseCommentTimestamp :: (MonadReader ParseContext m, MonadError ParseError m) => Cursor -> m LocalTime
parseCommentTimestamp cur = cur @>. [jq|time|] >>= parseHumanReadableTimestamp

parseHumanReadableTimestamp :: (MonadReader ParseContext m, MonadError ParseError m) => T.Text -> m LocalTime
parseHumanReadableTimestamp text = parse $ T.unpack <$> T.words text
  where
    parse [marker, _, timeStr] = do
      time <- parseTime timeStr
      diff <- case marker of
                   "сегодня" -> pure 0
                   "вчера"   -> pure $ negate 1
                   _         -> throwParseError [i|unknown date marker `#{marker}`|]
      curDay <- reader $ localDay . currentTime
      pure LocalTime { localDay = addDays diff curDay, localTimeOfDay = time }
    parse ws = parseTimeM False locale "%e %B %Y в %H:%M" $ unwords ws

    -- TODO use DiffTime parsing instance when time-1.9 is available in LTS
    parseTime [h1, h2, ':', m1, m2] = pure $ timeToTimeOfDay $ secondsToDiffTime $ read [h1, h2] * 60 + read [m1, m2]
    parseTime str = throwParseError [i|unable to parse time string `#{str}`|]

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
