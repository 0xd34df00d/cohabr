{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Cohabr.Fetch where

import qualified Data.ByteString.Char8 as BS
import Control.DeepSeq
import Control.Exception(evaluate)
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Data.Monoid
import Data.Time.LocalTime
import Network.HTTP.Client(HttpException(..), HttpExceptionContent(..))
import Network.HTTP.Conduit hiding(Proxy)
import Network.HTTP.Types.Status(statusCode)
import Text.HTML.DOM(parseLBS)
import Text.XML.Cursor(fromDocument, node)

import Cohabr.Db
import Cohabr.Db.HelperTypes
import Cohabr.Db.Inserts
import Cohabr.Db.Queries
import Cohabr.Db.SqlMonad
import Cohabr.Db.Updates
import Cohabr.Metrics
import Habr.Normalizer
import Habr.Parser
import Habr.Types
import Habr.RSS
import Habr.Util

refetchPost :: (SqlMonad m, MonadCatch m, MonadMetrics m) => PostHabrId -> m ()
refetchPost habrPostId = handleJust selector handler $ do
  writeLog LogDebug $ "fetching post " <> show habrPostId
  now <- liftIO $ zonedTimeToLocalTime <$> getZonedTime
  let url = urlForPostId $ getHabrId habrPostId
  let normalize = normalizeUrls $ URL url
  postPage <- timed PageFetchTime $ simpleHttp url
  let root = fromDocument $ parseLBS postPage
  void $ timed PageXMLParseTime $ force' $ node root
  parseResult <- timed PageContentsParseTime $ force' $ normalize $ runExcept $ runReaderT ((,) <$> parsePost root <*> parseComments root) ParseContext { currentTime = now }
  case parseResult of
    Left errs -> writeLog LogError $ unlines $ "Unable to parse " <> show habrPostId : errs
    Right (post, comments) -> do
      let commentsLength = getSum $ mconcat $ Sum . length <$> comments
      writeLog LogDebug "fetched!"
      trackLogging FetchedCommentsCount $ fromIntegral commentsLength
      maybeStoredInfo <- timed StoredPostInfoRetrievalTime $ getStoredPostInfo habrPostId
      case maybeStoredInfo of
        Nothing -> timed TotalInsertTime $ do
          writeLog LogDebug "inserting new one"
          dbId <- timed PostInsertTime $ insertPost habrPostId post
          unless (null comments) $ timedAvg PerCommentInsertTime commentsLength $ insertCommentTree dbId comments
        Just storedInfo -> timed TotalUpdateTime $ do
          writeLog LogDebug "updating"
          timed PostUpdateTime $ updatePost $ postUpdateActions storedInfo post
          timedAvg PerCommentUpdateTime commentsLength $ updateComments $ commentsUpdatesActions storedInfo comments
  writeLog LogDebug $ "done processing " <> show habrPostId
  where
    selector (HttpExceptionRequest _ (StatusCodeException resp respContents))
      | statusCode (responseStatus resp) == 403
      , "<a href=\"https://habr.com/ru/users/" `BS.isInfixOf` respContents = Just ()
    selector _ = Nothing

    handler _ = do
      writeLog LogError $ "Post is unavailable: " <> show habrPostId
      track DeniedPagesCount

    force' = liftIO . evaluate . force

pollRSS :: (SqlMonad m, MonadCatch m, MonadMetrics m) => m ()
pollRSS = do
  rss <- simpleHttp "https://habr.com/ru/rss/all/all/?fl=ru%2Cen"
  let maybeIds = recentArticles rss
  case maybeIds of
    Nothing -> undefined
    Just ids -> do
      writeLog LogDebug $ "Got new posts: " <> show ids
      recs <- selectMissingPosts $ HabrId <$> ids
      track NewPostsCount $ fromIntegral $ length recs
      writeLog LogDebug $ "Got missing posts: " <> show recs
      mapM_ refetchPost recs
