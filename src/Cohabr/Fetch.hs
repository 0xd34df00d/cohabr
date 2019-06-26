{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds, RankNTypes #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Cohabr.Fetch
( refetchPost
, pollRSS

, UpdatesThread
, newUpdatesThread
, withUpdatesThread

, checkUpdates
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashSet as HS
import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Exception(evaluate)
import Control.Monad.Extra
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Data.List
import Data.Monoid
import Data.Time.Clock
import Data.Time.LocalTime
import Network.HTTP.Client(HttpException(..), HttpExceptionContent(..))
import Network.HTTP.Conduit hiding(Proxy)
import Network.HTTP.Types.Status(statusCode)
import Numeric.Natural
import Text.HTML.DOM(parseLBS)
import Text.XML.Cursor(fromDocument, node)

import Cohabr.Db
import Cohabr.Db.HelperTypes
import Cohabr.Db.Inserts
import Cohabr.Db.Queries
import Cohabr.Db.SqlMonad
import Cohabr.Db.Updates
import Cohabr.Metrics
import Cohabr.MoscowTime
import Habr.Normalizer
import Habr.Parser
import Habr.Types
import Habr.RSS
import Habr.Util

type MetricableSqlMonad m = (SqlMonad m, MonadCatch m, MonadMetrics m)

type MetricableSqlMonadRunner = forall a. (forall m. MetricableSqlMonad m => m a) -> IO a

httpExHandler :: MetricableSqlMonad m => PostHabrId -> BS.ByteString -> HttpException -> m ()
httpExHandler habrPostId marker (HttpExceptionRequest _ (StatusCodeException resp respContents))
  | statusCode (responseStatus resp) `elem` [403, 404]
  , marker `BS.isInfixOf` respContents = track DeniedPagesCount >> writeLog LogError ("Post is unavailable: " <> show habrPostId)
httpExHandler _ _ ex = throwM ex

refetchPost :: MetricableSqlMonad m => PostHabrId -> m ()
refetchPost habrPostId = handle (httpExHandler habrPostId "<a href=\"https://habr.com/ru/users/") $ do
  writeLog LogDebug $ "fetching post " <> show habrPostId

  now <- liftIO $ zonedTimeToLocalTime <$> getZonedTime

  let url = urlForPostId $ getHabrId habrPostId
  postPage <- timed PageFetchTime $ simpleHttp url

  let root = fromDocument $ parseLBS postPage
  void $ timed PageXMLParseTime $ force' $ node root
  let normalize = normalizeUrls $ URL url
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
          let commentsUpdates = commentsUpdatesActions storedInfo comments
          trackLogging UpdatedCommentsCount $ fromIntegral $ newCommentsCount commentsUpdates
          timedAvg PerCommentUpdateTime commentsLength $ updateComments commentsUpdates
  writeLog LogDebug $ "done processing " <> show habrPostId

  where
    force' = liftIO . evaluate . force

pollRSS :: MetricableSqlMonad m => m ()
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

data UpdatesThread = UpdatesThread
  { monadRunner :: MetricableSqlMonadRunner
  , reqQueue :: TBQueue UpdateInfo
  , pendingRequests :: TVar (HS.HashSet PostHabrId)
  }

queueSize :: UpdatesThread -> IO Natural
queueSize ut = atomically $ lengthTBQueue $ reqQueue ut

schedulePostCheck :: UpdatesThread -> UpdateInfo -> IO ()
schedulePostCheck UpdatesThread { .. } updateInfo = atomically $ do
  pending <- readTVar pendingRequests
  unless (phId `HS.member` pending) $ do
    modifyTVar' pendingRequests $ HS.insert phId
    writeTBQueue reqQueue updateInfo
  where phId = postHabrId updateInfo

newUpdatesThread :: MetricableSqlMonadRunner -> IO (UpdatesThread, IO ())
newUpdatesThread monadRunner = do
  reqQueue <- newTBQueueIO 1000
  pendingRequests <- newTVarIO mempty
  let ut = UpdatesThread { .. }
  threadId <- forkIO $ updatesThreadServer ut
  pure (ut, killThread threadId)

withUpdatesThread :: MetricableSqlMonadRunner -> (UpdatesThread -> IO a) -> IO a
withUpdatesThread monadRunner f = bracket
  (newUpdatesThread monadRunner)
  snd
  (f . fst)

updatesThreadServer :: UpdatesThread -> IO ()
updatesThreadServer ut@UpdatesThread { .. } = forever $ do
  UpdateInfo { .. } <- atomically $ do
    updateInfo <- readTBQueue reqQueue
    modifyTVar' pendingRequests $ HS.delete $ postHabrId updateInfo
    pure updateInfo

  utQueueSize <- queueSize ut
  monadRunner $ track UpdateCheckQueueSize $ fromIntegral utQueueSize

  moscowNow <- utcTimeToMoscowTime . zonedTimeToUTC <$> liftIO getZonedTime

  monadRunner $ handleAll (\ex -> writeLog LogError $ "Unable to process " <> show postHabrId <> ":\n" <> show ex) $ do
    if moscowNow `diffLocalTime` published < week
       then refetchPost postHabrId
       else whenM (isRssNewer postPKey postHabrId) $ refetchPost postHabrId
    bumpPostQueryTime postPKey

  threadDelay 1000000

isRssNewer :: MetricableSqlMonad m => PostPKey -> PostHabrId -> m Bool
isRssNewer postPKey habrPostId = handle (\ex -> httpExHandler habrPostId "<!DOCTYPE" ex $> False) $ do
  rss <- simpleHttp $ rssUrlForPostId $ getHabrId habrPostId
  case lastCommentDate rss of
    Nothing -> pure False
    Just lastRss -> do
      maybeLastStored <- timed LastCommentDateQueryTime $ getLastCommentDate postPKey
      case maybeLastStored of
        Nothing -> pure True
        Just lastStored -> pure $ abs (lastStored `diffLocalTime` utcTimeToMoscowTime lastRss) > 30

checkUpdates :: MetricableSqlMonad m => UpdatesThread -> m ()
checkUpdates ut = do
  dates <- timed UpdatesCandidatesQueryTime getPublishUpdateDates
  moscowNow <- utcTimeToMoscowTime . zonedTimeToUTC <$> liftIO getZonedTime
  let fullQueue = reverse $ sortOn (published) $ filter (shallUpdate moscowNow) dates
  track OutdatedItemsCount $ genericLength fullQueue
  let toRequest = take 100 fullQueue
  writeLog LogDebug $ "Scheduling updating " <> show toRequest
  liftIO $ mapM_ (schedulePostCheck ut) toRequest

shallUpdate :: LocalTime -> UpdateInfo -> Bool
shallUpdate now UpdateInfo { .. } = checkDiff > thresholdFor lastModificationDiff
  where
    checkDiff = now `diffLocalTime` lastQueried
    lastModificationDiff = now `diffLocalTime` published

diffLocalTime :: LocalTime -> LocalTime -> NominalDiffTime
diffLocalTime a b = diffUTCTime (localTimeToUTC utc a) (localTimeToUTC utc b)

thresholdFor :: NominalDiffTime -> NominalDiffTime
thresholdFor dt | dt < 5 * minute = 2 * minute
                | dt < 30 * minute = 3 * minute
                | dt < hour = 4 * minute
                | dt < 3 * hour = 5 * minute
                | dt < 6 * hour = 10 * minute
                | dt < 12 * hour = 20 * minute
                | dt < day = 30 * minute
                | dt < 2 * day = hour
                | dt < 3 * day = 3 * hour
                | dt < 4 * day = 6 * hour
                | dt < week = 12 * hour
                | dt < 2 * week = day
                | dt < 4 * week = 2 * day
                | dt < year = week
                | otherwise = month

minute, hour, day, week, month, year :: NominalDiffTime
minute = 60
hour = minute * 60
day = 24 * hour
week = 7 * day
month = 30 * day
year = 365 * week
