{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds, RankNTypes, ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}

module Cohabr.Fetch
( refetchPost
, pollRSS

, UpdatesThread
, newUpdatesThread
, withUpdatesThread

, UpdatesConfig(..)
, HttpConfig(..)

, checkUpdates
) where

import qualified Data.HashSet as HS
import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Exception(evaluate)
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Reader
import Data.Default
import Data.List
import Data.Monoid
import Data.Ord
import Data.Time.Clock
import Data.Time.LocalTime
import Numeric.Natural
import Text.HTML.DOM(parseLBS)
import Text.XML.Cursor(fromDocument, node)

import Cohabr.Db(PostHabrId, PostPKey)
import Cohabr.Db.HelperTypes
import Cohabr.Db.Inserts
import Cohabr.Db.Queries
import Cohabr.Db.SqlMonad
import Cohabr.Db.Updates
import Cohabr.Fetch.ErrorHandling
import Cohabr.Logger
import Cohabr.Metrics
import Cohabr.MoscowTime
import Habr.Normalizer
import Habr.Parser
import Habr.Types
import Habr.RSS
import Habr.Util

type MetricableSqlMonad r m = (LoggerMonad r m, SqlMonad r m, Has HttpConfig r, MonadCatch m, MonadMetrics m)

type MetricableSqlMonadRunner = forall a. (forall r m. MetricableSqlMonad r m => m a) -> IO a

refetchPost :: MetricableSqlMonad r m => PostHabrId -> m ()
refetchPost habrPostId = catchesMaybe (handleHttpExceptionPost habrPostId "<a href=\"https://habr.com/ru/users/" ()) $ do
  writeLog LogDebug $ "fetching post " <> show habrPostId

  fetchAndParse habrPostId >>= \case
    Left errs -> writeLog LogError $ unlines $ "Unable to parse " <> show habrPostId : errs
    Right (post, comments) -> do
      track LastPageFetch
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

fetchAndParse :: MetricableSqlMonad r m => PostHabrId -> m (Either [String] (Post, Comments))
fetchAndParse habrPostId = do
  currentTime <- liftIO $ zonedTimeToLocalTime <$> getZonedTime

  let url = urlForPostId $ getHabrId habrPostId
  postPage <- timed PageFetchTime $ httpWithTimeout url

  let root = fromDocument $ parseLBS postPage
  void $ timed PageXMLParseTime $ force' $ node root
  let normalize = normalizeUrls $ URL url
  timed PageContentsParseTime $ force' $ normalize $ runExcept $ runReaderT ((,) <$> parsePost root <*> parseComments root) ParseContext { .. }
  where
    force' = liftIO . evaluate . force

pollRSS :: MetricableSqlMonad r m => m ()
pollRSS = catchesMaybe handlers $ do
  rss <- httpWithTimeout "https://habr.com/ru/rss/all/all/?fl=ru%2Cen"
  case recentArticles rss of
    Nothing -> writeLog LogError "No posts at all detected in the RSS feed"
    Just ids -> do
      track LastRssFetch
      writeLog LogDebug $ "Got new posts: " <> show ids
      recs <- selectMissingPosts $ HabrId <$> ids
      track NewPostsCount $ fromIntegral $ length recs
      writeLog LogDebug $ "Got missing posts: " <> show recs
      mapM_ refetchPost recs
  where
    handlers =
      [ handler $ httpTimeoutHandler rssStr
      , handler $ httpGenericHandler rssStr
      , handler $ httpHardTimeoutHandler rssStr
      ]
    rssStr = "RSS" :: String

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

data UpdatesConfig = UpdatesConfig
  { updateThreads :: Int
  , pendingUpdatesQueueSize :: Int
  } deriving (Eq, Ord, Show)

instance Default UpdatesConfig where
  def = UpdatesConfig { .. }
    where
      updateThreads = 1
      pendingUpdatesQueueSize = 1000

newUpdatesThread :: UpdatesConfig -> MetricableSqlMonadRunner -> IO (UpdatesThread, IO ())
newUpdatesThread UpdatesConfig { .. } monadRunner = do
  reqQueue <- newTBQueueIO 1000
  pendingRequests <- newTVarIO mempty
  let ut = UpdatesThread { .. }
  tids <- replicateM updateThreads $ forkIO $ updatesThreadServer ut
  pure (ut, mapM_ killThread tids)

withUpdatesThread :: UpdatesConfig -> MetricableSqlMonadRunner -> (UpdatesThread -> IO a) -> IO a
withUpdatesThread cfg monadRunner f = bracket
  (newUpdatesThread cfg monadRunner)
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

  monadRunner $ do
    if moscowNow `diffLocalTime` published < week
       then refetchPost postHabrId
       else whenM (isRssNewer postPKey postHabrId) $ refetchPost postHabrId
    bumpPostQueryTime postPKey

  threadDelay 100000

isRssNewer :: MetricableSqlMonad r m => PostPKey -> PostHabrId -> m Bool
isRssNewer postPKey habrPostId = catchesMaybe (handleHttpExceptionPost habrPostId "<!DOCTYPE" False) $ do
  rss <- httpWithTimeout $ rssUrlForPostId $ getHabrId habrPostId
  case lastCommentDate rss of
    Nothing -> pure False
    Just lastRss -> do
      maybeLastStored <- timed LastCommentDateQueryTime $ getLastCommentDate postPKey
      case maybeLastStored of
        Nothing -> pure True
        Just lastStored -> pure $ abs (lastStored `diffLocalTime` utcTimeToMoscowTime lastRss) > 30

checkUpdates :: MetricableSqlMonad r m => [PostHabrId] -> UpdatesThread -> m ()
checkUpdates blacklist ut = do
  dates <- timed UpdatesCandidatesQueryTime getPublishUpdateDates
  moscowNow <- utcTimeToMoscowTime . zonedTimeToUTC <$> liftIO getZonedTime
  let fullQueue = filter ((`notElem` blacklist) . postHabrId) $ sortOn (Down . published) $ filter (shallUpdate moscowNow) dates
  track OutdatedItemsCount $ genericLength fullQueue
  let toRequest = take 200 fullQueue
  writeLog LogDebug $ "Scheduling updating " <> show toRequest
  liftIO $ mapM_ (schedulePostCheck ut) toRequest

shallUpdate :: LocalTime -> UpdateInfo -> Bool
shallUpdate now UpdateInfo { .. } = checkDiff > thresholdFor lastModificationDiff
  where
    checkDiff = now `diffLocalTime` lastQueried
    lastModificationDiff = now `diffLocalTime` published

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
                | dt < 2 * year = month
                | otherwise = 2 * month

minute, hour, day, week, month, year :: NominalDiffTime
minute = 60
hour = minute * 60
day = 24 * hour
week = 7 * day
month = 30 * day
year = 365 * week
