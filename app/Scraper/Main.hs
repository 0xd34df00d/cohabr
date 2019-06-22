{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent.Async
import Control.Monad.Except
import Control.Monad.Reader
import Data.Time.LocalTime
import Network.HTTP.Conduit
import Text.HTML.DOM(parseLBS)
import Text.XML.Cursor(fromDocument)
import Time.Repeatedly
import System.IO

import Database.PostgreSQL.Util
import Cohabr.Db
import Cohabr.Db.Inserts
import Cohabr.Db.HelperTypes
import Cohabr.Db.Queries
import Cohabr.Db.SqlMonad
import Cohabr.Db.Updates
import Habr.Parser
import Habr.RSS
import Habr.Util

runSqlMonad :: (forall m. SqlMonad m => m a) -> IO a
runSqlMonad act = withConnection $ \c -> runReaderT act SqlEnv { conn = c, stmtLogger = logger }
  where
    logger LogSqlStmt _ = pure ()
    logger _ msg = liftIO $ putStrLn msg

refetchPost :: PostHabrId -> IO ()
refetchPost habrPostId = do
  now <- zonedTimeToLocalTime <$> getZonedTime
  postPage <- simpleHttp $ urlForPostId $ getHabrId habrPostId
  let root = fromDocument $ parseLBS postPage
  let parseResult = runExcept $ runReaderT ((,) <$> parsePost root <*> parseComments root) ParseContext { currentTime = now }
  case parseResult of
    Left errs -> hPutStr stderr $ unlines errs
    Right (post, comments) -> do
      maybeStoredInfo <- runSqlMonad $ getStoredPostInfo habrPostId
      case maybeStoredInfo of
        Nothing -> runSqlMonad $ do
          dbId <- insertPost habrPostId post
          insertCommentTree dbId comments
        Just storedInfo -> runSqlMonad $
          updatePost $ postUpdateActions storedInfo post

pollRSS :: IO ()
pollRSS = do
  rss <- simpleHttp "https://habr.com/ru/rss/all/all/?fl=ru%2Cen"
  let maybeIds = recentArticles rss
  case maybeIds of
    Nothing -> undefined
    Just ids -> do
      recs <- runSqlMonad $ selectMissingPosts $ HabrId <$> ids
      mapM_ refetchPost [head recs]

main :: IO ()
main = do
  rssPollHandle <- asyncRepeatedly (1 / 60) pollRSS
  wait rssPollHandle
