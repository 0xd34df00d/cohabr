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
import Cohabr.Db.Inserts
import Cohabr.Db.HelperTypes
import Cohabr.Db.Queries
import Cohabr.Db.SqlMonad
import Cohabr.Db.Updates
import Habr.Parser
import Habr.RSS
import Habr.Util

runSqlMonad :: (forall m. SqlMonad m => m a) -> IO a
runSqlMonad act = withConnection $ \c -> runReaderT act SqlEnv { conn = c, stmtLogger = const putStrLn }

refetchPost :: HabrId -> IO ()
refetchPost postId = do
  now <- zonedTimeToLocalTime <$> getZonedTime
  postPage <- simpleHttp $ urlForPostId $ getHabrId postId
  let root = fromDocument $ parseLBS postPage
  let parseResult = runExcept $ runReaderT ((,) <$> parsePost root <*> parseComments root) ParseContext { currentTime = now }
  case parseResult of
    Left errs -> hPutStr stderr $ unlines errs
    Right (post, comments) -> do
      maybeOurVersion <- runSqlMonad $ findPostByHabrId postId
      case maybeOurVersion of
        Nothing -> putStrLn "new post!"
        Just (ourPost, ourVersion) -> putStrLn "found post!"

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
