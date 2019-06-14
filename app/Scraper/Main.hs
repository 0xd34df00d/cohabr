{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}

module Main where

import Control.Concurrent.Async
import Control.Monad.Except
import Control.Monad.Reader
import Data.String.Interpolate
import Data.Time.LocalTime
import Database.Beam
import Network.HTTP.Conduit
import Text.HTML.DOM(parseLBS)
import Text.XML.Cursor(fromDocument)
import Time.Repeatedly
import System.IO

import Database.PostgreSQL.Util
import Cohabr.Db.Queries
import Cohabr.Db.HelperTypes
import Habr.Parser
import Habr.RSS

refetchPost :: HabrId -> IO ()
refetchPost postId = do
  now <- zonedTimeToLocalTime <$> getZonedTime
  postPage <- simpleHttp [i|https://habr.com/ru/post/#{getHabrId postId}/|]
  let root = fromDocument $ parseLBS postPage
  let parseResult = runExcept $ runReaderT ((,) <$> parsePost root <*> parseComments root) ParseContext { currentTime = now }
  case parseResult of
    Left errs -> hPutStr stderr $ unlines errs
    Right (post, comments) -> do
      maybeOurVersion <- withConnection $ \conn -> findPostByHabrId conn postId
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
      recs <- withConnection $ \conn -> selectMissingPosts conn $ HabrId <$> ids
      mapM_ refetchPost [head recs]

main :: IO ()
main = do
  rssPollHandle <- asyncRepeatedly (1 / 60) pollRSS
  wait rssPollHandle
