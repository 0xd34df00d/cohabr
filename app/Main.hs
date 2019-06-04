{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}

module Main where

import qualified Database.PostgreSQL.Simple as PGS

import Control.Concurrent.Async
import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Data.String.Interpolate
import Data.Time.Clock
import Network.HTTP.Conduit
import Opaleye
import Text.XML.Cursor(fromDocument)
import Text.HTML.DOM(parseLBS)
import Time.Repeatedly
import System.IO

import Cohabr.Db.Queries
import Habr.Parser
import Habr.RSS

withConnection :: (PGS.Connection -> IO c) -> IO c
withConnection = bracket
  (PGS.connect PGS.defaultConnectInfo { PGS.connectDatabase = "habr" })
  PGS.close

updatePost :: Int -> IO ()
updatePost postId = do
  now <- getCurrentTime
  postPage <- simpleHttp [i|https://habr.com/ru/post/#{postId}/|]
  let root = fromDocument $ parseLBS postPage
  let parseResult = runExcept $ runReaderT ((,) <$> parsePost root <*> parseComments root) ParseContext { currentTime = now }
  case parseResult of
    Left err -> hPutStrLn stderr err
    Right (post, comments) -> pure ()
  pure ()

pollRSS :: IO ()
pollRSS = do
  rss <- simpleHttp "https://habr.com/ru/rss/all/all/?fl=ru%2Cen"
  let maybeIds = recentArticles rss
  print maybeIds
  case maybeIds of
    Nothing -> undefined
    Just ids -> do
                  (recs :: [Int]) <- withConnection $ \conn -> runSelect conn $ selectKnownPosts ids
                  print recs
                  pure ()

main :: IO ()
main = do
  rssPollHandle <- asyncRepeatedly (1 / 60) pollRSS
  wait rssPollHandle
