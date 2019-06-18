{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module ParserSpec(spec) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Reader
import Data.Either
import Data.List
import Data.String.Interpolate.IsString
import Data.Time.LocalTime
import Network.HTTP.Conduit
import Test.Hspec
import Text.HTML.DOM(parseLBS)
import Text.XML.Cursor(Cursor, fromDocument)
import System.Directory.Extra

import Habr.Parser
import Habr.Types

spec :: Spec
spec = beforeAll_ fetchPages $
  describe "Basic page parsing" $ do
    it "parses a page successfully" $ do
      now <- zonedTimeToLocalTime <$> getZonedTime
      root <- rootElem 203820
      let parseResult = runExcept $ runReaderT ((,) <$> parsePost root <*> parseComments root) ParseContext { currentTime = now }
      parseResult `shouldSatisfy` isRight
    it "parses the post metainformation correctly" $ do
      post <- getParsedPost 203820
      sort (hubs post) `shouldBe` sort [ Hub "silverlight" "Silverlight" NormalHub
                                       , Hub "net" ".NET" NormalHub
                                       , Hub "csharp" "C#" NormalHub
                                       ]
      sort (flags post) `shouldBe` sort [Translation, Tutorial, Recovery]
    it "parses the title and body correctly" $ do
      post <- getParsedPost 203820
      title post `shouldBe` "WCF RIA Services. Начало. Часть 1"
      body post `shouldSatisfy` T.isPrefixOf [i|<a href="http://habrahabr.ru/post/203820/">WCF RIA Services.|]
      body post `shouldSatisfy` T.isSuffixOf [i|<a href="https://github.com/struggleendlessly/Silverlight_WCF_RIA/tree/Part1">Github</a>|]

fetchPages :: IO ()
fetchPages = do
  createDirectoryIfMissing True "test/testpages"
  forM_ ids $ \pgId -> do
    let pgPath = pathForId pgId
    unlessM (doesPathExist pgPath) $ do
      putStrLn [i|Fetching #{pgId}...|]
      pgContents <- simpleHttp [i|https://habr.com/ru/post/#{pgId}/|]
      LBS.writeFile pgPath pgContents
  where ids = [203820]

pathForId :: Int -> FilePath
pathForId pgId = [i|test/testpages/#{pgId}|]

rootElem :: Int -> IO Cursor
rootElem pgId = fromDocument . parseLBS <$> LBS.readFile (pathForId pgId)

getParsedPost :: Int -> IO Post
getParsedPost pgId = do
  now <- zonedTimeToLocalTime <$> getZonedTime
  root <- rootElem pgId
  case runExcept $ runReaderT (parsePost root) ParseContext { currentTime = now } of
    Left err -> error [i|Unable to parse post: #{err}|]
    Right post -> pure post
