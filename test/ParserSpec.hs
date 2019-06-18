{-# LANGUAGE QuasiQuotes #-}

module ParserSpec(spec) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Reader
import Data.Either
import Data.String.Interpolate
import Data.Time.LocalTime
import Network.HTTP.Conduit
import Test.Hspec
import Text.HTML.DOM(parseLBS)
import Text.XML.Cursor(Cursor, fromDocument)
import System.Directory.Extra

import Habr.Parser

spec :: Spec
spec = beforeAll_ fetchPages $
  describe "Habr parser" $
    it "parses a page successfully" $ do
      now <- liftIO $ zonedTimeToLocalTime <$> getZonedTime
      root <- rootElem 203820
      let parseResult = runExcept $ runReaderT ((,) <$> parsePost root <*> parseComments root) ParseContext { currentTime = now }
      parseResult `shouldSatisfy` isRight

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

rootElem :: MonadIO m => Int -> m Cursor
rootElem pgId = liftIO $ fromDocument . parseLBS <$> LBS.readFile (pathForId pgId)
