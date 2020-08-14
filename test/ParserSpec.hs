{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards, LambdaCase #-}

module ParserSpec(spec) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Reader
import Data.Either
import Data.Generics.Uniplate.Data
import Data.List
import Data.String.Interpolate.IsString
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Tree
import Network.HTTP.Conduit
import Test.Hspec
import Text.HTML.DOM(parseLBS)
import Text.XML.Cursor(Cursor, fromDocument)
import System.Directory.Extra

import Habr.Normalizer
import Habr.Parser
import Habr.Types
import Habr.Util

spec :: Spec
spec = beforeAll_ fetchPages $ do
  describe "Basic page parsing" $ do
    it "parses a page successfully" $ do
      parseCtx <- mkNowContext
      root <- rootElem 203820
      let parseResult = runExcept $ runReaderT ((,) <$> parsePost root <*> parseComments root) parseCtx
      parseResult `shouldSatisfy` isRight
    it "parses the post metainformation correctly" $ do
      Post { .. } <- getParsedPost 203820
      hubs `shouldBeSet` [ Hub "silverlight" "Silverlight" NormalHub
                         , Hub "net" ".NET" NormalHub
                         , Hub "csharp" "C#" NormalHub
                         ]
      flags `shouldBeSet` [Translation, Tutorial, Recovery]
      tags `shouldBeSet` (Tag <$> ["wcf ria services", "silverlight", "c#", ".net"])
      link `shouldBe` Just Link
                           { linkUrl = "http://www.silverlightshow.net/items/WCF-RIA-Services-Part-1-Getting-Started.aspx"
                           , linkName = "Brian Noyes"
                           }
      user `shouldBe` UserInfo
                      { username = "struggleendlessly"
                      , avatar = CustomAvatar "https://habrastorage.org/getpro/habr/avatars/92e/3fc/1b2/92e3fc1b2ba53dfc6ae8618dc5efb653.jpg"
                      }
      timestamp `shouldBe` LocalTime
                           { localDay = fromGregorian 2013 11 27
                           , localTimeOfDay = TimeOfDay 10 52 0
                           }
      postStats `shouldBe` PostStats
                           { votes = Votes 5 3
                           , views = PostViews False 38300
                           }
      postType `shouldBe` TyPost
    it "parses the post title and body correctly" $ do
      post <- getParsedPost 203820
      title post `shouldBe` "WCF RIA Services. Начало. Часть 1"
      body post `shouldSatisfy` T.isPrefixOf [i|<a href="http://habrahabr.ru/post/203820/">WCF RIA Services.|]
      body post `shouldSatisfy` T.isSuffixOf [i|<a href="https://github.com/struggleendlessly/Silverlight_WCF_RIA/tree/Part1">Github</a>|]
    it "parses the comments to the expected structure" $ do
      comments <- getParsedComments 203820
      fmap (fmap trimText) comments `shouldBe` expectedComments203820
  describe "News parsing" $
    it "parses a news item correctly" $ do
      Post { .. } <- getParsedPost 441828
      postType `shouldBe` TyNews
      title `shouldBe` "Обналичивание криптовалют в РФ попало под действие УК"
      hubs `shouldBeSet` [ Hub "business-laws" "Законодательство в IT" NormalHub
                         , Hub "cryptocurrency" "Криптовалюты" NormalHub
                         ]
      postStats `shouldBe` PostStats
                           { votes = Votes 27 21
                           , views = PostViews False 14200
                           }
  where
    trimText = transformBi $ \case
                c@CommentExisting {} -> c { commentText = T.strip $ fst $ T.breakOn "<br" $ commentText c }
                cnt -> cnt


expectedComments203820 :: Comments
expectedComments203820 =
  [ Node
      Comment
      { commentId = 7032184
      , parentId = 0
      , contents = CommentExisting
        { user = UserInfo "isxaker" $ CustomAvatar "https://habrastorage.org/getpro/habr/avatars/b02/9c6/eed/b029c6eed810389dad377cbe051502be.png"
        , votes = Votes 0 0
        , commentText = "а почему вы выбрали именно silverlight app?"
        , commentChanged = False
        , timestamp = LocalTime (fromGregorian 2013 11 27) (TimeOfDay 11 41 0)
        }
      }
      [ Node
          Comment
          { commentId = 7032204
          , parentId = 7032184
          , contents = CommentExisting
            { user = UserInfo "struggleendlessly" $ CustomAvatar "https://habrastorage.org/getpro/habr/avatars/92e/3fc/1b2/92e3fc1b2ba53dfc6ae8618dc5efb653.jpg"
            , votes = Votes 0 0
            , commentText = "для меня работать с XAML в разы приятней и легче, чем cо знаменитой пачкой: HTML + CSS + JS."
            , commentChanged = False
            , timestamp = LocalTime (fromGregorian 2013 11 27) (TimeOfDay 11 46 0)
            }
          }
          []
      ]
  , Node
      Comment
      { commentId = 7032282
      , parentId = 0
      , contents = CommentExisting
        { user = UserInfo "bRUtality" DefaultAvatar
        , votes = Votes 0 0
        , commentText = "Для меня RIA оказался не применим, когда нужно было работать в связке с ASP.NET Web Applicattion. Или я не умею его готовить."
        , commentChanged = False
        , timestamp = LocalTime (fromGregorian 2013 11 27) (TimeOfDay 12 20 0)
        }
      }
      [ Node
          Comment
          { commentId = 7032428
          , parentId = 7032282
          , contents = CommentExisting
            { user = UserInfo "jonie" $ CustomAvatar "https://habrastorage.org/getpro/habr/avatars/2f0/aff/a48/2f0affa48aacc595d3cf52560bed91c2.jpg"
            , votes = Votes 0 0
            , commentText = "Не умеете. Есть например OData factory (System.ServiceModel.DomainServices.Hosting.ODataEndpointFactory) под это дело…"
            , commentChanged = True
            , timestamp = LocalTime (fromGregorian 2013 11 27) (TimeOfDay 12 59 0)
            }
          }
          [ Node
              Comment
              { commentId = 7032554
              , parentId = 7032428
              , contents = CommentExisting
                { user = UserInfo "bRUtality" DefaultAvatar
                , votes = Votes 0 0
                , commentText = "У вас есть живой пример? В сети ничего толкового не нашел."
                , commentChanged = False
                , timestamp = LocalTime (fromGregorian 2013 11 27) (TimeOfDay 13 27 0)
                }
              }
              [ Node
                  Comment
                  { commentId = 7033012
                  , parentId = 7032554
                  , contents = CommentExisting
                    { user = UserInfo "jonie" $ CustomAvatar "https://habrastorage.org/getpro/habr/avatars/2f0/aff/a48/2f0affa48aacc595d3cf52560bed91c2.jpg"
                    , votes = Votes 0 0
                    , commentText = [i|К примеру вот: <a href="http://code.msdn.microsoft.com/How-to-open-a-WCF-RIA-171139fb#content">code.msdn.microsoft.com/How-to-open-a-WCF-RIA-171139fb#content</a>|]
                    , commentChanged = False
                    , timestamp = LocalTime (fromGregorian 2013 11 27) (TimeOfDay 15 14 0)
                    }
                  }
                []
              ]
          ]
      ]
  ]

fetchPages :: IO ()
fetchPages = do
  createDirectoryIfMissing True "test/testpages"
  forM_ ids $ \pgId -> do
    let pgPath = pathForId pgId
    whenM (shouldFetch pgPath) $ do
      putStrLn [i|Fetching #{pgId}...|]
      pgContents <- simpleHttp $ urlForPostId pgId
      LBS.writeFile pgPath pgContents
  where ids = [203820, 441828]

shouldFetch :: FilePath -> IO Bool
shouldFetch pgPath = ifM (notM $ doesPathExist pgPath)
  (pure True)
  $ do
    modTime <- getModificationTime pgPath
    now <- getCurrentTime
    pure $ now `diffUTCTime` modTime > 86400

pathForId :: Int -> FilePath
pathForId pgId = [i|test/testpages/#{pgId}|]

rootElem :: Int -> IO Cursor
rootElem pgId = fromDocument . parseLBS <$> LBS.readFile (pathForId pgId)

mkNowContext :: IO ParseContext
mkNowContext = do
  currentTime <- zonedTimeToLocalTime <$> getZonedTime
  pure ParseContext { .. }

getParsedPost :: Int -> IO Post
getParsedPost pgId = do
  parseCtx <- mkNowContext
  root <- rootElem pgId
  case runExcept $ runReaderT (parsePost root) parseCtx of
    Left err -> error [i|Unable to parse post: #{err}|]
    Right post -> pure $ normalizeUrls (urlForPostId pgId) post

getParsedComments :: Int -> IO Comments
getParsedComments pgId = do
  parseCtx <- mkNowContext
  root <- rootElem pgId
  case runExcept $ runReaderT (parseComments root) parseCtx of
    Left err -> error [i|Unable to parse post: #{err}|]
    Right post -> pure $ normalizeUrls (urlForPostId pgId) post

shouldBeSet :: (Ord a, Show a) => [a] -> [a] -> Expectation
shouldBeSet l r = sort l `shouldBe` sort r
