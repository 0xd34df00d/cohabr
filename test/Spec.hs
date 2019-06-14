{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DisambiguateRecordFields #-}

import Test.Hspec

import qualified Database.PostgreSQL.Simple as PGS
import Control.Exception
import Control.Monad.IO.Class
import Data.Time.Calendar
import Data.Time.LocalTime

import Cohabr.Db.Inserts
import Cohabr.Db.HelperTypes
import qualified Habr.Types as HT

withConnection :: (PGS.Connection -> IO c) -> IO c
withConnection = bracket
  (PGS.connect PGS.defaultConnectInfo { PGS.connectDatabase = "habr_test" })
  PGS.close

testPost :: HT.Post
testPost = HT.Post
  { HT.title = "Test post title"
  , HT.body = "First version of the body"
  , HT.hubs = [ HT.Hub "hubcode" "Hub code" HT.NormalHub
              , HT.Hub "initech" "Initech company" HT.CompanyHub
              ]
  , HT.tags = [ HT.Tag "tag1" "http://link1", HT.Tag "tag2" "http://link2" ]
  , HT.flags = [ HT.News, HT.Translation ]
  , HT.link = Nothing
  , HT.user = HT.UserInfo
    { HT.username = "foouser"
    , HT.avatar = HT.DefaultAvatar "svg"
    }
  , HT.timestamp = LocalTime (ModifiedJulianDay 2458648) midnight
  , HT.postStats = HT.PostStats
    { HT.votes = HT.Votes 10 20
    , HT.bookmarks = 5
    , HT.views = HT.PostViews True 123
    }
  }

main :: IO ()
main = hspec $ do
  describe "Inserting new posts" $ do
    it "inserts a new post" $ do
      val <- liftIO $ withConnection $ \conn -> insertPost conn (HabrId 1) testPost
      val `shouldBe` PKeyId 1
