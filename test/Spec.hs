{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

import Test.Hspec

import qualified Database.PostgreSQL.Simple as PGS
import Control.Exception
import Control.Monad.IO.Class
import Data.Functor
import Data.List(sort)
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Database.Beam hiding(timestamp)
import Database.Beam.Postgres

import Cohabr.Diff
import Cohabr.Db
import Cohabr.Db.Inserts
import Cohabr.Db.HelperTypes
import Cohabr.Db.Queries
import Cohabr.Db.Updates
import Cohabr.Db.Utils
import qualified Habr.Types as HT

testPost :: HT.Post
testPost = HT.Post
  { HT.title = "Test post title"
  , HT.body = "First version of the body"
  , HT.hubs = [ HT.Hub "somehub" "Some hub" HT.NormalHub
              , HT.Hub "initech" "Initech company" HT.CompanyHub
              ]
  , HT.tags = [ HT.Tag "tag1", HT.Tag "tag2" ]
  , HT.flags = [ HT.News, HT.Translation ]
  , HT.link = Just $ HT.Link "http://ya.ru" "Yayaya"
  , HT.user = HT.UserInfo
    { HT.username = "foouser"
    , HT.avatar = HT.DefaultAvatar "svg"
    }
  , HT.timestamp = LocalTime (ModifiedJulianDay 58648) midnight
  , HT.postStats = HT.PostStats
    { HT.votes = HT.Votes 10 20
    , HT.bookmarks = 5
    , HT.views = HT.PostViews True 123
    }
  }

testPostId :: HabrId
testPostId = HabrId 1

testPostNewMeta :: HT.Post
testPostNewMeta = testPost
  { HT.hubs = [ HT.Hub "otherhub" "Other hub" HT.NormalHub
              , HT.Hub "initech" "Initech company" HT.CompanyHub
              ]
  , HT.tags = [ HT.Tag "tag1", HT.Tag "tag2", HT.Tag "tag3" ]
  , HT.postStats = HT.PostStats
    { HT.votes = HT.Votes 12 22
    , HT.bookmarks = 7
    , HT.views = HT.PostViews True 589
    }
  }

main :: IO ()
main = hspec $ do
  describe "Preparing the table" $
    it "drops the existing data" $ do
      liftIO clearTables
      liftIO prepopulateHubs
      liftIO prepopulateFlags
      pure () :: Expectation
  describe "Inserting new post" $ do
    it "inserts a new post" $
      withConnection (\conn -> insertPost conn testPostId testPost) `shouldReturn` PKeyId 1
    it "fails due to dup key when inserting again" $
      withConnection (\conn -> insertPost conn testPostId testPost) `shouldThrow` anyException
    it "inserts again with a different ID" $
      withConnection (\conn -> insertPost conn (HabrId 2) testPost) `shouldNotReturn` PKeyId 1
  describe "Retrieving just inserted post" $ testStoredPostMatches testPost $ HabrId 1
  describe "Updating post with new metainformation" $ do
    it "inserts the update as expected" $ do
      liftIO $ do
        Just stored <- withConnection $ \conn -> getStoredPostInfo conn $ HabrId 1
        withConnection $ \conn -> updatePost conn $ postUpdateActions stored testPostNewMeta
      pure () :: Expectation
  where
    testStoredPostMatches post postId = do
      it "finds just inserted post" $ do
        maybeSavedPost <- liftIO $ withConnection $ \conn -> findPostByHabrId conn $ HabrId 1
        isJust maybeSavedPost `shouldBe` True
      it "saved post contents match" $ do
        Just (Post { .. }, PostVersion { .. }) <- liftIO $
            withConnection $ \conn -> findPostByHabrId conn postId

        let HT.Post { postStats = HT.PostStats { .. }, .. } = post

        -- TODO time-1.9: compare local times directly
        now <- liftIO $ localTimeToUTC utc . zonedTimeToLocalTime <$> getZonedTime
        diffUTCTime now (localTimeToUTC utc pvAdded) `shouldSatisfy` (< 5 {- seconds -})

        pSourceId `shouldBe` postId
        pPublished `shouldBe` timestamp
        pUser `shouldBe` Just (HT.username user)
        pLink `shouldBe` HT.linkUrl <$> link
        pLinkName `shouldBe` HT.linkName <$> link
        pScorePlus `shouldBe` Just (HT.pos votes)
        pScoreMinus `shouldBe` Just (HT.neg votes)
        pOrigViews `shouldBe` Just (HT.viewsCount views)
        pOrigViewsNearly `shouldBe` Just (not $ HT.isExactCount views)

        pCurrentVersion `shouldBe` pvId
        pvPostId `shouldBe` pId

        pvTitle `shouldBe` Just title
        pvContent `shouldBe` body
      it "produces the same hubs" $ do
        hubs <- liftIO $ withConnection $ \conn -> do
          vid <- pvId . snd . fromJust <$> findPostByHabrId conn postId
          getPostVersionHubs conn vid
        let expectedHubs = sort $ HT.hubs post
        let storedHubs = sort $ fromStoredHub <$> hubs
        storedHubs `shouldBe` expectedHubs
      it "produces the same tags" $ do
        tags <- liftIO $ withConnection $ \conn -> do
          vid <- pvId . snd . fromJust <$> findPostByHabrId conn postId
          getPostVersionTags conn vid
        let expectedTags = sort $ HT.tags post
        let storedTags = sort $ fromStoredTag <$> tags
        storedTags `shouldBe` expectedTags
      it "produces the same flags" $ do
        flags <- liftIO $ withConnection $ \conn -> do
          pid <- pId . fst . fromJust <$> findPostByHabrId conn postId
          getPostFlags conn pid
        let expectedFlags = sort $ HT.flags post
        let storedFlags = sort $ fromJust . strToFlag . pfFlag <$> flags
        storedFlags `shouldBe` expectedFlags

withConnection :: (PGS.Connection -> IO c) -> IO c
withConnection = bracket
  (PGS.connect PGS.defaultConnectInfo { PGS.connectDatabase = "habr_test" })
  PGS.close

clearTables :: IO ()
clearTables = void $ withConnection $ \conn ->
  mapM_ (\table -> PGS.execute_ conn $ "TRUNCATE TABLE " <> table <> " RESTART IDENTITY CASCADE")
    [ "comments"
    , "flags"
    , "hubs"
    , "post_aliases"
    , "posts"
    , "posts_flags"
    , "posts_hubs"
    , "posts_tags"
    , "posts_versions"
    , "saved_images"
    , "user_avatars"
    , "users"
    ]

prepopulateHubs :: IO ()
prepopulateHubs = void $ withConnection $ \conn ->
  runBeamPostgres conn $ runInsert $ insert (cHubs cohabrDb) $ insertValues
    [ Hub { hId = makeHubId hub, hName = HT.hubName hub }
    | hub <- HT.hubs testPost
    ]

prepopulateFlags :: IO ()
prepopulateFlags = void $ withConnection $ \conn ->
  runBeamPostgres conn $ runInsert $ insert (cFlags cohabrDb) $ insertValues
    [ Flag { fTooltip = mempty, fText = mempty, fId = flagToStr flag }
    | flag <- [minBound .. maxBound]
    ]
