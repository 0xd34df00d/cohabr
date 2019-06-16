{-# LANGUAGE OverloadedStrings, RecordWildCards, DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts, RankNTypes #-}

import Test.Hspec

import qualified Database.PostgreSQL.Simple as PGS
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Reader
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

testPostId, testPostIdUpdateContents, testPostIdUpdateBoth :: HabrId
testPostId = HabrId 1
testPostIdUpdateContents = HabrId 2
testPostIdUpdateBoth = HabrId 3

changePostMeta :: HT.Post -> HT.Post
changePostMeta post = post
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

changePostContents :: HT.Post -> HT.Post
changePostContents post = post
  { HT.title = "Yay we changed the title!"
  , HT.body = "Yay we changed the body!"
  }

main :: IO ()
main = hspec $ do
  describe "Preparing the table" $
    it "drops the existing data" $ do
      liftIO clearTables
      liftIO prepopulateFlags
      pure () :: Expectation
  describe "Inserting new post" $ do
    it "inserts a new post" $
      runSqlMonad (insertPost testPostId testPost) `shouldReturn` PKeyId 1
    it "fails due to dup key when inserting again" $
      runSqlMonad (insertPost testPostId testPost) `shouldThrow` anyException
    it "inserts again with a different ID" $ do
      runSqlMonad (insertPost testPostIdUpdateContents testPost) `shouldNotReturn` PKeyId 1
      runSqlMonad (insertPost testPostIdUpdateBoth testPost) `shouldNotReturn` PKeyId 1
  describe "Retrieving just inserted post" $
    testStoredPostMatches testPost testPostId
  describe "Updating post with new metainformation" $ do
    let updated = changePostMeta testPost
    it "inserts the update without errors keeping the version" $ do
      (oldVerId, newVerId) <- liftIO $ doUpdates updated testPostId
      oldVerId `shouldBe` newVerId
    describe "stored post matches" $
      testStoredPostMatches updated testPostId
    describe "and then updating it with new contents" $ do
      let updated' = changePostContents updated
      it "inserts the update without errors changing the version" $ do
        (oldVerId, newVerId) <- liftIO $ doUpdates updated' testPostId
        oldVerId `shouldNotBe` newVerId
      describe "stored post matches" $
        testStoredPostMatches updated' testPostId
  describe "Updating post with new contents" $ do
    let updated = changePostContents testPost
    it "inserts the update changing the version" $ do
      (oldVerId, newVerId) <- liftIO $ doUpdates updated testPostIdUpdateContents
      oldVerId `shouldNotBe` newVerId
    describe "stored post matches" $
      testStoredPostMatches updated testPostIdUpdateContents
    describe "and then updating it with new meta" $ do
      let updated' = changePostMeta updated
      it "inserts the update without errors keeping the version" $ do
        (oldVerId, newVerId) <- liftIO $ doUpdates updated' testPostIdUpdateContents
        oldVerId `shouldBe` newVerId
      describe "stored post matches" $
        testStoredPostMatches updated' testPostIdUpdateContents
  describe "Updating post with both new meta and contents" $ do
    let updated = changePostContents $ changePostMeta testPost
    it "inserts the update without errors updating the version" $ do
      (oldVerId, newVerId) <- liftIO $ doUpdates updated testPostIdUpdateBoth
      oldVerId `shouldNotBe` newVerId
    describe "stored post matches" $
      testStoredPostMatches updated testPostId
  where
    doUpdates updated habrId = do
        Just stored <- runSqlMonad $ getStoredPostInfo habrId
        runSqlMonad $ updatePost $ postUpdateActions stored updated
        Just (post', _) <- runSqlMonad $ findPostByHabrId habrId
        pure (pCurrentVersion $ storedPost stored, pCurrentVersion post')

    testStoredPostMatches post habrId = do
      it "finds just inserted post" $ do
        maybeSavedPost <- liftIO $ runSqlMonad $ findPostByHabrId habrId
        isJust maybeSavedPost `shouldBe` True
      it "saved post contents match" $ do
        Just (Post { .. }, PostVersion { .. }) <- liftIO $ runSqlMonad $ findPostByHabrId habrId

        let HT.Post { postStats = HT.PostStats { .. }, .. } = post

        -- TODO time-1.9: compare local times directly
        now <- liftIO $ localTimeToUTC utc . zonedTimeToLocalTime <$> getZonedTime
        diffUTCTime now (localTimeToUTC utc pvAdded) `shouldSatisfy` (< 5 {- seconds -})

        pSourceId `shouldBe` habrId
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
        hubs <- liftIO $ runSqlMonad $ do
          vid <- pvId . snd . fromJust <$> findPostByHabrId habrId
          getPostVersionHubs vid
        let expectedHubs = sort $ HT.hubs post
        let storedHubs = sort $ fromStoredHub <$> hubs
        storedHubs `shouldBe` expectedHubs
      it "produces the same tags" $ do
        tags <- liftIO $ runSqlMonad $ do
          vid <- pvId . snd . fromJust <$> findPostByHabrId habrId
          getPostVersionTags vid
        let expectedTags = sort $ HT.tags post
        let storedTags = sort $ fromStoredTag <$> tags
        storedTags `shouldBe` expectedTags
      it "produces the same flags" $ do
        flags <- liftIO $ runSqlMonad $ do
          pid <- pId . fst . fromJust <$> findPostByHabrId habrId
          getPostFlags pid
        let expectedFlags = sort $ HT.flags post
        let storedFlags = sort $ fromJust . strToFlag . pfFlag <$> flags
        storedFlags `shouldBe` expectedFlags

withConnection :: (PGS.Connection -> IO c) -> IO c
withConnection = bracket
  (PGS.connect PGS.defaultConnectInfo { PGS.connectDatabase = "habr_test" })
  PGS.close

runSqlMonad :: (forall m. SqlMonad m => m a) -> IO a
runSqlMonad act = withConnection $ \conn -> runReaderT act SqlEnv { conn = conn, stmtLogger = \_ _ -> pure () }

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

prepopulateFlags :: IO ()
prepopulateFlags = void $ withConnection $ \conn ->
  runBeamPostgres conn $ runInsert $ insert (cFlags cohabrDb) $ insertValues
    [ Flag { fTooltip = mempty, fText = mempty, fId = flagToStr flag }
    | flag <- [minBound .. maxBound]
    ]
