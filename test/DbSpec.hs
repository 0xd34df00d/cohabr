{-# LANGUAGE OverloadedStrings, RecordWildCards, DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module DbSpec(spec) where

import Test.Hspec

import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PGS
import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Data.List(sort)
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Tree
import Database.Beam hiding(timestamp)
import Database.Beam.Postgres

import Cohabr.Diff
import Cohabr.Db
import Cohabr.Db.Conversions
import Cohabr.Db.CommentsLoader
import Cohabr.Db.Inserts
import Cohabr.Db.HelperTypes
import Cohabr.Db.Queries
import Cohabr.Db.SqlMonad
import Cohabr.Db.Updates
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
    , HT.avatar = HT.DefaultAvatar
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

postTests :: Spec
postTests = do
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
      (oldVerId, newVerId) <- doUpdates updated testPostId
      oldVerId `shouldBe` newVerId
    describe "stored post matches" $
      testStoredPostMatches updated testPostId
    describe "and then updating it with new contents" $ do
      let updated' = changePostContents updated
      it "inserts the update without errors changing the version" $ do
        (oldVerId, newVerId) <- doUpdates updated' testPostId
        oldVerId `shouldNotBe` newVerId
      describe "stored post matches" $
        testStoredPostMatches updated' testPostId
  describe "Updating post with new contents" $ do
    let updated = changePostContents testPost
    it "inserts the update changing the version" $ do
      (oldVerId, newVerId) <- doUpdates updated testPostIdUpdateContents
      oldVerId `shouldNotBe` newVerId
    describe "stored post matches" $
      testStoredPostMatches updated testPostIdUpdateContents
    describe "and then updating it with new meta" $ do
      let updated' = changePostMeta updated
      it "inserts the update without errors keeping the version" $ do
        (oldVerId, newVerId) <- doUpdates updated' testPostIdUpdateContents
        oldVerId `shouldBe` newVerId
      describe "stored post matches" $
        testStoredPostMatches updated' testPostIdUpdateContents
  describe "Updating post with both new meta and contents" $ do
    let updated = changePostContents $ changePostMeta testPost
    it "inserts the update without errors updating the version" $ do
      (oldVerId, newVerId) <- doUpdates updated testPostIdUpdateBoth
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
        maybeSavedPost <- runSqlMonad $ findPostByHabrId habrId
        isJust maybeSavedPost `shouldBe` True
      it "saved post contents match" $ do
        Just (Post { .. }, PostVersion { .. }) <- runSqlMonad $ findPostByHabrId habrId

        let HT.Post { postStats = HT.PostStats { .. }, .. } = post

        -- TODO time-1.9: compare local times directly
        now <- localTimeToUTC utc . zonedTimeToLocalTime <$> getZonedTime
        diffUTCTime now (localTimeToUTC utc pvAdded) `shouldSatisfy` (< 5 {- seconds -})

        pSourceId `shouldBe` habrId
        pPublished `shouldBe` timestamp
        pUser `shouldBe` Just (HT.username user)
        pLink `shouldBe` HT.getUrl . HT.linkUrl <$> link
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
        hubs <- runSqlMonad $ do
          vid <- pvId . snd . fromJust <$> findPostByHabrId habrId
          getPostVersionHubs vid
        let expectedHubs = sort $ HT.hubs post
        let storedHubs = sort $ fromStoredHub <$> hubs
        storedHubs `shouldBe` expectedHubs
      it "produces the same tags" $ do
        tags <- runSqlMonad $ do
          vid <- pvId . snd . fromJust <$> findPostByHabrId habrId
          getPostVersionTags vid
        let expectedTags = sort $ HT.tags post
        let storedTags = sort $ fromStoredTag <$> tags
        storedTags `shouldBe` expectedTags
      it "produces the same flags" $ do
        flags <- runSqlMonad $ getPostIdByHabrId habrId >>= getPostFlags
        let expectedFlags = sort $ HT.flags post
        let storedFlags = sort $ fromJust . strToFlag . pfFlag <$> flags
        storedFlags `shouldBe` expectedFlags

initialTree :: HT.Comments
initialTree =
  [ Node
      HT.Comment
      { HT.commentId = 10
      , HT.parentId = 0
      , HT.contents = HT.CommentExisting u1 (HT.Votes 5 3) "This is a test comment" False ts1
      }
      [ Node
          HT.Comment
          { HT.commentId = 11
          , HT.parentId = 10
          , HT.contents = HT.CommentExisting u2 (HT.Votes 2 4) "This is another test comment" True ts2
          }
          [ Node
              HT.Comment
              { HT.commentId = 12
              , HT.parentId = 11
              , HT.contents = HT.CommentExisting u3 (HT.Votes 0 1) "This is third test comment" False ts3
              }
              []
          ]
      , Node
          HT.Comment
          { HT.commentId = 13
          , HT.parentId = 10
          , HT.contents = HT.CommentDeleted
          }
          [ Node
              HT.Comment
              { HT.commentId = 14
              , HT.parentId = 13
              , HT.contents = HT.CommentExisting u4 (HT.Votes 0 1) "This is fourth test comment" False ts4
              }
              []
          , Node
              HT.Comment
              { HT.commentId = 15
              , HT.parentId = 13
              , HT.contents = HT.CommentExisting u1 (HT.Votes 7 2) "This is fifth test comment" False ts5
              }
              []
          ]
      ]
  , Node HT.Comment
      { HT.commentId = 16
      , HT.parentId = 0
      , HT.contents = HT.CommentExisting u2 (HT.Votes 9 4) "This is sixth test comment" False ts6
      }
    []
  ]
  where
    [u1, u2, u3, u4] =
      [ HT.UserInfo ("commuser" <> n') $ HT.CustomAvatar $ HT.URL $ "http://avatars.link/" <> n'
      | n <- [1..4]
      , let n' = T.pack $ show (n :: Int)
      ]
    [ts1, ts2, ts3, ts4, ts5, ts6] =
      [ LocalTime (ModifiedJulianDay 58648) $ dayFractionToTimeOfDay $ frac / 100
      | frac <- [1..6]
      ]

commentTests :: Spec
commentTests =
  describe "Inserting a comment tree" $ do
    it "inserts without error" $ do
      pid <- runSqlMonad $ pId . fst . fromJust <$> findPostByHabrId testPostId
      runSqlMonad $ insertCommentTree pid initialTree
      pure () :: Expectation
    it "restores the same comments" $ do
      comments <- runSqlMonad $ getPostIdByHabrId testPostId >>= loadComments undefined
      comments `shouldBe` initialTree

getPostIdByHabrId :: SqlMonad m => HabrId -> m PKeyId
getPostIdByHabrId habrId = pId . fst . fromJust <$> findPostByHabrId habrId

spec :: Spec
spec = do
  describe "Preparing the table" $
    it "drops the existing data" $ do
      clearTables
      prepopulateFlags
  postTests
  commentTests

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
