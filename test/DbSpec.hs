{-# LANGUAGE OverloadedStrings, OverloadedLists, RecordWildCards, DuplicateRecordFields, TypeApplications #-}
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
import Database.Beam hiding(timestamp)
import Database.Beam.Postgres

import Cohabr.Db
import Cohabr.Db.Conversions
import Cohabr.Db.CommentsLoader
import Cohabr.Db.Inserts
import Cohabr.Db.HelperTypes
import Cohabr.Db.Queries
import Cohabr.Db.SqlMonad
import Cohabr.Db.Updates
import qualified Habr.Types as HT
import Habr.Util

testPost :: HT.Post
testPost = HT.Post
  { HT.title = "Test post title"
  , HT.body = "Первая версия тела &mdash; поста"
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

testPostId, testPostIdUpdateContents, testPostIdUpdateBoth :: PostHabrId
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
  , HT.body = "Вторая версия тела поста"
  }

renormalizePostContents :: HT.Post -> HT.Post
renormalizePostContents post = post
  { HT.body = "Первая версия тела — поста"
  }

postTests :: Spec
postTests = do
  describe "Matching posts contents" $ do
    let a ~~ b = it (a <> " ~~ " <> b) $ bodyFuzzyMatch (T.pack a) (T.pack b) `shouldBe` True
    let a ~/~ b = it (a <> " ~/~ " <> b) $ bodyFuzzyMatch (T.pack a) (T.pack b) `shouldBe` False
    "foo bar" ~~ "foo bar"
    "foo bar" ~/~ "foo baz"
    "foo — bar" ~~ "foo &mdash; bar"
    "foo — bar" ~~ "foo &mdash;&mdash; bar"
    "foo amp; bar" ~~ "foo &amp;amp; bar"
    "foo «bar»" ~~ "foo &laquo;bar&raquo;"
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
  describe "Updating post with itself" $
    it "inserts the update without errors keeping the version" $ do
      (oldVerId, newVerId) <- doUpdates testPost testPostId
      oldVerId `shouldBe` newVerId
  describe "Updating post with Unicode/control renormalization" $
    it "inserts the update without errors keeping the version" $ do
      (oldVerId, newVerId) <- doUpdates (renormalizePostContents testPost) testPostId
      oldVerId `shouldBe` newVerId
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
      testStoredPostMatches updated testPostIdUpdateBoth
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

initialComments :: [HT.Comment]
initialComments =
  [ HT.Comment
    { HT.parentId = 0
    , HT.commentId = 10
    , HT.contents = HT.CommentExisting u1 (HT.Votes 5 3) "This is a test comment" False $ tss !! 1
    }
    , HT.Comment
      { HT.parentId = 10
      , HT.commentId = 11
      , HT.contents = HT.CommentExisting u2 (HT.Votes 2 4) "This is another test comment" True $ tss !! 2
      }
      , HT.Comment
        { HT.parentId = 11
        , HT.commentId = 12
        , HT.contents = HT.CommentExisting u3 (HT.Votes 0 1) "This is third test comment" False $ tss !! 3
        }
    , HT.Comment
      { HT.parentId = 10
      , HT.commentId = 13
      , HT.contents = HT.CommentDeleted
      }
      , HT.Comment
        { HT.parentId = 13
        , HT.commentId = 14
        , HT.contents = HT.CommentExisting u4 (HT.Votes 0 1) "This is fourth test comment" False $ tss !! 4
        }
      , HT.Comment
        { HT.parentId = 13
        , HT.commentId = 15
        , HT.contents = HT.CommentExisting u1 (HT.Votes 7 2) "This is fifth test comment" False $ tss !! 5
        }
  , HT.Comment
    { HT.parentId = 0
    , HT.commentId = 16
    , HT.contents = HT.CommentExisting u2 (HT.Votes 9 4) "This is sixth test comment" False $ tss !! 6
    }
  ]

appendedComments :: [HT.Comment]
appendedComments =
  updateComment 2 (\comm -> comm { HT.contents = (HT.contents comm) { HT.votes = HT.Votes 3 6 } }) $
  updateComment 4 (\comm -> comm { HT.contents = (HT.contents comm) { HT.votes = HT.Votes 4 7 } }) $
    initialComments <>
    [ HT.Comment
      { HT.parentId = 0
      , HT.commentId = 17
      , HT.contents = HT.CommentExisting u1 (HT.Votes 5 3) "This is a newly added comment" False $ tss !! 7
      }
    , HT.Comment
      { HT.parentId = 16
      , HT.commentId = 18
      , HT.contents = HT.CommentExisting u1 (HT.Votes 5 3) "This is a reply" False $ tss !! 8
      }
    ]

u1, u2, u3, u4 :: HT.UserInfo
[u1, u2, u3, u4] =
  [ HT.UserInfo ("commuser" <> n') $ HT.CustomAvatar $ HT.URL $ "http://avatars.link/" <> n'
  | n <- [1..4]
  , let n' = T.pack $ show (n :: Int)
  ]

tss :: [LocalTime]
tss = [ LocalTime (ModifiedJulianDay 58648) $ dayFractionToTimeOfDay $ frac / 100 | frac <- [1..] ]

updateComment :: Int -> (HT.Comment -> HT.Comment) -> [HT.Comment] -> [HT.Comment]
updateComment idx f list = prefix <> (f h : rest)
  where (prefix, h:rest) = splitAt idx list

commentTests :: Spec
commentTests = do
  let initialTree = buildCommentsTree initialComments
  let appendedTree = buildCommentsTree appendedComments
  let changedTree = buildCommentsTree $
        updateComment 2 (\comm -> comm { HT.contents = (HT.contents comm) { HT.commentChanged = True, HT.commentText = "This is a changed body" } })
        appendedComments
  let deletedTree = buildCommentsTree $
        updateComment 2 (\comm -> comm { HT.contents = HT.CommentDeleted }) $
        updateComment 5 (\comm -> comm { HT.contents = HT.CommentDeleted })
        appendedComments
  describe "Inserting a comment tree" $ do
    it "inserts without error" $ do
      pid <- runSqlMonad $ pId . fst . fromJust <$> findPostByHabrId testPostId
      runSqlMonad $ insertCommentTree pid initialTree
    it "restores the same comments" $ do
      LoadedComments { .. } <- runSqlMonad $ getPostIdByHabrId testPostId >>= loadComments undefined
      commentsTree `shouldBe` initialTree
      deletedSet `shouldBe` [HabrId 13]
  describe "Updating with more comments" $ do
    it "saves the new comments wihout error" $
      runSqlMonad $ do
        stored <- getStoredPostInfo testPostId
        updateComments $ commentsUpdatesActions (fromJust stored) appendedTree
    it "saves the comments fully" $ do
      LoadedComments { .. } <- runSqlMonad $ getPostIdByHabrId testPostId >>= loadComments undefined
      commentsTree `shouldBe` appendedTree
      deletedSet `shouldBe` [HabrId 13]
  describe "When changing the body" $ do
    it "saves the new comments wihout error" $
      runSqlMonad $ do
        stored <- getStoredPostInfo testPostId
        updateComments $ commentsUpdatesActions (fromJust stored) changedTree
    it "saves the comments fully" $ do
      LoadedComments { .. } <- runSqlMonad $ getPostIdByHabrId testPostId >>= loadComments undefined
      commentsTree `shouldBe` changedTree
      deletedSet `shouldBe` [HabrId 13]
  describe "When deleting a comment" $ do
    it "saves the new comments wihout error" $
      runSqlMonad $ do
        stored <- getStoredPostInfo testPostId
        updateComments $ commentsUpdatesActions (fromJust stored) deletedTree
    it "keeps the non-deleted version" $ do
      LoadedComments { .. } <- runSqlMonad $ getPostIdByHabrId testPostId >>= loadComments undefined
      commentsTree `shouldBe` changedTree
      deletedSet `shouldBe` [HabrId 12, HabrId 13, HabrId 15]

getPostIdByHabrId :: SqlMonad r m => PostHabrId -> m PostPKey
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

runSqlMonad :: (forall r m. SqlMonad r m => m a) -> IO a
runSqlMonad act = withConnection $ \conn -> runReaderT act SqlEnv { conn = conn, stmtLogger = \_ _ -> pure () }

clearTables :: IO ()
clearTables = void $ withConnection $ \conn ->
  mapM_ @[] (\table -> PGS.execute_ conn $ "TRUNCATE TABLE " <> table <> " RESTART IDENTITY CASCADE")
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
