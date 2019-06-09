{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE PolyKinds, DataKinds, FlexibleContexts #-}
{-# LANGUAGE DerivingVia, ScopedTypeVariables #-}

module Cohabr.Db.Updates
( UpdateField(..)
, RawPostVersion(..)
, ListDiff(..)

, PostUpdateActions(..)
, updatePost
) where

import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PGS
import Control.Monad
import Data.List
import Data.Maybe
import Data.Monoid
import GHC.Stack
import Opaleye
import Opaleye.TypeFamilies

import qualified Cohabr.Db.Tables.Hub as H
import qualified Cohabr.Db.Tables.Post as P
import qualified Cohabr.Db.Tables.PostHub as PH
import qualified Cohabr.Db.Tables.PostVersion as PV
import Cohabr.Db.HelperTypes
import Habr.Types

newtype UpdateField tableRec = UpdateField { getUpdate :: tableRec O -> tableRec O }
  deriving (Semigroup, Monoid) via Endo (tableRec O)

data RawPostVersion = RawPostVersion
  { rawPVTitle :: T.Text
  , rawPVText :: T.Text
  } deriving (Eq, Ord, Show)

data ListDiff a = ListDiff
  { added :: [a]
  , removed :: [a]
  } deriving (Eq, Ord, Show)

data PostUpdateActions = PostUpdateActions
  { postId :: PKeyId
  , postUpdates :: [UpdateField P.Post]
  , hubsDiff :: ListDiff Hub
  , newPostVersion :: Maybe RawPostVersion
  }

expectSingleResult :: (HasCallStack, Monad m) => [a] -> m a
expectSingleResult [e] = pure e
expectSingleResult _ = error $ "Expected single ID at:\n" <> prettyCallStack callStack

updatePost :: PGS.Connection -> PostUpdateActions -> IO ()
updatePost conn PostUpdateActions { .. } = do
  ensureHubsExisting conn $ added hubsDiff
  PGS.withTransaction conn $ do
    maybeNewVersionId <- updatePostVersion conn postId newPostVersion

    let additionalUpdates = [
            (\verId -> UpdateField $ \p -> p { P.currentVersion = toFields verId }) <$> maybeNewVersionId
          ]

    let postUpdates' = catMaybes additionalUpdates <> postUpdates

    currentVersion <- expectSingleResult =<< runUpdate_ conn Update
      { uTable = P.postsTable
      , uUpdateWith = updateEasy $ getUpdate $ mconcat postUpdates'
      , uWhere = \post -> P.postId post .== toFields postId
      , uReturning = rReturning P.currentVersion
      }

    updateVersionHubs conn currentVersion hubsDiff

updatePostVersion :: PGS.Connection -> PKeyId -> Maybe RawPostVersion -> IO (Maybe Int)
updatePostVersion _ _ Nothing = pure Nothing
updatePostVersion conn postId (Just RawPostVersion { .. }) = runInsert_ conn Insert
    { iTable = PV.postsVersionsTable
    , iRows = pure PV.PostVersion
      { versionId = Nothing
      , postId = toFields postId
      , added = Nothing
      , title = toFields $ Just rawPVTitle
      , content = toFields rawPVText
      }
    , iReturning = rReturning PV.versionId
    , iOnConflict = Nothing
    }
    >>= fmap Just . expectSingleResult

updateVersionHubs :: PGS.Connection -> PKeyId -> ListDiff Hub -> IO ()
updateVersionHubs conn postVersionId ListDiff { .. } = do
  remCnt <- remove removed
  addCnt <- add added
  guard $ remCnt == genericLength removed && addCnt == genericLength added    -- TODO error handling
  where
    remove [] = pure 0
    remove hubs = runDelete_ conn Delete
      { dTable = PH.postsHubsTable
      , dWhere = \PH.PostHub { .. } -> postVersion .== toFields postVersionId
                                   .&& in_ (toFields . makeHubId <$> hubs) hub
      , dReturning = rCount
      }

    add [] = pure 0
    add hubs = runInsert_ conn Insert
      { iTable = PH.postsHubsTable
      , iRows = mkPostHub <$> hubs
      , iReturning = rCount
      , iOnConflict = Nothing
      }
    mkPostHub h = PH.PostHub { postVersion = toFields postVersionId, hub = toFields $ makeHubId h }

ensureHubsExisting :: PGS.Connection -> [Hub] -> IO ()
ensureHubsExisting conn hubs = void $ runInsert_ conn Insert
  { iTable = H.hubsTable
  , iRows = (\h -> H.Hub { hubId = toFields $ makeHubId h, hubName = toFields $ hubName h }) <$> hubs
  , iReturning = rCount
  , iOnConflict = Just DoNothing
  }

makeHubId :: Hub -> T.Text
makeHubId Hub { .. } = prefix hubKind <> hubCode
  where
    prefix NormalHub = mempty
    prefix CompanyHub = "company-"
