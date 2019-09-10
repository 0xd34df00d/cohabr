{-# LANGUAGE DataKinds, TypeOperators, PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving, DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Proxy
import Lucid
import Network.Wai
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Server
import Servant.HTML.Lucid

import Habr.Types as HT
import Cohabr.Db
import Cohabr.Db.Loader
import Cohabr.Db.HelperTypes
import Cohabr.Db.SqlMonad
import Cohabr.Db.Utils

data ServedPost = ServedPost
  { post :: HT.PostFromDb
  , comments :: HT.Comments
  }

instance ToHtml ServedPost where
  toHtml _ = strong_ "Yay"
  toHtmlRaw = toHtml

deriving newtype instance FromHttpApiData (HabrId tag)

type CohabrAPI = "post" :> Capture "postid" PostHabrId :> Get '[HTML] ServedPost

cohabrAPI :: Proxy CohabrAPI
cohabrAPI = Proxy

cohabrServer :: Server CohabrAPI
cohabrServer = postServer
  where
    postServer postId = do
      maybeDenorm <- runSqlMonad $ getPostDenorm postId
      case maybeDenorm of
           Nothing -> throwError err404
           Just denorm -> pure ServedPost { post = ppvToPost denorm, comments = undefined }

dbName :: String
dbName = "habr"

runSqlMonad :: MonadIO mio => (forall r m. SqlMonad r m => m a) -> mio a
runSqlMonad act = liftIO $ withConnection dbName $ \pgConn -> runReaderT act (SqlEnv { conn = pgConn, stmtLogger = \_ -> pure () })

cohabrApp :: Application
cohabrApp = serve cohabrAPI cohabrServer

main :: IO ()
main = run 8081 cohabrApp
