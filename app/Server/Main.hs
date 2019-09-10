{-# LANGUAGE DataKinds, TypeOperators, PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving, DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import qualified Data.Text as T
import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe
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

noTitleLabel :: T.Text
noTitleLabel = "<Без заголовка>"

instance ToHtml HT.PostFromDb where
  toHtml HT.Post { .. } = div_ [class_ "post-wrapper"] $ do
    h1_ [class_ "post-title"] $ toHtmlRaw $ fromMaybe noTitleLabel title
    case link of
         Nothing -> pure ()
         Just Link { .. } -> div_ [class_ "post-link"] $
                              a_ [href_ $ getUrl linkUrl] $ toHtml linkName
    div_ [class_ "post"] $ toHtmlRaw body
  toHtmlRaw = toHtml

instance ToHtml ServedPost where
  toHtml ServedPost { .. } = doctypehtml_ $ do
    head_ $ title_ $ toHtml $ fromMaybe noTitleLabel (HT.title post) <> " // cohabr"
    body_ $ toHtml post
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
