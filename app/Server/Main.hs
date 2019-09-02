{-# LANGUAGE DataKinds, TypeOperators, PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving, DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Data.Proxy
import Lucid
import Network.Wai
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Server
import Servant.HTML.Lucid

import Habr.Types as HT
import Cohabr.Db
import Cohabr.Db.HelperTypes

data ServedPost = ServedPost
  { post :: HT.Post
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
      pure undefined

cohabrApp :: Application
cohabrApp = serve cohabrAPI cohabrServer

main :: IO ()
main = run 8081 cohabrApp
