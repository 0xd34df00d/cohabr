{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Cohabr.AppEnv where

import GHC.Generics

import Cohabr.Db.SqlMonad
import Cohabr.Fetch

data AppEnv = AppEnv
  { httpConfigPart :: HttpConfig
  , sqlEnvPart :: SqlEnv
  } deriving (Generic, Has HttpConfig, Has SqlEnv)
