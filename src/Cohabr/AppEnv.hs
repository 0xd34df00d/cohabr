{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Cohabr.AppEnv where

import GHC.Generics

import Cohabr.Db.SqlMonad
import Cohabr.Fetch
import Cohabr.Logger

data AppEnv = AppEnv
  { httpConfigPart :: HttpConfig
  , sqlEnvPart :: SqlEnv
  , loggerHolder :: LoggerHolder
  } deriving (Generic, Has HttpConfig, Has SqlEnv, Has LoggerHolder)
