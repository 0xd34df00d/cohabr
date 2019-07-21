{-# LANGUAGE MultiParamTypeClasses #-}

module Cohabr.AppEnv where

import Cohabr.Db.SqlMonad
import Cohabr.Fetch

data AppEnv = AppEnv
  { httpConfigPart :: HttpConfig
  , sqlEnvPart :: SqlEnv
  }

instance Has HttpConfig AppEnv where
  extract = httpConfigPart

instance Has SqlEnv AppEnv where
  extract = sqlEnvPart
