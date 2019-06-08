{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cohabr.Db.HelperTypes where

import Opaleye

newtype HabrId = HabrId { habrId :: Int } deriving (Eq, Ord, Show, QueryRunnerColumnDefault PGInt4)
newtype PKeyId = PKeyId { pkeyId :: Int } deriving (Eq, Ord, Show, QueryRunnerColumnDefault PGInt4)
