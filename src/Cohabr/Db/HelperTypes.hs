{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}

module Cohabr.Db.HelperTypes where

import Data.Profunctor.Product.Default
import Opaleye

newtype HabrId = HabrId { habrId :: Int } deriving (Eq, Ord, Show, QueryRunnerColumnDefault PGInt4)
newtype PKeyId = PKeyId { pkeyId :: Int } deriving (Eq, Ord, Show, QueryRunnerColumnDefault PGInt4)

instance Default Constant Int (Column col) => Default Constant HabrId (Column col) where
  def = Constant $ constantExplicit def . habrId

instance Default Constant Int (Column col) => Default Constant PKeyId (Column col) where
  def = Constant $ constantExplicit def . pkeyId
