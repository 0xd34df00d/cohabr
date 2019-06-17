{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}

module Cohabr.Db.HelperTypes where

import Database.Beam
import Database.Beam.Backend.SQL

newtype PKeyId = PKeyId { getPKeyId :: Int } deriving (Eq, Ord, Show, Num)
newtype HabrId = HabrId { getHabrId :: Int } deriving (Eq, Ord, Show, Num)

deriving instance HasSqlValueSyntax sy Int => HasSqlValueSyntax sy PKeyId
deriving instance HasSqlValueSyntax sy Int => HasSqlValueSyntax sy HabrId

deriving instance (BeamSqlBackend be, FromBackendRow be Int) => FromBackendRow be PKeyId
deriving instance (BeamSqlBackend be, FromBackendRow be Int) => FromBackendRow be HabrId

deriving instance (BeamSqlBackend be, HasSqlEqualityCheck be Int) => HasSqlEqualityCheck be PKeyId
deriving instance (BeamSqlBackend be, HasSqlEqualityCheck be Int) => HasSqlEqualityCheck be HabrId
