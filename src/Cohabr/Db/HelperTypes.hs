{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}

module Cohabr.Db.HelperTypes where

import Database.Beam
import Database.Beam.Backend.SQL

newtype PKeyId tag = PKeyId { getPKeyId :: Int } deriving (Eq, Ord, Show, Num)
newtype HabrId tag = HabrId { getHabrId :: Int } deriving (Eq, Ord, Show, Num)

deriving instance HasSqlValueSyntax sy Int => HasSqlValueSyntax sy (PKeyId tag)
deriving instance HasSqlValueSyntax sy Int => HasSqlValueSyntax sy (HabrId tag)

deriving instance (BeamSqlBackend be, FromBackendRow be Int) => FromBackendRow be (PKeyId tag)
deriving instance (BeamSqlBackend be, FromBackendRow be Int) => FromBackendRow be (HabrId tag)

deriving instance (BeamSqlBackend be, HasSqlEqualityCheck be Int) => HasSqlEqualityCheck be (PKeyId tag)
deriving instance (BeamSqlBackend be, HasSqlEqualityCheck be Int) => HasSqlEqualityCheck be (HabrId tag)
