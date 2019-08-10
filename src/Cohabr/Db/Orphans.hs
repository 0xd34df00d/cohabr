{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# OPTIONS_GHC -O0 -Wno-orphans #-}

module Cohabr.Db.Orphans
( module Habr.Types
) where

import Data.Char
import Database.Beam
import Database.Beam.Backend.SQL.SQL92
import Database.Beam.Backend.Types

import Habr.Types(PostType(..))

instance (BeamBackend be, FromBackendRow be String) => FromBackendRow be PostType where
  fromBackendRow = do
    val <- fromBackendRow
    case lookup val tys of
      Just ty -> pure ty
      _ -> fail $ "invalid value for PostType: " <> val
    where tys = [ (postTypeName ty, ty) | ty <- [minBound .. maxBound] ]

instance HasSqlValueSyntax expr String => HasSqlValueSyntax expr PostType where
  sqlValueSyntax = sqlValueSyntax . postTypeName

postTypeName :: PostType -> String
postTypeName ty = drop 2 $ toLower <$> show ty
