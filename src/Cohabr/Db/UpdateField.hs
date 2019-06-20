{-# LANGUAGE RankNTypes, GADTs, FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Cohabr.Db.UpdateField
( UpdateField(..)
, toUpdaterConcat
, produceUpdateField
) where

import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax

data UpdateField table where
  UpdateField :: HasSqlValueSyntax PgValueSyntax a =>
      { accessor :: forall f. table f -> Columnar f a
      , newVal :: a
      } -> UpdateField table

toUpdater :: Beamable table => UpdateField table -> (forall s. table (QField s) -> QAssignment Postgres s)
toUpdater UpdateField { .. } = \table -> accessor table <-. val_ newVal

toUpdaterConcat :: Beamable table => [UpdateField table] -> (forall s. table (QField s) -> QAssignment Postgres s)
toUpdaterConcat = foldMap toUpdater

produceUpdateField :: (Eq a, HasSqlValueSyntax PgValueSyntax a)
                   => table Identity
                   -> (forall f. table f -> Columnar f (Maybe a))
                   -> a
                   -> Maybe (UpdateField table)
produceUpdateField stored acc parsed | Just val <- acc stored
                                     , val == parsed = Nothing
                                     | otherwise = Just $ UpdateField acc (Just parsed)
