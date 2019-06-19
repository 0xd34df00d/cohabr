{-# LANGUAGE OverloadedStrings #-}

module Habr.Normalizer where

import qualified Data.Text as T
import Data.Data
import Data.Generics.Uniplate.Data

import Habr.Types

normalizeUrls :: Data from => URL -> from -> from
normalizeUrls (URL base) = transformBi f
  where
    f (URL u) | "//" `T.isPrefixOf` u = URL $ fst (T.breakOn "//" base) <> u
              | otherwise = URL u
