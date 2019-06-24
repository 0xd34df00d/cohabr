{-# LANGUAGE OverloadedStrings #-}

module Habr.Normalizer where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Data.Data
import Data.Generics.Uniplate.Data
import HTMLEntities.Decoder

import Habr.Types

normalizeText :: Data from => from -> from
normalizeText = transformBi f
  where
    f = TL.toStrict . TLB.toLazyText . htmlEncodedText

normalizeUrls :: Data from => URL -> from -> from
normalizeUrls (URL base) = transformBi f
  where
    f (URL u) | "//" `T.isPrefixOf` u = URL $ fst (T.breakOn "//" base) <> u
              | otherwise = URL u
