{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}

module Habr.Internal.Util
( readInt
) where

import qualified Data.Text as T
import qualified Data.Text.Read as T
import Control.Monad.Except
import Data.Bifunctor
import Data.String.Interpolate

readInt :: (MonadError [String] m) => T.Text -> m Int
readInt text = do
  (val, rest) <- liftEither . first pure $ T.decimal text
  if T.null rest
    then pure val
    else throwError [ [i|unable to parse `#{text}` as int|] ]
