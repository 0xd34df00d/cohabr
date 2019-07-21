{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Control.Monad.Reader.Has where

class Has part r where
  extract :: r -> part

instance Has r r where
  extract = id
