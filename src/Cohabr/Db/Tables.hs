{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Cohabr.Db.Tables where

import qualified Data.Profunctor.Product.Default as D
import qualified Data.Text as T
import Data.Time.Clock
import Opaleye
import Opaleye.TypeFamilies

import Cohabr.Db.TH

data Comment f = Comment
  { commentId   :: TableField f Int     SqlInt4       NN Req
  , sourceId    :: TableField f Int     SqlInt4       NN Req
  , parentId    :: TableField f Int     SqlInt4       NN Req
  , postId      :: TableField f Int     SqlInt4       NN Req
  , user        :: TableField f String  SqlText       NN Req
  , date        :: TableField f UTCTime SqlTimestamp  NN Req
  , text        :: TableField f T.Text  SqlText       NN Req
  , changed     :: TableField f Bool    SqlBool       NN Req
  , scorePlus   :: TableField f Double  SqlFloat8     NN Req
  , scoreMinus  :: TableField f Double  SqlFloat8     NN Req
  , deleted     :: TableField f Bool    SqlBool       NN Req
  , author      :: TableField f Int     SqlInt4       NN Req
  }

$(makeTFAdaptorAndInstance "pComment" ''Comment)
