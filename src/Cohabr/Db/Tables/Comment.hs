{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances, TypeFamilies #-}
module Cohabr.Db.Tables.Comment where
import Cohabr.Db.TH

import qualified Data.Text as T
import Data.Time.LocalTime

data Comment f = Comment
  { commentId   :: TableField f Int       SqlInt4       NN Opt
  , sourceId    :: TableField f Int       SqlInt4       N  Req
  , parentId    :: TableField f Int       SqlInt4       N  Req
  , postId      :: TableField f Int       SqlInt4       N  Req
  , user        :: TableField f String    SqlText       N  Req
  , date        :: TableField f LocalTime SqlTimestamp  N  Req
  , text        :: TableField f T.Text    SqlText       N  Req
  , changed     :: TableField f Bool      SqlBool       N  Req
  , scorePlus   :: TableField f Double    SqlFloat8     N  Req
  , scoreMinus  :: TableField f Double    SqlFloat8     N  Req
  , deleted     :: TableField f Bool      SqlBool       N  Req
  , author      :: TableField f Int       SqlInt4       N  Req
  }

$(makeTFAdaptorAndInstance "pComment" ''Comment)
$(makeTable ''Comment 'pComment "comments"
    [ "id"
    , "source_id"
    , "parent"
    , "post_id"
    , "user"
    , "date"
    , "text"
    , "changed"
    , "score_plus"
    , "score_minus"
    , "deleted"
    , "author"
    ]
  )
