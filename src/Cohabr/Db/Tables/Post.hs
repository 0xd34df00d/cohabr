{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances, TypeFamilies #-}
module Cohabr.Db.Tables.Post where
import Cohabr.Db.TH
import Cohabr.Db.HelperTypes

import Data.Time.LocalTime

data Post f = Post
  { postId          :: TableField f PKeyId    SqlInt4       NN Opt
  , sourceId        :: TableField f HabrId    SqlInt4       NN Req
  , user            :: TableField f String    SqlText       N  Req
  , published       :: TableField f LocalTime SqlTimestamp  NN Req
  , link            :: TableField f String    SqlText       N  Req
  , linkName        :: TableField f String    SqlText       N  Req
  , scorePlus       :: TableField f Int       SqlInt4       N  Req
  , scoreMinus      :: TableField f Int       SqlInt4       N  Req
  , origViews       :: TableField f Int       SqlInt4       N  Req
  , origViewsNearly :: TableField f Bool      SqlBool       N  Req
  , currentVersion  :: TableField f PKeyId    SqlInt4       NN Req
  , author          :: TableField f PKeyId    SqlInt4       N  Req
  }

$(makeTFAdaptorAndInstance "pPost" ''Post)
$(makeTableLenses ''Post)
$(makeTable ''Post 'pPost "posts"
    [ "id"
    , "source_id"
    , "user"
    , "published"
    , "link"
    , "link_name"
    , "score_plus"
    , "score_minus"
    , "orig_views"
    , "orig_views_nearly"
    , "current_version"
    , "author"
    ]
  )
