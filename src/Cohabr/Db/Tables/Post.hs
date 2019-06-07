{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances, TypeFamilies #-}
module Cohabr.Db.Tables.Post where
import Cohabr.Db.TH

import Data.Time.LocalTime

-- TODO type safety for the sourceId field
data Post f = Post
  { postId          :: TableField f Int       SqlInt4       NN Opt
  , sourceId        :: TableField f Int       SqlInt4       NN Req
  , sourceSite      :: TableField f String    SqlText       N  Req
  , user            :: TableField f String    SqlText       N  Req
  , published       :: TableField f LocalTime SqlTimestamp  NN Req
  , link            :: TableField f String    SqlText       N  Req
  , linkName        :: TableField f String    SqlText       N  Req
  , scorePlus       :: TableField f Int       SqlInt4       N  Req
  , scoreMinus      :: TableField f Int       SqlInt4       N  Req
  , origViews       :: TableField f Int       SqlInt4       N  Req
  , origViewsNearly :: TableField f Bool      SqlBool       N  Req
  , currentVersion  :: TableField f Int       SqlInt4       NN Req
  , author          :: TableField f Int       SqlInt4       N  Req
  }

$(makeTFAdaptorAndInstance "pPost" ''Post)
$(makeTableLenses ''Post)
$(makeTable ''Post 'pPost "posts"
    [ "id"
    , "source_id"
    , "source_site"
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
