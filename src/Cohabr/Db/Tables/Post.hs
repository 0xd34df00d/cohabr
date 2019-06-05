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
  , published       :: TableField f LocalTime SqlTimestamp  N  Req
  , link            :: TableField f String    SqlText       N  Req
  , linkName        :: TableField f String    SqlText       N  Req
  , linkType        :: TableField f Int       SqlInt4       N  Req
  , scorePlus       :: TableField f Double    SqlFloat8     N  Req
  , scoreMinus      :: TableField f Double    SqlFloat8     N  Req
  , views           :: TableField f Int       SqlInt4       N  Req
  , origViews       :: TableField f Int       SqlInt4       N  Req
  , visible         :: TableField f Bool      SqlBool       N  Req
  , scanned         :: TableField f Bool      SqlBool       N  Opt
  , origViewsNearly :: TableField f Bool      SqlBool       N  Req
  , currentVersion  :: TableField f Int       SqlInt4       NN Req
  , author          :: TableField f Int       SqlInt4       N  Req
  }

$(makeTFAdaptorAndInstance "pPost" ''Post)
$(makeTable ''Post 'pPost "posts"
    [ "id"
    , "source_id"
    , "source_site"
    , "user"
    , "published"
    , "link"
    , "link_name"
    , "link_type"
    , "score_plus"
    , "score_minus"
    , "views"
    , "orig_views"
    , "visible"
    , "scanned"
    , "orig_views_nearly"
    , "current_version"
    , "author"
    ]
  )
