{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances, TypeFamilies #-}
module Cohabr.Db.Tables.Post where
import Cohabr.Db.TH

import Data.Time.Clock

data Post f = Post
  { postId            :: TableField f Int     SqlInt4       NN Opt
  , sourceId          :: TableField f Int     SqlInt4       NN Req
  , sourceSite        :: TableField f String  SqlText       N  Req
  , user              :: TableField f String  SqlText       N  Req
  , published         :: TableField f UTCTime SqlTimestamp  N  Req
  , link              :: TableField f String  SqlText       N  Req
  , link_name         :: TableField f String  SqlText       N  Req
  , link_type         :: TableField f Int     SqlInt4       N  Req
  , scorePlus         :: TableField f Double  SqlFloat8     N  Req
  , scoreMinus        :: TableField f Double  SqlFloat8     N  Req
  , views             :: TableField f Int     SqlInt4       N  Req
  , orig_views        :: TableField f Int     SqlInt4       N  Req
  , visible           :: TableField f Bool    SqlBool       N  Req
  , scanned           :: TableField f Bool    SqlBool       N  Opt
  , orig_views_nearly :: TableField f Bool    SqlBool       N  Req
  , current_version   :: TableField f Int     SqlInt4       N  Req
  , author            :: TableField f Int     SqlInt4       N  Req
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
