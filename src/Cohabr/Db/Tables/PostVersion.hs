{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances, TypeFamilies #-}
module Cohabr.Db.Tables.PostVersion where
import Cohabr.Db.TH

import qualified Data.Text as T
import Data.Time.LocalTime

data PostVersion f = PostVersion
  { versionId :: TableField f Int       SqlInt4       NN  Opt
  , postId    :: TableField f Int       SqlInt4       NN  Req
  , added     :: TableField f LocalTime SqlTimestamp  NN  Req
  , title     :: TableField f String    SqlText       N   Req
  , content   :: TableField f T.Text    SqlText       NN  Req
  }

$(makeTFAdaptorAndInstance "pPostVersion" ''PostVersion)
$(makeTable ''PostVersion 'pPostVersion "posts_versions" ["id", "post_id", "added", "title", "content"])
