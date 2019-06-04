{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances, TypeFamilies #-}
module Cohabr.Db.Tables.PostAlias where
import Cohabr.Db.TH

data PostAlias f = PostAlias
  { postId      :: TableField f Int     SqlInt4 N Req
  , sourceId    :: TableField f Int     SqlInt4 NN Req
  , sourceSite  :: TableField f String  SqlText NN Req
  }

$(makeTFAdaptorAndInstance "pPostAlias" ''PostAlias)
$(makeTable ''PostAlias 'pPostAlias "post_aliases" ["post_id", "source_id", "source_site"])
