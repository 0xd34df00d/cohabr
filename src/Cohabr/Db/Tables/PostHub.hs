{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances, TypeFamilies #-}
module Cohabr.Db.Tables.PostHub where
import Cohabr.Db.TH
import Cohabr.Db.HelperTypes

import qualified Data.Text as T

data PostHub f = PostHub
  { postVersion :: TableField f PKeyId SqlInt4 NN Req
  , hub         :: TableField f T.Text SqlText NN Req
  }

$(makeTFAdaptorAndInstance "pPostHub" ''PostHub)
$(makeTable ''PostHub 'pPostHub "posts_hubs" ["post_version", "hub"])
