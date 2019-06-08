{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances, TypeFamilies #-}
module Cohabr.Db.Tables.PostFlag where
import Cohabr.Db.TH

import qualified Data.Text as T

data PostFlag f = PostFlag
  { post :: TableField f Int    SqlInt4 NN Req
  , flag :: TableField f T.Text SqlText NN Req
  }

$(makeTFAdaptorAndInstance "pPostFlag" ''PostFlag)
$(makeTable ''PostFlag 'pPostFlag "posts_flags" ["post", "flag"])
