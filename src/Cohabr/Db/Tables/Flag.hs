{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances, TypeFamilies #-}
module Cohabr.Db.Tables.Flag where
import Cohabr.Db.TH

import qualified Data.Text as T

data Flag f = Flag
  { flagId  :: TableField f T.Text SqlText NN Req
  , text    :: TableField f T.Text SqlText NN Req
  , tooltip :: TableField f T.Text SqlText N  Req
  }

$(makeTFAdaptorAndInstance "pFlag" ''Flag)
$(makeTable ''Flag 'pFlag "flags" ["id", "text", "tooltip"])
