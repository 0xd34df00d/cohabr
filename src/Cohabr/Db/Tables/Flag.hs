{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances, TypeFamilies #-}
module Cohabr.Db.Tables.Flag where
import Cohabr.Db.TH

data Flag f = Flag
  { flagId  :: TableField f String SqlText NN Req
  , text    :: TableField f String SqlText N  Req
  , tooltip :: TableField f String SqlText N  Req
  }

$(makeTFAdaptorAndInstance "pFlag" ''Flag)
$(makeTable ''Flag 'pFlag "flags" ["id", "text", "tooltip"])
