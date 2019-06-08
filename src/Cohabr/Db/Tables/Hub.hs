{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances, TypeFamilies #-}
module Cohabr.Db.Tables.Hub where
import Cohabr.Db.TH

data Hub f = Hub
  { hubId   :: TableField f String SqlText NN Req
  , hubName :: TableField f String SqlText NN Req
  }

$(makeTFAdaptorAndInstance "pHub" ''Hub)
$(makeTable ''Hub 'pHub "hubs" ["id", "name"])
