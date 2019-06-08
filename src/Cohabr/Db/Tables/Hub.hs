{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances, TypeFamilies #-}
module Cohabr.Db.Tables.Hub where
import Cohabr.Db.TH
import qualified Data.Text as T

data Hub f = Hub
  { hubId   :: TableField f T.Text SqlText NN Req
  , hubName :: TableField f T.Text SqlText NN Req
  }

$(makeTFAdaptorAndInstance "pHub" ''Hub)
$(makeTable ''Hub 'pHub "hubs" ["id", "name"])
