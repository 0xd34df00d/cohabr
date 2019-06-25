module Cohabr.MoscowTime where

import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Zones
import Data.Time.Zones.All

utcTimeToMoscowTime :: UTCTime -> LocalTime
utcTimeToMoscowTime = utcToLocalTimeTZ $ tzByLabel Europe__Moscow
