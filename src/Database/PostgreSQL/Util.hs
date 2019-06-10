module Database.PostgreSQL.Util where

import qualified Database.PostgreSQL.Simple as PGS
import Control.Exception

withConnection :: (PGS.Connection -> IO c) -> IO c
withConnection = bracket
  (PGS.connect PGS.defaultConnectInfo { PGS.connectDatabase = "habr" })
  PGS.close
