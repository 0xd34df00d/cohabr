{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PGS
import Database.Beam
import Database.Beam.Postgres

import Database.PostgreSQL.Util
import Cohabr.Db
import Cohabr.Db.HelperTypes

handleUsers :: PGS.Connection -> [(PKeyId UserT, T.Text)] -> IO ()
handleUsers conn usersList = do
  print $ length usersDups
  mapM_ (reduceDup conn) usersDups
  where
    usersDups = filter ((> 1) . length) $ HM.elems $
                 HM.fromListWith (<>) [ (uname, [uid]) | (uid, uname) <- usersList ]

reduceDup :: PGS.Connection -> [PKeyId UserT] -> IO ()
reduceDup _ [] = pure ()
reduceDup conn (keepId : removeIds) = PGS.withTransaction conn $ runBeamPostgresDebug print conn $ do
  runUpdate $ update (cPosts cohabrDb)
    (\post -> pAuthor post <-. val_ (Just keepId))
    (\post -> pAuthor post `in_` removeIdsSql)
  runUpdate $ update (cComments cohabrDb)
    (\comm -> cAuthor comm <-. val_ (Just keepId))
    (\comm -> cAuthor comm `in_` removeIdsSql)
  runDelete $ delete (cUsers cohabrDb)
    (\user -> uId user `in_` (val_ <$> removeIds))
  where
    removeIdsSql :: forall s. [QGenExpr QValueContext Postgres s (Maybe (PKeyId UserT))]
    removeIdsSql = val_ . Just <$> removeIds

compressUsers :: IO ()
compressUsers = withConnection $ \conn -> do
  allUsers <- runBeamPostgres conn $ runSelectReturningList $ select query
  handleUsers conn allUsers
  where query = orderBy_ (asc_ . fst) $ fmap (\u -> (uId u, uUsername u)) $ all_ $ cUsers cohabrDb

main :: IO ()
main = compressUsers
