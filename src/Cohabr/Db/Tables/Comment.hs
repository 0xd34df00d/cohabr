module Cohabr.Db.Tables.Comment where

import qualified Data.Text as T
import Data.Time.LocalTime
import Database.Beam

data CommentT f = Comment
  { commentId   :: Columnar f Int
  , sourceId    :: Columnar f Int
  , parent      :: Columnar f Int
  , postId      :: Columnar f Int
  , user        :: Columnar f String
  , date        :: Columnar f LocalTime
  , text        :: Columnar f T.Text
  , changed     :: Columnar f Bool
  , scorePlus   :: Columnar f Double
  , scoreMinus  :: Columnar f Double
  , deleted     :: Columnar f Bool
  , author      :: Columnar f Int
  }
