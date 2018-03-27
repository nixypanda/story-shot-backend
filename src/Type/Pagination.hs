{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Type.Pagination
  ( CursorParam(..)
  , Cursor(..)
  ) where


import Data.Aeson (ToJSON)
import Data.Default

import GHC.Generics (Generic)


-- Offset based pagination
data Offset = Offset
  { total :: Int
  , pages :: Int
  , page :: Int
  , size :: Int
  } deriving (Show, Eq)


data OffsetParam = OffsetParam
  { page :: Int
  , size :: Int
  } deriving (Show, Eq)


instance Default OffsetParam where
  def = OffsetParam 1 10


-- Cursor Based pagination
data Cursor = Cursor
  { next :: Int
  , size :: Int
  } deriving (Show, Eq, Generic)


instance ToJSON Cursor


data CursorParam = CursorParam
  { nextCursor :: Int
  , sizeCursor :: Int
  } deriving (Show, Eq)


instance Default CursorParam where
  def = CursorParam 0 10
