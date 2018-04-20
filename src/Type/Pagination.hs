{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Type.Pagination
  ( CursorParam(..)
  , Cursor(..)
  ) where

import qualified GHC.Generics as Generics

import qualified Data.Aeson as DA
import qualified Data.Default as Default



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


instance Default.Default OffsetParam where
  def = OffsetParam 1 10


-- Cursor Based pagination
data Cursor = Cursor
  { next :: Int
  , size :: Int
  } deriving (Show, Eq, Generics.Generic)


instance DA.ToJSON Cursor


data CursorParam = CursorParam
  { nextCursor :: Int
  , sizeCursor :: Int
  } deriving (Show, Eq)


instance Default.Default CursorParam where
  def = CursorParam 0 10
