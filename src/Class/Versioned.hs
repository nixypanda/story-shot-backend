{-# OPTIONS_GHC -Wall #-}

module Class.Versioned where

import Data.Time (UTCTime)

class Versioned a where
  updatedAt :: a -> UTCTime
  createdAt :: a -> UTCTime
