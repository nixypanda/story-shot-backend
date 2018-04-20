{-# OPTIONS_GHC -Wall #-}

module Class.Versioned where

import qualified Data.Time as DT

class Versioned a where
  updatedAt :: a -> DT.UTCTime
  createdAt :: a -> DT.UTCTime
