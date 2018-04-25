module Class.Resource
  ( Resource(..)
  ) where


import qualified Data.Time as DT


class Resource r where
  identity  :: r -> Int
  updatedAt :: r -> DT.UTCTime
  createdAt :: r -> DT.UTCTime
