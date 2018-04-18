{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Type.Or
  where


import GHC.Generics (Generic)

import qualified Data.Aeson as DA

newtype Or a b =
  Or (Either a b)
  deriving (Eq, Show, Functor, Applicative, Monad, Generic)


instance (DA.ToJSON a, DA.ToJSON b) => DA.ToJSON (Or a b) where
  toJSON (Or (Left a)) = DA.toJSON a
  toJSON (Or (Right b)) = DA.toJSON b
