{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}


module Type.Genre
  ( Genre(..)
  , allGenres
  ) where


import qualified GHC.Generics                         as Generics

import qualified Data.Aeson                           as Aeson
import qualified Data.Profunctor.Product.Default      as ProductProfunctorDefault
import qualified Data.Text                            as Text
import qualified Database.PostgreSQL.Simple.FromField as PGSFromField
import qualified Database.PostgreSQL.Simple.ToField   as PGSToField
import qualified Opaleye                              as O



data Genre
  = Fiction
  | NonFiction
  deriving (Eq, Show, Generics.Generic, Enum, Bounded)


allGenres :: [Genre]
allGenres = [minBound..]


instance Aeson.ToJSON Genre
instance Aeson.FromJSON Genre


instance PGSFromField.FromField Genre where
  fromField f Nothing              = PGSFromField.returnError PGSFromField.UnexpectedNull f ""
  fromField _ (Just "FICTION")     = pure Fiction
  fromField _ (Just "NON-FICTION") = pure NonFiction
  fromField _ (Just _)             = error "SQL-Haskell-Type-Mismatch: `Genre`"


instance PGSToField.ToField Genre where
  toField Fiction    = PGSToField.toField ("FICTION" :: Text.Text)
  toField NonFiction = PGSToField.toField ("NON-FICTION" :: Text.Text)


instance ProductProfunctorDefault.Default O.Constant Genre (O.Column O.PGText) where
  def = O.Constant def'
    where
      def' :: Genre -> O.Column O.PGText
      def' Fiction    = O.pgStrictText "FICTION"
      def' NonFiction = O.pgStrictText "NON-FICTION"


instance O.QueryRunnerColumnDefault O.PGText Genre where
    queryRunnerColumnDefault = O.fieldQueryRunnerColumn
