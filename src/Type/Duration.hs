{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Type.Duration
  ( Duration(..)
  ) where


import qualified GHC.Generics as Generics

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Profunctor.Product.Default as ProductProfunctorDefault
import qualified Database.PostgreSQL.Simple.ToField as PGSToField
import qualified Database.PostgreSQL.Simple.FromField as PGSFromField
import qualified Opaleye as O


data Duration
  = Short
  | Medium
  | Long
  deriving (Eq, Show, Generics.Generic)


instance Aeson.ToJSON Duration
instance Aeson.FromJSON Duration


instance PGSFromField.FromField Duration where
  fromField f Nothing = PGSFromField.returnError PGSFromField.UnexpectedNull f ""
  fromField _ (Just "SMALL") = pure Short
  fromField _ (Just "MEDIUM") = pure Medium
  fromField _ (Just "LONG") = pure Long
  fromField _ (Just _) = error "SQL-Haskell-Type-Mismatch: `Duration`"


instance PGSToField.ToField Duration where
  toField Short  = PGSToField.toField ("SMALL"  :: Text.Text)
  toField Medium = PGSToField.toField ("MEDIUM" :: Text.Text)
  toField Long   = PGSToField.toField ("LONG"   :: Text.Text)


instance ProductProfunctorDefault.Default O.Constant Duration (O.Column O.PGText) where
  def = O.Constant def'
    where
      def' :: Duration -> O.Column O.PGText
      def' Short  = O.pgStrictText "SMALL"
      def' Medium = O.pgStrictText "MEDIUM"
      def' Long   = O.pgStrictText "LONG"


instance O.QueryRunnerColumnDefault O.PGText Duration where
  queryRunnerColumnDefault = O.fieldQueryRunnerColumn

