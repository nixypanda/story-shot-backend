{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Type.Duration
  ( Duration(..)
  ) where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Data.Profunctor.Product.Default (Default(..))
import Database.PostgreSQL.Simple.ToField (ToField, toField)
import Database.PostgreSQL.Simple.FromField
  ( FromField
  , ResultError(..)
  , fromField
  , returnError
  )
import Opaleye
  ( Column
  , Constant(..)
  , PGText
  , QueryRunnerColumnDefault
  , pgStrictText
  , queryRunnerColumnDefault
  , fieldQueryRunnerColumn
  )


data Duration
  = Short
  | Medium
  | Long
  deriving (Eq, Show, Generic)


instance ToJSON Duration

instance FromJSON Duration

instance FromField Duration where
  fromField f Nothing = returnError UnexpectedNull f ""
  fromField _ (Just "SMALL") = pure Short
  fromField _ (Just "MEDIUM") = pure Medium
  fromField _ (Just "LONG") = pure Long
  fromField _ (Just _) = error "SQL-Haskell-Type-Mismatch: `Duration`"

instance ToField Duration where
  toField Short  = toField ("SMALL"  :: Text)
  toField Medium = toField ("MEDIUM" :: Text)
  toField Long   = toField ("LONG"   :: Text)

instance Default Constant Duration (Column PGText) where
  def = Constant def'
    where
      def' :: Duration -> Column PGText
      def' Short = pgStrictText "SMALL"
      def' Medium = pgStrictText "MEDIUM"
      def' Long = pgStrictText "LONG"

instance QueryRunnerColumnDefault PGText Duration where
  queryRunnerColumnDefault = fieldQueryRunnerColumn
