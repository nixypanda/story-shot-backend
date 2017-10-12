{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Type.Genre
  ( Genre(..)
  ) where

import GHC.Generics (Generic)

import Data.Profunctor.Product.Default (Default(..))
import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON)
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


data Genre
  = Fiction
  | NonFiction
  deriving (Eq, Show, Generic)


instance ToJSON Genre

instance FromJSON Genre

instance FromField Genre where
  fromField f Nothing = returnError UnexpectedNull f ""
  fromField _ (Just "FICTION") = pure Fiction
  fromField _ (Just "NON-FICTION") = pure NonFiction
  fromField _ (Just _) = error "SQL-Haskell-Type-Mismatch: `Genre`"

instance ToField Genre where
  toField Fiction = toField ("FICTION" :: Text)
  toField NonFiction = toField ("NON-FICTION" :: Text)

instance Default Constant Genre (Column PGText) where
  def = Constant def'
    where
      def' :: Genre -> Column PGText
      def' Fiction = pgStrictText "FICTION"
      def' NonFiction = pgStrictText "NON-FICTION"

instance QueryRunnerColumnDefault PGText Genre where
    queryRunnerColumnDefault = fieldQueryRunnerColumn
