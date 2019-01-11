{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE OverloadedStrings       #-}


module Class.Resource
  ( Resource(..)
  , ShortResource(..)
  , UnlinkedResource(..)
  , LinkedResource(..)
  ) where


import           Data.Monoid ((<>))

import qualified Data.Map    as M
import qualified Data.Text   as Text
import qualified Data.Time   as DT

import qualified TextShow    as TS
import qualified Type.Or     as Or


class Resource r where
  rid  :: r -> Int
  type' :: r -> Text.Text
  updatedAt :: r -> DT.UTCTime
  createdAt :: r -> DT.UTCTime

  link  :: r -> Text.Text
  link r = "/" <> type' r <> "/" <> TS.showt (rid r)


class UnlinkedResource r where
  urid :: r -> Int


class LinkedResource lr where
  lrid :: lr -> Int


class ShortResource sr where
  srid  :: sr -> Int
  srType' :: sr -> Text.Text

  srLink  :: sr -> Text.Text
  srLink sr = "/" <> srType' sr <> "/" <> TS.showt (srid sr)
