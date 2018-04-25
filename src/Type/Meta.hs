{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Type.Meta
  where

import qualified GHC.Generics as Generics
import qualified Data.Int as DInt

import qualified Data.Aeson as Aeson

import qualified Class.Resource as CR
import qualified Type.Doc as TD
import qualified Type.Pagination as TP
import qualified Type.AppError as TAe


data MetaData
  = CursorInfo TP.Cursor
  | CountInfo Int
  deriving (Eq, Show, Generics.Generic)

instance TD.MetaObject MetaData where
  typeName (CursorInfo _) = "cursor"
  typeName (CountInfo _) = "count"


instance Aeson.ToJSON MetaData where
  toJSON (CursorInfo cur)  = Aeson.toJSON cur
  toJSON (CountInfo count) = Aeson.toJSON count


-- Builds the Meta data for the 'index' action
indexMetaData :: (CR.Resource a) => [a] -> TD.Meta
indexMetaData resources = TD.mkMeta (CursorInfo TP.Cursor
  { TP.next = if null resources then 0 else maximum (fmap CR.identity resources)
  , TP.size = length resources
  })


-- Builds the repsonse Document for the 'index' action
indexDocument :: [a] -> TD.Meta -> TD.Document a
indexDocument authors meta = TD.mkListDoc authors (Just meta)


indexDocument' :: r -> TD.Document r
indexDocument' = TD.mkSingleDoc


docMulti :: (CR.Resource r) => [r] -> TD.Document r
docMulti authors = indexDocument authors $ indexMetaData authors


docMetaOrError :: (CR.Resource r) => DInt.Int64 -> Either (TD.ErrorDocument r) (TD.Document r)
docMetaOrError 0 = Left $ TAe.docError TAe.ResourceNotFound
docMetaOrError 1 = Right $ indexDocument [] $ TD.mkMeta $ CountInfo 1
docMetaOrError _ = error "Impossible"


docMeta :: Int -> TD.Document r
docMeta = indexDocument [] . TD.mkMeta . CountInfo


docOrError :: Maybe r -> Either (TD.ErrorDocument r) (TD.Document r)
docOrError Nothing = Left $ TAe.docError TAe.ResourceNotFound
docOrError (Just at) = Right $ indexDocument' at
