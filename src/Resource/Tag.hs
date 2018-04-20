{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Resource.Tag
  ( getTagResources
  , getTagResource
  , createTagResources
  , createTagResource
  , updateTagResources
  , updateTagResource
  , deleteTagResources
  , deleteTagResource
  ) where

import qualified Data.Int as DI
import qualified GHC.Generics as Generics

import qualified Data.Aeson as DA

import qualified Init as I
import qualified Type.Pagination as TP
import qualified Type.Doc as TD
import qualified Type.Tag as TT
import qualified Type.AppError as TAe
import qualified Storage.Tag as ST


-- CREATE

createTagResource :: TT.TagInsert -> I.WithConfig (TD.Document TT.Tag)
createTagResource =
  fmap indexDocument' . ST.createTag


createTagResources :: [TT.TagInsert] -> I.WithConfig (TD.Document TT.Tag)
createTagResources =
  fmap docMulti . ST.createTags



-- RETRIVE

getTagResources :: TP.CursorParam -> I.WithConfig (TD.Document TT.Tag)
getTagResources cp =
  docMulti <$> ST.getTags cp


getTagResource :: Int -> I.WithConfig (Either (TD.ErrorDocument TT.Tag) (TD.Document TT.Tag))
getTagResource =
  fmap docOrError . ST.getTag



-- UPDATE

updateTagResource :: TT.TagPut -> I.WithConfig (Either (TD.ErrorDocument TT.Tag) (TD.Document TT.Tag))
updateTagResource = 
  fmap docOrError . ST.updateTag


updateTagResources :: [TT.TagPut] -> I.WithConfig (TD.Document TT.Tag)
updateTagResources =
  fmap docMulti . ST.updateTags



-- DELETE

deleteTagResource :: Int -> I.WithConfig (Either (TD.ErrorDocument TT.Tag) (TD.Document TT.Tag))
deleteTagResource =
    fmap docMetaOrError . ST.deleteTag


deleteTagResources :: [Int] -> I.WithConfig (TD.Document TT.Tag)
deleteTagResources =
  fmap (docMeta . fromIntegral) . ST.deleteTags


-- HELPERS

-- JSON API Related

data TagMetaData = CursorInfo TP.Cursor | CountInfo Int
  deriving (Eq, Show, Generics.Generic)

instance TD.MetaObject TagMetaData where
  typeName (CursorInfo _) = "cursor"
  typeName (CountInfo _) = "count"


instance DA.ToJSON TagMetaData where
  toJSON (CursorInfo cur) = DA.toJSON cur
  toJSON (CountInfo count) = DA.toJSON count


-- Builds the Meta data for the 'index' action
indexMetaData :: [TT.Tag] -> TD.Meta
indexMetaData tags = TD.mkMeta (CursorInfo TP.Cursor
  { TP.next = if null tags then 0 else maximum (fmap TT.tagID tags)
  , TP.size = length tags
  })


-- Builds the repsonse Document for the 'index' action
indexDocument :: [TT.Tag] -> TD.Meta -> TD.Document TT.Tag
indexDocument tags meta =
  TD.mkListDoc tags (Just meta)


indexDocument' :: TT.Tag -> TD.Document TT.Tag
indexDocument' = TD.mkSingleDoc


docMulti :: [TT.Tag] -> TD.Document TT.Tag
docMulti tags =
  indexDocument tags $ indexMetaData tags


docMetaOrError :: DI.Int64 -> Either (TD.ErrorDocument TT.Tag) (TD.Document TT.Tag)
docMetaOrError 0 = Left $ TAe.docError TAe.ResourceNotFound
docMetaOrError 1 = Right $ indexDocument [] $ TD.mkMeta $ CountInfo 1
docMetaOrError _ = error "Impossible"


docMeta :: Int -> TD.Document TT.Tag
docMeta = indexDocument [] . TD.mkMeta . CountInfo


docOrError :: Maybe TT.Tag -> Either (TD.ErrorDocument a) (TD.Document TT.Tag)
docOrError Nothing = Left $ TAe.docError TAe.ResourceNotFound
docOrError (Just at) = Right $ indexDocument' at
