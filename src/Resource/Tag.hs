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

import Data.Int (Int64)
import GHC.Generics (Generic)

import Data.Aeson (ToJSON(..))

import Init (WithConfig)
import Type.Pagination
import Type.Doc 
import Type.Tag
import Type.AppError
import Storage.Tag
import Utils (toURL)


-- CREATE

createTagResource :: TagInsert -> WithConfig (Document Tag)
createTagResource =
  fmap indexDocument' . createTag


createTagResources :: [TagInsert] -> WithConfig (Document Tag)
createTagResources =
  fmap docMulti . createTags



-- RETRIVE

getTagResources :: CursorParam -> WithConfig (Document Tag)
getTagResources cp =
  docMulti <$> getTags cp


getTagResource :: Int -> WithConfig (Either (ErrorDocument Tag) (Document Tag))
getTagResource =
  fmap docOrError . getTag



-- UPDATE

updateTagResource :: TagPut -> WithConfig (Either (ErrorDocument Tag) (Document Tag))
updateTagResource = 
  fmap docOrError . updateTag


updateTagResources :: [TagPut] -> WithConfig (Document Tag)
updateTagResources =
  fmap docMulti . updateTags



-- DELETE

deleteTagResource :: Int -> WithConfig (Either (ErrorDocument Tag) (Document Tag))
deleteTagResource =
    fmap docMetaOrError . deleteTag


deleteTagResources :: [Int] -> WithConfig (Document Tag)
deleteTagResources =
  fmap (docMeta . fromIntegral) . deleteTags


-- HELPERS

-- JSON API Related

data TagMetaData = CursorInfo Cursor | CountInfo Int
  deriving (Eq, Show, Generic)

instance MetaObject TagMetaData where
  typeName (CursorInfo _) = "cursor"
  typeName (CountInfo _) = "count"


instance ToJSON TagMetaData where
  toJSON (CursorInfo cur) = toJSON cur
  toJSON (CountInfo count) = toJSON count


-- Builds the Meta data for the 'index' action
indexMetaData :: [Tag] -> Meta
indexMetaData tags = mkMeta (CursorInfo Cursor
  { next = if null tags then 0 else maximum (fmap tagID tags)
  , size = length tags
  })


-- Builds the repsonse Document for the 'index' action
indexDocument :: [Tag] -> Meta -> Document Tag
indexDocument tags meta =
  mkListDoc tags (Just meta)


indexDocument' :: Tag -> Document Tag
indexDocument' = mkSingleDoc


docMulti :: [Tag] -> Document Tag
docMulti tags =
  indexDocument tags $ indexMetaData tags


docMetaOrError :: Int64 -> Either (ErrorDocument Tag) (Document Tag)
docMetaOrError 0 = Left $ docError ResourceNotFound
docMetaOrError 1 = Right $ indexDocument [] $ mkMeta $ CountInfo 1
docMetaOrError _ = error "Impossible"


docMeta :: Int -> Document Tag
docMeta = indexDocument [] . mkMeta . CountInfo


docOrError :: Maybe Tag -> Either (ErrorDocument a) (Document Tag)
docOrError Nothing = Left $ docError ResourceNotFound
docOrError (Just at) = Right $ indexDocument' at
