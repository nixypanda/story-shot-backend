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
import Network.JSONApi
  ( Document
  , ErrorDocument(..)
  , MetaObject(..)
  , Links
  , Meta
  , mkMeta
  , mkLinks
  , mkDocument
  , singleton
  , mkDocument'
  )

import Storage.Tag
import Type.Pagination
import Type.Tag
import Utils (toURL)
import Init (WithConfig)
import Exception.AppError (APIError, ClientError(..), toErrorDoc)


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



-- Builds the Links data for the 'index' action
indexLinks :: Links
indexLinks = mkLinks [("self", selfLink)]
  where
    selfLink = toURL "/tag"


-- Builds the Meta data for the 'index' action
indexMetaData :: [Tag] -> Meta
indexMetaData tags = mkMeta (CursorInfo Cursor
  { next = if null tags then 0 else maximum (fmap tagID tags)
  , size = length tags
  })


-- Builds the repsonse Document for the 'index' action
indexDocument :: [Tag] -> Links -> Meta -> Document Tag
indexDocument tags links meta =
  mkDocument tags (Just links) (Just meta)


indexDocument' :: Tag -> Document Tag
indexDocument' story' =
  mkDocument' (singleton story') Nothing Nothing


docMulti :: [Tag] -> Document Tag
docMulti tags =
  indexDocument tags indexLinks $ indexMetaData tags


docMetaOrError :: Int64 -> Either (ErrorDocument Tag) (Document Tag)
docMetaOrError 0 = Left $ docError ResourceNotFound
docMetaOrError 1 = Right $ indexDocument [] indexLinks $ mkMeta $ CountInfo 1
docMetaOrError _ = error "Impossible"


docMeta :: Int -> Document Tag
docMeta =
  indexDocument [] indexLinks . mkMeta . CountInfo


docError :: APIError e => e -> ErrorDocument a
docError e =
  ErrorDocument (toErrorDoc e) Nothing Nothing


docOrError :: Maybe Tag -> Either (ErrorDocument a) (Document Tag)
docOrError Nothing = Left $ docError ResourceNotFound
docOrError (Just at) = Right $ indexDocument' at
