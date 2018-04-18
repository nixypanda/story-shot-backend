{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Resource.Author
  ( getAuthorResources
  , getAuthorResource
  , createAuthorResources
  , createAuthorResource
  , updateAuthorResources
  , updateAuthorResource
  , deleteAuthorResources
  , deleteAuthorResource
  ) where

import Data.Int (Int64)
import GHC.Generics (Generic)

import Data.Aeson (ToJSON(..))

import Init (WithConfig)
import Type.Author
import Type.Pagination
import Type.Doc
import Type.AppError
import Storage.Author


-- CREATE

createAuthorResource :: AuthorInsert -> WithConfig (Document Author)
createAuthorResource =
  fmap indexDocument' . createAuthor


createAuthorResources :: [AuthorInsert] -> WithConfig (Document Author)
createAuthorResources =
  fmap docMulti . createAuthors



-- RETRIVE

getAuthorResources :: CursorParam -> WithConfig (Document Author)
getAuthorResources cur =
  docMulti <$> getAuthors cur


getAuthorResource :: Int -> WithConfig (Either (ErrorDocument Author) (Document Author))
getAuthorResource =
  fmap docOrError . getAuthor



-- UPDATE

updateAuthorResource :: AuthorPut -> WithConfig (Either (ErrorDocument Author) (Document Author))
updateAuthorResource = 
  fmap docOrError . updateAuthor


updateAuthorResources :: [AuthorPut] -> WithConfig (Document Author)
updateAuthorResources =
  fmap docMulti . updateAuthors



-- DELETE

deleteAuthorResource :: Int -> WithConfig (Either (ErrorDocument Author) (Document Author))
deleteAuthorResource = fmap docMetaOrError . deleteAuthor


deleteAuthorResources :: [Int] -> WithConfig (Document Author)
deleteAuthorResources = fmap (docMeta . fromIntegral) . deleteAuthors


-- HELPERS

-- JSON API Related

data AuthorMetaData
  = CursorInfo Cursor
  | CountInfo Int
  deriving (Eq, Show, Generic)

instance MetaObject AuthorMetaData where
  typeName (CursorInfo _) = "cursor"
  typeName (CountInfo _) = "count"


instance ToJSON AuthorMetaData where
  toJSON (CursorInfo cur) = toJSON cur
  toJSON (CountInfo count) = toJSON count


-- Builds the Meta data for the 'index' action
indexMetaData :: [Author] -> Meta
indexMetaData authors = mkMeta (CursorInfo Cursor
  { next = if null authors then 0 else maximum (fmap authorID authors)
  , size = length authors
  })


-- Builds the repsonse Document for the 'index' action
indexDocument :: [Author] -> Meta -> Document Author
indexDocument authors meta = mkListDoc authors (Just meta)


indexDocument' :: Author -> Document Author
indexDocument' = mkSingleDoc


docMulti :: [Author] -> Document Author
docMulti authors =
  indexDocument authors $ indexMetaData authors


docMetaOrError :: Int64 -> Either (ErrorDocument Author) (Document Author)
docMetaOrError 0 = Left $ docError ResourceNotFound
docMetaOrError 1 = Right $ indexDocument [] $ mkMeta $ CountInfo 1
docMetaOrError _ = error "Impossible"


docMeta :: Int -> Document Author
docMeta = indexDocument [] . mkMeta . CountInfo


docOrError :: Maybe Author -> Either (ErrorDocument a) (Document Author)
docOrError Nothing = Left $ docError ResourceNotFound
docOrError (Just at) = Right $ indexDocument' at
