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

import qualified Data.Int as DI
import qualified GHC.Generics as Generics

import qualified Data.Aeson as DA

import qualified Init as I
import qualified Type.Pagination as TP
import qualified Type.Doc as TD
import qualified Type.Author as TA
import qualified Type.AppError as TAe
import qualified Storage.Author as SA


-- CREATE

createAuthorResource :: TA.AuthorInsert -> I.WithConfig (TD.Document TA.Author)
createAuthorResource =
  fmap indexDocument' . SA.createAuthor


createAuthorResources :: [TA.AuthorInsert] -> I.WithConfig (TD.Document TA.Author)
createAuthorResources =
  fmap docMulti . SA.createAuthors



-- RETRIVE

getAuthorResources :: TP.CursorParam -> I.WithConfig (TD.Document TA.Author)
getAuthorResources cur =
  docMulti <$> SA.getAuthors cur


getAuthorResource :: Int -> I.WithConfig (Either (TD.ErrorDocument TA.Author) (TD.Document TA.Author))
getAuthorResource =
  fmap docOrError . SA.getAuthor



-- UPDATE

updateAuthorResource :: TA.AuthorPut -> I.WithConfig (Either (TD.ErrorDocument TA.Author) (TD.Document TA.Author))
updateAuthorResource = 
  fmap docOrError . SA.updateAuthor


updateAuthorResources :: [TA.AuthorPut] -> I.WithConfig (TD.Document TA.Author)
updateAuthorResources =
  fmap docMulti . SA.updateAuthors



-- DELETE

deleteAuthorResource :: Int -> I.WithConfig (Either (TD.ErrorDocument TA.Author) (TD.Document TA.Author))
deleteAuthorResource = fmap docMetaOrError . SA.deleteAuthor


deleteAuthorResources :: [Int] -> I.WithConfig (TD.Document TA.Author)
deleteAuthorResources = fmap (docMeta . fromIntegral) . SA.deleteAuthors


-- HELPERS

-- JSON API Related

data MetaData
  = CursorInfo TP.Cursor
  | CountInfo Int
  deriving (Eq, Show, Generics.Generic)

instance TD.MetaObject MetaData where
  typeName (CursorInfo _) = "cursor"
  typeName (CountInfo _) = "count"


instance DA.ToJSON MetaData where
  toJSON (CursorInfo cur) = DA.toJSON cur
  toJSON (CountInfo count) = DA.toJSON count


-- Builds the Meta data for the 'index' action
indexMetaData :: [TA.Author] -> TD.Meta
indexMetaData authors = TD.mkMeta (CursorInfo TP.Cursor
  { TP.next = if null authors then 0 else maximum (fmap TA.authorID authors)
  , TP.size = length authors
  })


-- Builds the repsonse Document for the 'index' action
indexDocument :: [TA.Author] -> TD.Meta -> TD.Document TA.Author
indexDocument authors meta = TD.mkListDoc authors (Just meta)


indexDocument' :: TA.Author -> TD.Document TA.Author
indexDocument' = TD.mkSingleDoc


docMulti :: [TA.Author] -> TD.Document TA.Author
docMulti authors =
  indexDocument authors $ indexMetaData authors


docMetaOrError :: DI.Int64 -> Either (TD.ErrorDocument TA.Author) (TD.Document TA.Author)
docMetaOrError 0 = Left $ TAe.docError TAe.ResourceNotFound
docMetaOrError 1 = Right $ indexDocument [] $ TD.mkMeta $ CountInfo 1
docMetaOrError _ = error "Impossible"


docMeta :: Int -> TD.Document TA.Author
docMeta = indexDocument [] . TD.mkMeta . CountInfo


docOrError :: Maybe TA.Author -> Either (TD.ErrorDocument a) (TD.Document TA.Author)
docOrError Nothing = Left $ TAe.docError TAe.ResourceNotFound
docOrError (Just at) = Right $ indexDocument' at
