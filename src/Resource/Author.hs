{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
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

import qualified Init as I
import qualified Type.Pagination as TP
import qualified Type.Doc as TD
import qualified Type.Author as TA
import qualified Type.Meta as TM
import qualified Storage.Author as SA


-- CREATE

createAuthorResource :: TA.AuthorInsert -> I.WithConfig (TD.Document TA.Author)
createAuthorResource = fmap TM.indexDocument' . SA.createAuthor


createAuthorResources :: [TA.AuthorInsert] -> I.WithConfig (TD.Document TA.Author)
createAuthorResources = fmap TM.docMulti . SA.createAuthors



-- RETRIVE

getAuthorResources :: TP.CursorParam -> I.WithConfig (TD.Document TA.Author)
getAuthorResources cur =
  TM.docMulti <$> SA.getAuthors cur


getAuthorResource :: Int -> I.WithConfig (Either (TD.ErrorDocument TA.Author) (TD.Document TA.Author))
getAuthorResource = fmap TM.docOrError . SA.getAuthor



-- UPDATE

updateAuthorResource :: TA.AuthorPut -> I.WithConfig (Either (TD.ErrorDocument TA.Author) (TD.Document TA.Author))
updateAuthorResource = fmap TM.docOrError . SA.updateAuthor


updateAuthorResources :: [TA.AuthorPut] -> I.WithConfig (TD.Document TA.Author)
updateAuthorResources = fmap TM.docMulti . SA.updateAuthors



-- DELETE

deleteAuthorResource :: Int -> I.WithConfig (Either (TD.ErrorDocument TA.Author) (TD.Document TA.Author))
deleteAuthorResource = fmap TM.docMetaOrError . SA.deleteAuthor


deleteAuthorResources :: [Int] -> I.WithConfig (TD.Document TA.Author)
deleteAuthorResources = fmap (TM.docMeta . fromIntegral) . SA.deleteAuthors
