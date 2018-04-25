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

createAuthorResource :: TA.AuthorInsert -> I.AppT (TD.Doc TA.Author)
createAuthorResource = fmap TM.indexDoc' . SA.createAuthor


createAuthorResources :: [TA.AuthorInsert] -> I.AppT (TD.Doc TA.Author)
createAuthorResources = fmap TM.docMulti . SA.createAuthors



-- RETRIVE

getAuthorResources :: TP.CursorParam -> I.AppT (TD.Doc TA.Author)
getAuthorResources = fmap TM.docMulti . SA.getAuthors


getAuthorResource :: Int -> I.AppT (TD.MaybeResource TA.Author)
getAuthorResource = fmap TM.docOrError . SA.getAuthor



-- UPDATE

updateAuthorResource :: TA.AuthorPut -> I.AppT (TD.MaybeResource TA.Author)
updateAuthorResource = fmap TM.docOrError . SA.updateAuthor


updateAuthorResources :: [TA.AuthorPut] -> I.AppT (TD.Doc TA.Author)
updateAuthorResources = fmap TM.docMulti . SA.updateAuthors



-- DELETE

deleteAuthorResource :: Int -> I.AppT (TD.MaybeResource TA.Author)
deleteAuthorResource = fmap TM.docMetaOrError . SA.deleteAuthor


deleteAuthorResources :: [Int] -> I.AppT (TD.Doc TA.Author)
deleteAuthorResources = fmap (TM.docMeta . fromIntegral) . SA.deleteAuthors
