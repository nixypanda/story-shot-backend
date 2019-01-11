{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Controller.Author
  ( post
  , postBatch
  , getBatch
  , get
  , putBatch
  , put
  , deleteBatch
  , delete
  ) where


import qualified Control.Monad.Trans as MonadT

import qualified Web.Scotty.Trans    as Scotty

import qualified Controller.Utils    as CU
import qualified Init                as I
import qualified Resource.Author     as RA
import qualified Type.Author         as TA
import qualified Type.Doc            as TD
import qualified Type.Meta           as TM


import Controller.Types.Author
import Domain.Types.Author
import Storage.Repos.Authors (createAuthor, createAuthors)


-- CREATE

post :: I.ActionA
post = do
  authorInsertObj :: AuthorInsert <- CU.extractData validAuthorInsertObject
  author <- MonadT.lift $ createAuthor authorInsertObj
  let authorResource = TM.indexDoc' author
  Scotty.json authorResource


postBatch :: I.ActionA
postBatch = do
  authorInsertObjs :: [AuthorInsert] <- CU.extractData validAuthorInsertObject
  authors <- MonadT.lift $ createAuthors authorInsertObjs
  let authorResources = TM.docMulti authors
  Scotty.json authorResources



-- RETRIVE

getBatch :: I.ActionA
getBatch = do
  qparams <- Scotty.params
  authors <- MonadT.lift . RA.getAuthors $ CU.cursorPagination qparams
  let authorResources = TM.docMulti authors
  Scotty.json authorResources


get :: I.ActionA
get = do
  authorId' <- Scotty.param "id"
  maybeAuthor <- MonadT.lift $ RA.getAuthor authorId'
  let authorResource = TM.docOrError maybeAuthor
  either Scotty.json Scotty.json authorResource



-- UPDATE

put :: I.ActionA
put = do
  authorId :: Int <- Scotty.param "id"
  authorInsertObj :: TA.AuthorInsert <- CU.extractData TA.validAuthorInsertObject
  let authorPutObj = TA.mkAuthorPut authorId (TA.authorName authorInsertObj)
  maybeUpdatedAuthor <- MonadT.lift $ RA.updateAuthor authorPutObj
  let authorResource = TM.docOrError maybeUpdatedAuthor
  either Scotty.json Scotty.json authorResource


putBatch :: I.ActionA
putBatch = do
  authorPutObjs :: [TA.AuthorPut] <- CU.extractData TA.validAuthorPutObject
  updatedAuthors <- MonadT.lift $ RA.updateAuthors authorPutObjs
  let authorResources = TM.docMulti updatedAuthors
  Scotty.json authorResources



-- DELETE

deleteBatch :: I.ActionA
deleteBatch = do
  authorIds :: [Int] <- CU.extractData CU.deleteBatchExample
  totalDeleted <- MonadT.lift $ RA.deleteAuthors authorIds
  let authorMetaInfo :: TD.MaybeResource TA.Author = TM.metaDocFromInt totalDeleted
  Scotty.json authorMetaInfo


delete :: I.ActionA
delete = do
  authorId <- Scotty.param "id"
  totalDeleted <- MonadT.lift $ RA.deleteAuthor authorId
  let authorMetaInfo :: TD.MaybeResource TA.Author = TM.metaDocFromInt totalDeleted
  either Scotty.json Scotty.json authorMetaInfo
