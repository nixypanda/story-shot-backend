{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
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


import qualified Control.Monad.Trans as CMT

import qualified Data.Aeson as DA
import qualified Web.Scotty.Trans as WST

import qualified Init as I
import qualified Type.Author as TA
import qualified Resource.Author as RA
import qualified Controller.Utils as CU



-- CREATE

post :: I.ActionA
post = do
  author' :: TA.AuthorInsert <- CU.extractData TA.validAuthorInsertObject
  authorResource <- CMT.lift $ RA.createAuthorResource author'
  WST.json authorResource


postBatch :: I.ActionA
postBatch = do
  authors :: [TA.AuthorInsert] <- CU.extractData TA.validAuthorInsertObject
  authorResources <- CMT.lift $ RA.createAuthorResources authors
  WST.json authorResources



-- RETRIVE

getBatch :: I.ActionA
getBatch = do
  qparams <- WST.params
  ar <- CMT.lift . RA.getAuthorResources $ CU.cursorPagination qparams
  WST.json ar


get :: I.ActionA
get = do
  authorId' <- WST.param "id"
  authorResource <- CMT.lift $ RA.getAuthorResource authorId'
  either WST.json WST.json authorResource



-- UPDATE

put :: I.ActionA
put = do
  authorId' :: Int <- WST.param "id"
  author' :: TA.AuthorInsert <- CU.extractData TA.validAuthorInsertObject
  let
    author'' = TA.mkAuthorPut authorId' (TA.authorName author')

  authorResource <- CMT.lift $ RA.updateAuthorResource author''
  either WST.json WST.json authorResource


putBatch :: I.ActionA
putBatch = do
  authors :: [TA.AuthorPut] <- CU.extractData TA.validAuthorPutObject
  authorResources <- CMT.lift $ RA.updateAuthorResources authors
  WST.json authorResources



-- DELETE

deleteBatch :: I.ActionA
deleteBatch = do
  authors :: [Int] <- CU.extractData deleteBatchExample
  authorResources <- CMT.lift $ RA.deleteAuthorResources authors
  WST.json authorResources


delete :: I.ActionA
delete = do
  authorId' <- WST.param "id"
  authorResource <- CMT.lift $ RA.deleteAuthorResource authorId'
  either WST.json WST.json authorResource



-- HELPERS

deleteBatchExample :: DA.Value
deleteBatchExample = undefined
