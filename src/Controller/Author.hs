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

import Control.Monad.Trans (lift)

import Data.Aeson (Value)
import Web.Scotty.Trans
  ( json
  , jsonData
  , rescue
  , param
  , params
  )

import Type.Author
import Resource.Author
import Init (ActionA)
import Controller.Basic (invalidPayload)
import Controller.Utils (cursorPagination)


-- CREATE

post :: ActionA
post = do
  author' :: AuthorInsert <- jsonData `rescue` invalidPayload validAuthorInsertObject
  authorResource <- lift $ createAuthorResource author'
  json authorResource


postBatch :: ActionA
postBatch = do
  authors :: [AuthorInsert] <- jsonData `rescue` invalidPayload validAuthorInsertObject
  authorResources <- lift $ createAuthorResources authors
  json authorResources


-- RETRIVE

getBatch :: ActionA
getBatch = do
  qparams <- params
  ar <- lift . getAuthorResources $ cursorPagination qparams
  json ar


get :: ActionA
get = do
  authorId' <- param "id"
  authorResource <- lift $ getAuthorResource authorId'
  either json json authorResource



-- UPDATE

put :: ActionA
put = do
  authorId' :: Int <- param "id"
  author' :: AuthorInsert <- jsonData `rescue` invalidPayload validAuthorInsertObject
  let
    author'' = mkAuthorPut authorId' (authorName author')

  authorResource <- lift $ updateAuthorResource author''
  either json json authorResource


putBatch :: ActionA
putBatch = do
  authors :: [AuthorPut] <- jsonData `rescue` invalidPayload validAuthorPutObject
  authorResources <- lift $ updateAuthorResources authors
  json authorResources



-- DELETE

deleteBatch :: ActionA
deleteBatch = do
  authors :: [Int] <- jsonData `rescue` invalidPayload deleteBatchExample
  authorResources <- lift $ deleteAuthorResources authors
  json authorResources


delete :: ActionA
delete = do
  authorId' <- param "id"
  authorResource <- lift $ deleteAuthorResource authorId'
  either json json authorResource


-- HELPERS

deleteBatchExample :: Value
deleteBatchExample = undefined
