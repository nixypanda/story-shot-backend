{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Controller.Tag
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
import Web.Scotty.Trans
  ( json
  , jsonData
  , rescue
  , param
  , params
  )

import Type.Tag
import Resource.Tag
import Init (ActionA)
import Controller.Basic (invalidPayload)
import Controller.Utils (cursorPagination)


-- CREATE

post :: ActionA
post = do
  tag' :: TagInsert <- jsonData `rescue` invalidPayload validTagInsertObject
  tagResource <- lift $ createTagResource tag'
  json tagResource

postBatch :: ActionA
postBatch = do
  tags :: [TagInsert] <- jsonData `rescue` invalidPayload validTagInsertObject
  tagResources <- lift $ createTagResources tags
  json tagResources


-- RETRIVE

getBatch :: ActionA
getBatch = do
  qparams <- params
  ar <- lift . getTagResources $ cursorPagination qparams
  json ar


get :: ActionA
get = do
  tagId' <- param "id"
  tagResource <- lift $ getTagResource tagId'
  either json json tagResource



-- UPDATE

put :: ActionA
put = do
  tagId' :: Int <- param "id"
  tag' :: TagInsert <- jsonData `rescue` invalidPayload validTagInsertObject
  let
    tag'' = mkTagPut tagId' (tagName tag') (tagGenre tag')

  tagResource <- lift $ updateTagResource tag''
  either json json tagResource


putBatch :: ActionA
putBatch = do
  tags :: [TagPut] <- jsonData `rescue` invalidPayload validTagPutObject
  tagResources <- lift $ updateTagResources tags
  json tagResources



-- DELETE

deleteBatch :: ActionA
deleteBatch = do
  tags :: [Int] <- jsonData `rescue` invalidPayload undefined
  tagResources <- lift $ deleteTagResources tags
  json tagResources


delete :: ActionA
delete = do
  tagId' <- param "id"
  tagResource <- lift $ deleteTagResource tagId'
  either json json tagResource
