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


import qualified Control.Monad.Trans as MonadT

import qualified Web.Scotty.Trans as Scotty

import qualified Type.Tag as TT
import qualified Resource.Tag as RT
import qualified Init as I
import qualified Controller.Utils as CU



-- CREATE

post :: I.ActionA
post = do
  tag' :: TT.TagInsert <- CU.extractData TT.validTagInsertObject
  tagResource <- MonadT.lift $ RT.createTagResource tag'
  Scotty.json tagResource

postBatch :: I.ActionA
postBatch = do
  tags :: [TT.TagInsert] <- CU.extractData TT.validTagInsertObject
  tagResources <- MonadT.lift $ RT.createTagResources tags
  Scotty.json tagResources



-- RETRIVE

getBatch :: I.ActionA
getBatch = do
  qparams <- Scotty.params
  ar <- MonadT.lift . RT.getTagResources $ CU.cursorPagination qparams
  Scotty.json ar


get :: I.ActionA
get = do
  tagId' <- Scotty.param "id"
  tagResource <- MonadT.lift $ RT.getTagResource tagId'
  either Scotty.json Scotty.json tagResource



-- UPDATE

put :: I.ActionA
put = do
  tagId' :: Int <- Scotty.param "id"
  tag' :: TT.TagInsert <- CU.extractData TT.validTagInsertObject
  let
    tag'' = TT.mkTagPut tagId' (TT.tagName tag') (TT.tagGenre tag')

  tagResource <- MonadT.lift $ RT.updateTagResource tag''
  either Scotty.json Scotty.json tagResource


putBatch :: I.ActionA
putBatch = do
  tags :: [TT.TagPut] <- CU.extractData TT.validTagPutObject
  tagResources <- MonadT.lift $ RT.updateTagResources tags
  Scotty.json tagResources



-- DELETE

deleteBatch :: I.ActionA
deleteBatch = do
  tags :: [Int] <- CU.extractData undefined
  tagResources <- MonadT.lift $ RT.deleteTagResources tags
  Scotty.json tagResources


delete :: I.ActionA
delete = do
  tagId' <- Scotty.param "id"
  tagResource <- MonadT.lift $ RT.deleteTagResource tagId'
  either Scotty.json Scotty.json tagResource
