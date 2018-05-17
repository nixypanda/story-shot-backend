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
import qualified Type.Meta as TM
import qualified Type.Doc as TD
import qualified Resource.Tag as RT
import qualified Init as I
import qualified Controller.Utils as CU



-- CREATE

post :: I.ActionA
post = do
  tagInsertObj :: TT.TagInsert <- CU.extractData TT.validTagInsertObject
  tag <- MonadT.lift $ RT.createTag tagInsertObj
  let tagResource = TM.indexDoc' tag
  Scotty.json tagResource


postBatch :: I.ActionA
postBatch = do
  tagInsertObjs :: [TT.TagInsert] <- CU.extractData TT.validTagInsertObject
  tags <- MonadT.lift $ RT.createTags tagInsertObjs
  let tagResources = TM.docMulti tags
  Scotty.json tagResources



-- RETRIVE

getBatch :: I.ActionA
getBatch = do
  qparams <- Scotty.params
  tags <- MonadT.lift . RT.getTags $ CU.cursorPagination qparams
  let tagResources = TM.docMulti tags
  Scotty.json tagResources


get :: I.ActionA
get = do
  tagId' <- Scotty.param "id"
  maybeTag <- MonadT.lift $ RT.getTag tagId'
  let tagResourceOrErrorDoc = TM.docOrError maybeTag
  either Scotty.json Scotty.json tagResourceOrErrorDoc



-- UPDATE

put :: I.ActionA
put = do
  tagId :: Int <- Scotty.param "id"
  tagInsertObj :: TT.TagInsert <- CU.extractData TT.validTagInsertObject
  let tag'' = TT.mkTagPut tagId (TT.tagName tagInsertObj)
  maybeUpdatedTag <- MonadT.lift $ RT.updateTag tag''
  let tagResource = TM.docOrError maybeUpdatedTag
  either Scotty.json Scotty.json tagResource


putBatch :: I.ActionA
putBatch = do
  tagPutObjs :: [TT.TagPut] <- CU.extractData TT.validTagPutObject
  updatedTags <- MonadT.lift $ RT.updateTags tagPutObjs
  let tagResources = TM.docMulti updatedTags
  Scotty.json tagResources



-- DELETE

deleteBatch :: I.ActionA
deleteBatch = do
  tagIds :: [Int] <- CU.extractData CU.deleteBatchExample
  totalDeleted <- MonadT.lift $ RT.deleteTags tagIds
  let tagMetaInfo :: TD.MaybeResource TT.Tag = TM.metaDocFromInt totalDeleted
  Scotty.json tagMetaInfo


delete :: I.ActionA
delete = do
  tagId <- Scotty.param "id"
  totalDeleted <- MonadT.lift $ RT.deleteTag tagId
  let tagMetaInfo :: TD.MaybeResource TT.Tag = TM.metaDocFromInt totalDeleted
  either Scotty.json Scotty.json tagMetaInfo
