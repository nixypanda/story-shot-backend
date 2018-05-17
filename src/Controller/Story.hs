{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Controller.Story
  ( post
  , postBatch
  , getBatch
  , get
  , getRandom
  , putBatch
  , put
  , deleteBatch
  , delete
  ) where


import qualified Control.Monad.Trans as MonadT

import qualified Web.Scotty.Trans as Scotty

import qualified Type.Story as TS
import qualified Type.Meta as TM
import qualified Type.Doc as TD
import qualified Resource.Story as RS
import qualified Init as I
import qualified Controller.Utils as CU


-- CREATE

post :: I.ActionA
post = do
  includes <- CU.extractIncludes
  storyInsertObj :: TS.StoryInsert <- CU.extractData TS.validStoryInsertObject
  storyResource <- CU.executeAction includes (\incs ->
    fmap TM.docOrError . MonadT.lift $ RS.createStory incs storyInsertObj)
  either Scotty.json Scotty.json storyResource


postBatch :: I.ActionA
postBatch = do
  includes <- CU.extractIncludes
  storyInsertObjs :: [TS.StoryInsert] <- CU.extractData TS.validStoryInsertObject
  storyResources :: TD.MaybeResource TS.Story <- CU.executeAction includes (\incs ->
    fmap (Right . TM.docMulti) . MonadT.lift $ RS.createStories incs storyInsertObjs)
  either Scotty.json Scotty.json storyResources



-- RETRIVE

getBatch :: I.ActionA
getBatch = do
  qparams <- Scotty.params
  includes <- CU.extractIncludes
  storyResources :: TD.MaybeResource TS.Story <- CU.executeAction includes (\incs ->
    fmap (Right . TM.docMulti) . MonadT.lift $ RS.getStories (CU.cursorPagination qparams) incs)
  either Scotty.json Scotty.json storyResources


get :: I.ActionA
get = do
  storyId' <- Scotty.param "id"
  includes <- CU.extractIncludes
  storyResource :: TD.MaybeResource TS.Story <- CU.executeAction includes (\incs ->
    fmap TM.docOrError . MonadT.lift $ RS.getStory storyId' incs)
  either Scotty.json Scotty.json storyResource


getRandom :: I.ActionA
getRandom = do
  includes <- CU.extractIncludes
  storyResource :: TD.MaybeResource TS.Story <- CU.executeAction includes (\incs ->
    fmap TM.docOrError . MonadT.lift $ RS.getRandomStory incs)
  either Scotty.json Scotty.json storyResource



-- UPDATE

put :: I.ActionA
put = do
  storyId :: Int <- Scotty.param "id"
  storyInsertObj :: TS.StoryPut' <- CU.extractData TS.validStoryInsertObject
  let storyPutObj = TS.mkStoryPut storyId storyInsertObj
  maybeUpdatedStory <- MonadT.lift $ RS.updateStory storyPutObj
  let storyResource = TM.docOrError maybeUpdatedStory
  either Scotty.json Scotty.json storyResource


putBatch :: I.ActionA
putBatch = do
  storyPutObjs :: [TS.StoryPut] <- CU.extractData TS.validStoryPutObject
  updatedStories <- MonadT.lift $ RS.updateStories storyPutObjs
  let storyResources = TM.docMulti updatedStories
  Scotty.json storyResources



-- DELETE

deleteBatch :: I.ActionA
deleteBatch = do
  storyIds :: [Int] <- CU.extractData CU.deleteBatchExample
  totalDeleted <- MonadT.lift $ RS.deleteStories storyIds
  let storyMetaInfo :: TD.MaybeResource TS.Story = TM.metaDocFromInt totalDeleted
  Scotty.json storyMetaInfo


delete :: I.ActionA
delete = do
  storyId <- Scotty.param "id"
  totalDeleted <- MonadT.lift $ RS.deleteStory storyId
  let storyMetaInfo :: TD.MaybeResource TS.Story = TM.metaDocFromInt totalDeleted
  either Scotty.json Scotty.json storyMetaInfo
