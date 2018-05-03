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
import qualified Resource.Story as RS
import qualified Init as I
import qualified Controller.Utils as CU



-- CREATE

post :: I.ActionA
post = do
  includes <- CU.extractIncludes
  story' :: TS.StoryInsert <- CU.extractData TS.validStoryInsertObject
  storyResource <- MonadT.lift $ RS.createStoryResource includes story'
  either Scotty.json Scotty.json storyResource


postBatch :: I.ActionA
postBatch = do
  includes <- CU.extractIncludes
  stories :: [TS.StoryInsert] <- CU.extractData TS.validStoryInsertObject
  storyResources <- MonadT.lift $ RS.createStoryResources includes stories
  either Scotty.json Scotty.json storyResources



-- RETRIVE

getBatch :: I.ActionA
getBatch = do
  qparams <- Scotty.params
  includes <- CU.extractIncludes
  storyResources <- MonadT.lift $ RS.getStoryResources (CU.cursorPagination qparams) includes
  either Scotty.json Scotty.json storyResources


get :: I.ActionA
get = do
  storyId' <- Scotty.param "id"
  includes <- CU.extractIncludes
  storyResource <- MonadT.lift $ RS.getStoryResource storyId' includes
  either Scotty.json Scotty.json storyResource


getRandom :: I.ActionA
getRandom = do
  includes <- CU.extractIncludes
  storyResource <- MonadT.lift $ RS.getRandomStoryResource includes
  either Scotty.json Scotty.json storyResource



-- UPDATE

put :: I.ActionA
put = do
  story' :: TS.StoryPut <- CU.extractData TS.validStoryPutObject
  storyResource <- MonadT.lift $ RS.updateStoryResource story'
  either Scotty.json Scotty.json storyResource


putBatch :: I.ActionA
putBatch = do
  stories :: [TS.StoryPut] <- CU.extractData TS.validStoryPutObject
  storyResources <- MonadT.lift $ RS.updateStoryResources stories
  Scotty.json storyResources



-- DELETE

deleteBatch :: I.ActionA
deleteBatch = do
  stories :: [Int] <- CU.extractData undefined
  storyResources <- MonadT.lift $ RS.deleteStoryResources stories
  Scotty.json storyResources


delete :: I.ActionA
delete = do
  storyId' <- Scotty.param "id"
  storyResource <- MonadT.lift $ RS.deleteStoryResource storyId'
  either Scotty.json Scotty.json storyResource
