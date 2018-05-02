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
import qualified Controller.Basic as CB
import qualified Controller.Utils as CU
import qualified Class.Includes as CI



-- CREATE

post :: I.ActionA
post = do
  includes <- (CI.fromCSV <$> Scotty.param "includes") `Scotty.rescue` (\_ -> return $ Right [])
  story' :: TS.StoryInsert <- Scotty.jsonData `Scotty.rescue` CB.invalidPayload TS.validStoryInsertObject
  storyResource <- MonadT.lift $ RS.createStoryResource includes story'
  either Scotty.json Scotty.json storyResource


postBatch :: I.ActionA
postBatch = do
  includes <- (CI.fromCSV <$> Scotty.param "includes") `Scotty.rescue` (\_ -> return $ Right [])
  stories :: [TS.StoryInsert] <- Scotty.jsonData `Scotty.rescue` CB.invalidPayload TS.validStoryInsertObject
  storyResources <- MonadT.lift $ RS.createStoryResources includes stories
  either Scotty.json Scotty.json storyResources



-- RETRIVE

getBatch :: I.ActionA
getBatch = do
  qparams <- Scotty.params
  includes <- (CI.fromCSV <$> Scotty.param "includes") `Scotty.rescue` (\_ -> return $ Right [])
  storyResources <- MonadT.lift $ RS.getStoryResources (CU.cursorPagination qparams) includes
  either Scotty.json Scotty.json storyResources


get :: I.ActionA
get = do
  storyId' <- Scotty.param "id"
  includes <- (CI.fromCSV <$> Scotty.param "includes") `Scotty.rescue` (\_ -> return $ Right [])
  storyResource <- MonadT.lift $ RS.getStoryResource storyId' includes
  either Scotty.json Scotty.json storyResource


getRandom :: I.ActionA
getRandom = do
  includes <- (CI.fromCSV <$> Scotty.param "includes") `Scotty.rescue` (\_ -> return $ Right [])
  storyResource <- MonadT.lift $ RS.getRandomStoryResource includes
  either Scotty.json Scotty.json storyResource



-- UPDATE

put :: I.ActionA
put = do
  story' :: TS.StoryPut <- Scotty.jsonData `Scotty.rescue` CB.invalidPayload TS.validStoryPutObject
  storyResource <- MonadT.lift $ RS.updateStoryResource story'
  either Scotty.json Scotty.json storyResource


putBatch :: I.ActionA
putBatch = do
  stories :: [TS.StoryPut] <- Scotty.jsonData `Scotty.rescue` CB.invalidPayload TS.validStoryPutObject
  storyResources <- MonadT.lift $ RS.updateStoryResources stories
  Scotty.json storyResources



-- DELETE

deleteBatch :: I.ActionA
deleteBatch = do
  stories :: [Int] <- Scotty.jsonData `Scotty.rescue` CB.invalidPayload undefined
  storyResources <- MonadT.lift $ RS.deleteStoryResources stories
  Scotty.json storyResources


delete :: I.ActionA
delete = do
  storyId' <- Scotty.param "id"
  storyResource <- MonadT.lift $ RS.deleteStoryResource storyId'
  either Scotty.json Scotty.json storyResource
