{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Controller.Story
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
  )

import Type.Story
import Resource.Story
import Init (ActionA)
import Controller.Basic (invalidPayload)


-- CREATE

post :: ActionA
post = do
  story' :: StoryInsert <- jsonData `rescue` invalidPayload validStoryInsertObject
  storyResource <- lift $ createStoryResource story'
  json storyResource

postBatch :: ActionA
postBatch = do
  stories :: [StoryInsert] <- jsonData `rescue` invalidPayload validStoryInsertObject
  storyResources <- lift $ createStoryResources stories
  json storyResources


-- RETRIVE

getBatch :: ActionA
getBatch = do
  ar <- lift getStoryResources
  json ar


get :: ActionA
get = do
  storyId' <- param "id"
  storyResource <- lift $ getStoryResource storyId'
  either json json storyResource



-- UPDATE

put :: ActionA
put = do
  storyId' :: Int <- param "id"
  story' :: StoryPut <- jsonData `rescue` invalidPayload validStoryPutObject

  storyResource <- lift $ updateStoryResource story'
  either json json storyResource


putBatch :: ActionA
putBatch = do
  stories :: [StoryPut] <- jsonData `rescue` invalidPayload validStoryPutObject
  storyResources <- lift $ updateStoryResources stories
  json storyResources



-- DELETE

deleteBatch :: ActionA
deleteBatch = do
  stories :: [Int] <- jsonData `rescue` invalidPayload undefined
  storyResources <- lift $ deleteStoryResources stories
  json storyResources


delete :: ActionA
delete = do
  storyId' <- param "id"
  storyResource <- lift $ deleteStoryResource storyId'
  either json json storyResource
