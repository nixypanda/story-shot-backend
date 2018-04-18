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

import Control.Monad.Trans (lift)

import Data.List.Split (splitOn)
import Web.Scotty.Trans
  ( json
  , jsonData
  , rescue
  , param
  , params
  )

import Type.Story
import Resource.Story
import Init (ActionA)
import Controller.Basic (invalidPayload)
import Controller.Utils (cursorPagination)
import Class.Includes


-- Query Includes Processing

-- CREATE

post :: ActionA
post = do
  includes <- (fromCSV <$> param "includes") `rescue` (\_ -> return $ Right [])
  story' :: StoryInsert <- jsonData `rescue` invalidPayload validStoryInsertObject
  storyResource <- lift $ createStoryResource includes story'
  either json json storyResource


postBatch :: ActionA
postBatch = do
  includes <- (fromCSV <$> param "includes") `rescue` (\_ -> return $ Right [])
  stories :: [StoryInsert] <- jsonData `rescue` invalidPayload validStoryInsertObject
  storyResources <- lift $ createStoryResources includes stories
  either json json storyResources


-- RETRIVE

getBatch :: ActionA
getBatch = do
  qparams <- params
  includes <- (fromCSV <$> param "includes") `rescue` (\_ -> return $ Right [])
  storyResources <- lift $ getStoryResources (cursorPagination qparams) includes
  either json json storyResources


get :: ActionA
get = do
  storyId' <- param "id"
  includes <- (fromCSV <$> param "includes") `rescue` (\_ -> return $ Right [])
  storyResource <- lift $ getStoryResource storyId' includes
  either json json storyResource


getRandom :: ActionA
getRandom = do
  includes <- (fromCSV <$> param "includes") `rescue` (\_ -> return $ Right [])
  storyResource <- lift $ getRandomStoryResource includes
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
