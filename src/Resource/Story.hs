{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Resource.Story
  ( getStoryResources
  , getStoryResource
  , createStoryResources
  , createStoryResource
  , updateStoryResources
  , updateStoryResource
  , deleteStoryResources
  , deleteStoryResource
  ) where

import Data.Int (Int64)
import GHC.Generics (Generic)

import Data.Aeson (ToJSON, FromJSON)
import Network.JSONApi
  ( Document
  , ErrorDocument(..)
  , MetaObject(..)
  , Links
  , Meta
  , mkMeta
  , mkLinks
  , mkDocument
  , singleton
  , mkDocument'
  )

import Storage.Story
import Type.Story
import Utils (toURL)
import Init (WithConfig)
import Exception.AppError (APIError, ClientError(..), toErrorDoc)


-- CREATE

createStoryResource :: StoryInsert -> WithConfig (Document Story)
createStoryResource =
  fmap indexDocument' . createStory


createStoryResources :: [StoryInsert] -> WithConfig (Document Story)
createStoryResources =
  fmap docMulti . createStories



-- RETRIVE

getStoryResources :: WithConfig (Document Story)
getStoryResources =
  docMulti <$> getStories


getStoryResource :: Int -> WithConfig (Either (ErrorDocument Story) (Document Story))
getStoryResource =
  fmap docOrError . getStory



-- UPDATE

updateStoryResource :: StoryPut -> WithConfig (Either (ErrorDocument Story) (Document Story))
updateStoryResource = 
  fmap docOrError . updateStory


updateStoryResources :: [StoryPut] -> WithConfig (Document Story)
updateStoryResources =
  fmap docMulti . updateStories



-- DELETE

deleteStoryResource :: Int -> WithConfig (Either (ErrorDocument Story) (Document Story))
deleteStoryResource =
    fmap docMetaOrError . deleteStory


deleteStoryResources :: [Int] -> WithConfig (Document Story)
deleteStoryResources =
    fmap (docMeta . fromIntegral) . deleteStories


-- HELPERS


-- JSON API Related

data StoryMetaData = StoryMetaData
  { count :: Int
  } deriving (Eq, Show, Generic)

instance MetaObject StoryMetaData where
  typeName _ = "storyCount"

instance ToJSON StoryMetaData
instance FromJSON StoryMetaData

-- Builds the Links data for the 'index' action
indexLinks :: Links
indexLinks = mkLinks [("self", selfLink)]
  where
    selfLink = toURL "/story"


-- Builds the Meta data for the 'index' action
indexMetaData :: [a] -> Meta
indexMetaData stories = mkMeta (StoryMetaData $ length stories)


-- Builds the repsonse Document for the 'index' action
indexDocument :: [Story] -> Links -> Meta -> Document Story
indexDocument stories links meta =
  mkDocument stories (Just links) (Just meta)


indexDocument' :: Story -> Document Story
indexDocument' story' =
  mkDocument' (singleton story') Nothing Nothing


docMulti :: [Story] -> Document Story
docMulti stories =
  indexDocument stories indexLinks $ indexMetaData stories


docMetaOrError :: Int64 -> Either (ErrorDocument Story) (Document Story)
docMetaOrError 0 = Left $ docError ResourceNotFound
docMetaOrError 1 = Right $ indexDocument [] indexLinks $ mkMeta $ StoryMetaData 1
docMetaOrError _ = error "Impossible"


docMeta :: Int -> Document Story
docMeta =
  indexDocument [] indexLinks . mkMeta . StoryMetaData


docError :: APIError e => e -> ErrorDocument a
docError e =
  ErrorDocument (toErrorDoc e) Nothing Nothing


docOrError :: Maybe Story -> Either (ErrorDocument a) (Document Story)
docOrError Nothing = Left $ docError ResourceNotFound
docOrError (Just at) = Right $ indexDocument' at
