{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Type.Story
  ( Story
  , PGStory
  , StoryInsert
  , StoryPut
  , StoryWrite
  , StoryRead
  , pgStoryID
  , storyTable
  , mkStoryWrite
  , mkStoryWrite'
  , mkStoryFromDB
  , storyID
  , storyColID
  , storyLinks
  , tagIDs
  , Type.Story.authorColID
  , validStoryPutObject
  , validStoryInsertObject
  ) where

import Data.Monoid ((<>))
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text, length, pack)
import Data.Aeson
  ( ToJSON(..)
  , FromJSON(..)
  , Value
  , object
  , withObject
  , (.=)
  , (.:)
  )
import Network.JSONApi
  ( Links
  , ResourcefulEntity (..)
  , Relationships
  , mkLinks
  , mkRelationship
  , mkRelationships
  )
import Opaleye
  ( Column
  , PGInt4
  , PGText
  , Table(Table)
  , PGTimestamptz
  , required
  , optional
  , constant
  )

import Type.Duration
import Type.Genre
import Type.Tag
import Type.Author
import Utils (toURL)
import Class.Versioned


data Story' storyID title duration author timesRead stars genre tags story createdAt updatedAt = Story
  { _storyID :: storyID
  , _title :: title
  , _duration :: duration
  , _author :: author
  , _timesRead :: timesRead
  , _stars :: stars
  , _genre :: genre
  , _tags :: tags
  , _story :: story
  , _createdAt :: createdAt
  , _updatedAt :: updatedAt
  } deriving (Eq, Show, Generic)

data PGStory' storyID title duration author timesRead stars genre story createdAt updatedAt = PGStory
  { _pgStoryID :: storyID
  , _pgTitle :: title
  , _pgDuration :: duration
  , _pgAuthor :: author
  , _pgTimesRead :: timesRead
  , _pgStars :: stars
  , _pgGenre :: genre
  , _pgStory :: story
  , _pgCreatedAt :: createdAt
  , _pgUpdatedAt :: updatedAt
  } deriving (Eq, Show, Generic)

type Story       = Story' Int Text         Duration Author      Int         Int         Genre         [Tag]         Text         UTCTime UTCTime
type StoryPut    = Story' Int (Maybe Text) ()       (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Genre) (Maybe [Int]) (Maybe Text) ()      ()
type StoryInsert = Story' ()  Text         ()       Int         ()          ()          Genre         [Int]         Text         ()      ()

type PGStory   = PGStory' Int Text Duration Int Int Int Genre Text UTCTime UTCTime
type StoryRead = PGStory'
  (Column PGInt4)
  (Column PGText)
  (Column PGText)
  (Column PGInt4)
  (Column PGInt4)
  (Column PGInt4)
  (Column PGText)
  (Column PGText)
  (Column PGTimestamptz)
  (Column PGTimestamptz)

type StoryWrite = PGStory'
  (Maybe (Column PGInt4))
  (Column PGText)
  (Column PGText)
  (Column PGInt4)
  (Column PGInt4)
  (Column PGInt4)
  (Column PGText)
  (Column PGText)
  (Maybe (Column PGTimestamptz))
  (Maybe (Column PGTimestamptz))


instance Versioned Story where
  createdAt = _createdAt
  updatedAt = _updatedAt

-- Magic
$(makeAdaptorAndInstance "pStory" ''PGStory')

-- Opaleye table binding
storyTable :: Table StoryWrite StoryRead
storyTable = Table "stories" $ pStory
  PGStory
    { _pgStoryID = optional "id"
    , _pgTitle = required "title"
    , _pgDuration = required "duration"
    , _pgAuthor = required "author"
    , _pgTimesRead = required "times_read"
    , _pgStars = required "stars"
    , _pgGenre = required "genre"
    , _pgStory = required "story"
    , _pgCreatedAt = optional "created_at"
    , _pgUpdatedAt = optional "updated_at"
    }


-- Some Helpers

durationFromStoryLen :: Text -> Duration
durationFromStoryLen s
  | Data.Text.length s < 500 = Short
  | Data.Text.length s < 5000 = Medium
  | otherwise = Long

mkStoryFromDB :: PGStory -> Author -> [Tag] -> Story
mkStoryFromDB PGStory{..} author' tags' = Story
  { _storyID = _pgStoryID
  , _title = _pgTitle
  , _duration = _pgDuration
  , _author = author'
  , _timesRead = _pgTimesRead
  , _stars = _pgStars
  , _genre = _pgGenre
  , _story = _pgStory
  , _tags = tags'
  , _createdAt = _pgCreatedAt
  , _updatedAt = _pgUpdatedAt
  }

mkStoryWrite :: StoryInsert -> StoryWrite
mkStoryWrite Story{..} = PGStory
  { _pgStoryID = Nothing
  , _pgTitle = constant _title
  , _pgDuration = constant $ durationFromStoryLen _story
  , _pgAuthor = constant _author
  , _pgTimesRead = constant (0 :: Int)
  , _pgStars = constant (0 :: Int)
  , _pgGenre = constant _genre
  , _pgStory = constant _story
  , _pgCreatedAt = Nothing
  , _pgUpdatedAt = Nothing
  }

mkStoryWrite' :: StoryPut -> StoryRead -> StoryWrite
mkStoryWrite' Story{..} PGStory{..} = PGStory
  { _pgStoryID = Just $ constant _storyID
  , _pgTitle = maybe _pgTitle constant _title
  , _pgDuration = maybe _pgDuration constant $ fmap durationFromStoryLen _story
  , _pgAuthor = maybe _pgAuthor constant _author
  , _pgTimesRead = maybe _pgTimesRead constant _timesRead
  , _pgStars = maybe _pgStars constant _stars
  , _pgGenre = maybe _pgGenre constant _genre
  , _pgStory = maybe _pgStory constant _story
  , _pgCreatedAt = Nothing
  , _pgUpdatedAt = Nothing
  }

storyID :: Story' Int b c d e f g h i j k -> Int
storyID = _storyID

tagIDs :: StoryInsert -> [Int]
tagIDs = _tags

pgStoryID :: PGStory' Int b c d e f g h i j -> Int
pgStoryID = _pgStoryID

storyColID :: PGStory' (Column PGInt4) b c d e f g h i j -> Column PGInt4
storyColID = _pgStoryID

authorColID :: PGStory' a b c (Column PGInt4) e f g h i j -> Column PGInt4
authorColID = _pgAuthor

author :: Story -> Author
author = _author

tags :: Story -> [Tag]
tags = _tags

-- JSON

instance ToJSON Story where
  toJSON Story{..} = object
    [ "id" .= _storyID
    , "title" .= _title
    , "duration" .= _duration
    , "read-count" .= _timesRead
    , "stars" .= _stars
    , "genre" .= _genre
    , "story" .= _story
    , "created-at" .= _createdAt
    , "updated-at" .= _updatedAt
    ]

instance FromJSON Story where
  parseJSON = withObject "story" $ \o -> Story
      <$> o .: "id"
      <*> o .: "title"
      <*> o .: "duration"
      <*> o .: "author"
      <*> o .: "read-count"
      <*> o .: "stars"
      <*> o .: "genre"
      <*> o .: "tags"
      <*> o .: "story"
      <*> o .: "created-at"
      <*> o .: "updated-at"

instance FromJSON StoryInsert where
  parseJSON = withObject "story" $ \o -> Story
      <$> pure ()
      <*> o .: "title"
      <*> pure ()
      <*> o .: "author"
      <*> pure ()
      <*> pure ()
      <*> o .: "genre"
      <*> o .: "tags"
      <*> o .: "story"
      <*> pure ()
      <*> pure ()


validStoryInsertObject :: Value
validStoryInsertObject = object
  [ "title" .= ("The title for the new story" :: Text)
  , "author" .= ("The author-id for the story (Should be in the DB)" :: Text)
  , "genre" .= ("One of " ++ show allGenres)
  , "tags" .= ("The list of tag-id's for this story" :: Text)
  , "story" .= ("The story itself" :: Text)
  ]

instance FromJSON StoryPut where
  parseJSON = withObject "story" $ \o -> Story
      <$> o .: "id"
      <*> o .: "title"
      <*> pure ()
      <*> o .: "author"
      <*> o .: "read-count"
      <*> o .: "stars"
      <*> o .: "genre"
      <*> o .: "tags"
      <*> o .: "story"
      <*> pure ()
      <*> pure ()


validStoryPutObject :: Value
validStoryPutObject = object
  [ "id" .= ("The id of the story which should be in the DB" :: Text)
  , "title" .= ("The new/old title for the story with the above id" :: Text)
  , "author" .= ("The new/old author-id for the story with the above id" :: Text)
  , "read-count" .= ("The new/old read-count for the story with the above id" :: Text)
  , "stars" .= ("The new/old stars count for the story with the above id" :: Text)
  , "genre" .= ("One of " ++ show allGenres)
  , "tags" .= ("The new/old list of tag-id's for this story" :: Text)
  , "story" .= ("The new/old value for the story" :: Text)
  ]

-- JSON API

instance ResourcefulEntity Story where
  resourceIdentifier = pack . show . storyID
  resourceType _ = "story"
  resourceLinks = Just . storyLinks
  resourceMetaData _ = Nothing
  resourceRelationships = storyRelations

storyLinks :: Story -> Links
storyLinks story = mkLinks [("self", selfLink)]
  where
    selfLink = toURL selfPath
    selfPath = "/story/" <> show (storyID story)


storyRelations :: Story -> Maybe Relationships
storyRelations story =
  let
    author' = author story
    tags' = tags story
    authorRelation = mkRelationship (Just $ authorIdentifier author') (Just $ authorLinks author')
    authorRelationships = fmap mkRelationships authorRelation
    tagRelations = mapM (\t -> mkRelationship (Just $ tagIdentifier t) (Just $ tagLinks t) ) tags'
    tagRelationships = fmap (fmap mkRelationships) tagRelations
    allRelationships = (:) <$> authorRelationships <*> tagRelationships
  in
    fmap (foldr1 (<>)) allRelationships
