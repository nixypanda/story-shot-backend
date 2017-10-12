{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Type.StoryTag
  ( StoryTag
  , StoryTagWrite
  , StoryTagRead
  , storyTagTable
  , mkStoryTag
  , tagID
  , tagColID
  , storyID
  , storyColID
  ) where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Opaleye
  ( Column
  , PGInt4
  , Table(Table)
  , required
  , optional
  , constant
  )


-- Strangely Polymorphic data type (Internal Use)

data StoryTag' id' tagID storyID =
  StoryTag
    { _storyTagID :: id'
    , _tagID :: tagID
    , _storyID :: storyID
    } deriving (Eq, Show)


-- Types that Will be used
type StoryTag      = StoryTag' Int                     Int             Int
type StoryTagRead  = StoryTag' (Column PGInt4)         (Column PGInt4) (Column PGInt4)
type StoryTagWrite = StoryTag' (Maybe (Column PGInt4)) (Column PGInt4) (Column PGInt4)


-- Magic
$(makeAdaptorAndInstance "pStoryTag" ''StoryTag')

-- Opaleye table binding
storyTagTable :: Table StoryTagWrite StoryTagRead
storyTagTable = Table "story_tags" $
  pStoryTag
    StoryTag
      { _storyTagID = optional "id"
      , _tagID = required "tag_id"
      , _storyID = required "story_id"
      }


mkStoryTag :: Int -> Int -> StoryTagWrite
mkStoryTag tid sid = StoryTag
  { _storyTagID = Nothing
  , _tagID = constant tid
  , _storyID = constant sid
  }

tagID :: StoryTag -> Int
tagID = _tagID

storyID :: StoryTag -> Int
storyID = _storyID

tagColID :: StoryTag' a (Column PGInt4) (Column PGInt4) -> Column PGInt4
tagColID = _tagID

storyColID :: StoryTag' a (Column PGInt4) (Column PGInt4) -> Column PGInt4
storyColID = _storyID
