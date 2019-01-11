{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}


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


import qualified Data.Profunctor.Product.TH as ProductProfunctor
import qualified Opaleye                    as O



-- Strangely Polymorphic data type (Internal Use)

data StoryTag' id' tagID storyID =
  StoryTag
    { _storyTagID :: id'
    , _tagID      :: tagID
    , _storyID    :: storyID
    } deriving (Eq, Show)



-- Types that Will be used

type StoryTag      = StoryTag' Int                     Int             Int
type StoryTagRead  = StoryTag' (O.Column O.PGInt4)         (O.Column O.PGInt4) (O.Column O.PGInt4)
type StoryTagWrite = StoryTag' (Maybe (O.Column O.PGInt4)) (O.Column O.PGInt4) (O.Column O.PGInt4)



-- Magic

$(ProductProfunctor.makeAdaptorAndInstance "pStoryTag" ''StoryTag')



-- Opaleye table binding

storyTagTable :: O.Table StoryTagWrite StoryTagRead
storyTagTable = O.Table "story_tags" $
  pStoryTag
    StoryTag
      { _storyTagID = O.optional "id"
      , _tagID      = O.required "tag_id"
      , _storyID    = O.required "story_id"
      }


mkStoryTag :: Int -> Int -> StoryTagWrite
mkStoryTag tid sid = StoryTag
  { _storyTagID = Nothing
  , _tagID = O.constant tid
  , _storyID = O.constant sid
  }


tagID :: StoryTag -> Int
tagID = _tagID


storyID :: StoryTag -> Int
storyID = _storyID


tagColID :: StoryTag' a (O.Column O.PGInt4) (O.Column O.PGInt4) -> O.Column O.PGInt4
tagColID = _tagID


storyColID :: StoryTag' a (O.Column O.PGInt4) (O.Column O.PGInt4) -> O.Column O.PGInt4
storyColID = _storyID
