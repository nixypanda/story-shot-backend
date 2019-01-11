{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}


module Type.StoryAuthor
  ( StoryAuthor
  , StoryAuthorWrite
  , StoryAuthorRead
  , storyAuthorTable
  , mkStoryAuthor
  , authorID
  , authorColID
  , storyID
  , storyColID
  ) where


import qualified Data.Profunctor.Product.TH as ProductProfunctor
import qualified Opaleye                    as O



-- Strangely Polymorphic data type (Internal Use)

data StoryAuthor' id' authorID storyID =
  StoryAuthor
    { _storyAuthorID :: id'
    , _authorID      :: authorID
    , _storyID       :: storyID
    } deriving (Eq, Show)



-- Types that Will be used

type StoryAuthor      = StoryAuthor' Int                     Int             Int
type StoryAuthorRead  = StoryAuthor' (O.Column O.PGInt4)         (O.Column O.PGInt4) (O.Column O.PGInt4)
type StoryAuthorWrite = StoryAuthor' (Maybe (O.Column O.PGInt4)) (O.Column O.PGInt4) (O.Column O.PGInt4)



-- Magic

$(ProductProfunctor.makeAdaptorAndInstance "pStoryAuthor" ''StoryAuthor')



-- Opaleye table binding

storyAuthorTable :: O.Table StoryAuthorWrite StoryAuthorRead
storyAuthorTable = O.Table "story_authors" $
  pStoryAuthor
    StoryAuthor
      { _storyAuthorID = O.optional "id"
      , _authorID      = O.required "author_id"
      , _storyID       = O.required "story_id"
      }


mkStoryAuthor :: Int -> Int -> StoryAuthorWrite
mkStoryAuthor tid sid = StoryAuthor
  { _storyAuthorID = Nothing
  , _authorID = O.constant tid
  , _storyID = O.constant sid
  }


authorID :: StoryAuthor -> Int
authorID = _authorID


storyID :: StoryAuthor -> Int
storyID = _storyID


authorColID :: StoryAuthor' a (O.Column O.PGInt4) (O.Column O.PGInt4) -> O.Column O.PGInt4
authorColID = _authorID


storyColID :: StoryAuthor' a (O.Column O.PGInt4) (O.Column O.PGInt4) -> O.Column O.PGInt4
storyColID = _storyID
