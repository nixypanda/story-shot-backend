{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Storage.Story
  ( createStories
  , createStory
  , getStories
  , getStory
  , getTagsForStory
  , getTagIDsForStory
  , getTagsForStories
  , getTagIDsForStories
  , getRandomStory
  , updateStories
  , updateStory
  , deleteStories
  , deleteStory
  ) where


import Data.Int (Int64)
import Control.Arrow (returnA)
import Data.Map (Map, fromList, fromListWith, lookup, (!))
import Data.Maybe (fromJust, fromMaybe, listToMaybe)

import Opaleye
  ( Query
  , QueryArr
  , Column
  , PGInt4
  , asc
  , in_
  , constant
  , limit
  , orderBy
  , restrict
  , queryTable
  , (.==)
  , (.>)
  )

import Type.Story
import Type.Author
import Type.StoryTag
import Type.Tag
import Type.Pagination
import Storage.Utils
import Storage.Author
import Storage.Tag
import Init (WithConfig)



-- CREATE

createStories :: [StoryInsert] -> WithConfig [PGStory]
createStories stories = do
  stories' <- runDBInsertR storyTable (map mkStoryWrite stories) id
  let
    storyTags = concat $ zipWith (\sr si -> map (mkStoryTag $ pgStoryID sr) (tagIDs si)) stories' stories

  _ <- runDBInsert storyTagTable storyTags
  return stories'


createStory :: StoryInsert -> WithConfig PGStory
createStory =
  fmap head . createStories . return



-- RETRIVE

getRandomStory :: WithConfig (Maybe PGStory)
getRandomStory = listToMaybe <$> runDB randomStory

getStory :: Int -> WithConfig (Maybe PGStory)
getStory = fmap listToMaybe . runDB . singleStory

getStories :: CursorParam -> WithConfig [PGStory]
getStories cur = runDB (cursorPaginatedStoryQuery cur)

getTagIDsForStory :: Int -> WithConfig [Int]
getTagIDsForStory sid = runDB (tagIDsForStory sid)

getTagsForStory :: Int -> WithConfig [Tag]
getTagsForStory sid = runDB (tagsForStory sid) 

_toMap :: (Ord a) => [(a, b)] -> Map a [b]
_toMap xs = fromListWith (++) [(k, [v]) | (k, v) <- xs]

getTagIDsForStories :: [Int] -> WithConfig (Map Int [TagS])
getTagIDsForStories sid = fmap (map mkTagS) . _toMap <$> runDB (tagIDsForStories sid)

getTagsForStories :: [Int] -> WithConfig (Map Int [Tag])
getTagsForStories sid = _toMap <$> runDB (tagsForStories sid) 



-- UPDATE

updateStory :: StoryPut -> WithConfig (Maybe Story)
updateStory = fmap listToMaybe . updateStories . return


-- TODO: FIX ME
updateStories :: [StoryPut] -> WithConfig [Story]
updateStories stories = undefined
  -- let
  --   storyIDs = map Type.Story.storyID stories
  --   storyMap = fromList $ zip storyIDs stories
  --
  --   getID :: StoryRead -> Int
  --   getID sr = read $ show $ Type.Story.storyColID sr
  --
  --   updateF storyRead = mkStoryWrite' (storyMap ! getID storyRead) storyRead
  --   predicate storyRead = map (constant . Type.Story.storyID) stories `in_` Type.Story.storyColID storyRead
  --
  -- runDBUpdateR storyTable updateF predicate id >>= _fromPGStories



-- DELETE

deleteStory :: Int -> WithConfig Int64
deleteStory = deleteStories . return


deleteStories :: [Int] -> WithConfig Int64
deleteStories ids =
  let
    storyTagP st = map constant ids `in_` Type.StoryTag.storyColID st
    storyP sic = map constant ids `in_` Type.Story.storyColID sic
  in
    runDBDelete storyTagTable storyTagP >> runDBDelete storyTable storyP


-- QUERIES

storyQuery :: Query StoryRead
storyQuery = queryTable storyTable


cursorPaginatedStoryQuery :: CursorParam -> Query StoryRead
cursorPaginatedStoryQuery CursorParam{..} = limit sizeCursor $ proc () -> do
  row <- storyQuery -< ()
  restrict -< Type.Story.storyColID row .> constant nextCursor

  returnA -< row


storyTagsQuery :: Query StoryTagRead
storyTagsQuery = queryTable storyTagTable


-- TODO: How would you do it?
-- HACK: Not Random at the moment
randomStory :: Query StoryRead
randomStory = limit 1 . orderBy (asc Type.Story.storyColID) $ storyQuery


singleStory :: Int -> Query StoryRead
singleStory idA = proc () -> do
  row <- storyQuery -< ()
  restrict -< Type.Story.storyColID row .== constant idA
  returnA -< row


-- Get Tags

storyTagJoin :: QueryArr TagRead (Column PGInt4)
storyTagJoin = proc tag -> do
  storyTag <- storyTagsQuery -< ()
  restrict -< Type.Tag.tagColID tag .== Type.StoryTag.tagColID storyTag
  returnA -< Type.StoryTag.storyColID storyTag

tagsForStories :: [Int] -> Query (Column PGInt4, TagRead)
tagsForStories storyIDs = proc () -> do
  tag <- tagQuery -< ()
  sid <- storyTagJoin -< tag
  restrict -< map constant storyIDs `in_` sid
  returnA -< (sid, tag)

tagIDsForStories :: [Int] -> Query (Column PGInt4, Column PGInt4)
tagIDsForStories storyIDs = proc () -> do
  row <- storyTagsQuery -< ()
  restrict -< map constant storyIDs `in_` Type.StoryTag.storyColID row
  returnA -< (Type.StoryTag.storyColID row, Type.StoryTag.tagColID row)

tagIDsForStory :: Int -> Query (Column PGInt4)
tagIDsForStory sid = proc () -> do
  row <- storyTagsQuery -< ()
  restrict -< Type.StoryTag.storyColID row .== constant sid
  returnA -< Type.StoryTag.tagColID row

tagsForStory :: Int -> Query TagRead
tagsForStory sid' = proc () -> do
  tag <- tagQuery -< ()
  stid <- storyTagJoin -< tag
  restrict -< constant sid' .== stid
  returnA -< tag



-- Get Authors

storyAuthorJoin :: QueryArr AuthorRead (Column PGInt4)
storyAuthorJoin = proc author' -> do
  story <- storyQuery -< ()
  restrict -< Type.Author.authorColID author' .== Type.Story.authorColID story
  returnA -< Type.Story.storyColID story
  

authorsForStories :: [Int] -> Query (Column PGInt4, AuthorRead)
authorsForStories storyIDs = proc () -> do
  author' <- authorQuery -< ()
  sid <- storyAuthorJoin -< author'
  restrict -< map constant storyIDs `in_` sid
  returnA -< (sid, author')
