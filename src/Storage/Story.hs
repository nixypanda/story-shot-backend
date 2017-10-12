{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Storage.Story
  ( createStories
  , createStory
  , getStories
  , getStory
  , getRandomStory
  , updateStories
  , updateStory
  , deleteStories
  , deleteStory
  ) where


import Data.Int (Int64)
import Control.Arrow (returnA)
import Data.Map (fromList, fromListWith, lookup, (!))
import Data.Maybe (fromJust, fromMaybe, listToMaybe)

import Opaleye
  ( Query
  , QueryArr
  , Column
  , PGInt4
  , (.==)
  , in_
  , constant
  , restrict
  , queryTable
  )

import Type.Story
import Type.Author
import Type.StoryTag
import Type.Tag
import Storage.Utils
import Storage.Author
import Storage.Tag
import Init (WithConfig)



-- CREATE

createStories :: [StoryInsert] -> WithConfig [Story]
createStories stories = do
  stories' <- runDBInsertR storyTable (map mkStoryWrite stories) id
  let
    storyTags = concat $ zipWith (\sr si -> map (mkStoryTag $ pgStoryID sr) (tagIDs si)) stories' stories

  _ <- runDBInsert storyTagTable storyTags
  _fromPGStories stories'


createStory :: StoryInsert -> WithConfig Story
createStory =
  fmap head . createStories . return



-- RETRIVE

getRandomStory :: WithConfig Story
getRandomStory = runDB randomStory >>= fmap head . _fromPGStories


getStory :: Int -> WithConfig (Maybe Story)
getStory id' = runDB (singleStory id') >>= fmap listToMaybe . _fromPGStories


getStories :: WithConfig [Story]
getStories = runDB storyQuery >>= _fromPGStories



-- UPDATE

updateStory :: StoryPut -> WithConfig (Maybe Story)
updateStory = fmap listToMaybe . updateStories . return


-- TODO: FIX ME
updateStories :: [StoryPut] -> WithConfig [Story]
updateStories stories = do
  let
    storyIDs = map Type.Story.storyID stories
    storyMap = fromList $ zip storyIDs stories

    getID :: StoryRead -> Int
    getID sr = read $ show $ Type.Story.storyColID sr

    updateF storyRead = mkStoryWrite' (storyMap ! getID storyRead) storyRead
    predicate storyRead = map (constant . Type.Story.storyID) stories `in_` Type.Story.storyColID storyRead

  runDBUpdateR storyTable updateF predicate id >>= _fromPGStories



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



-- HELPERS

_linkAll :: [PGStory] -> [(Int, Author)] -> [(Int, Tag)] -> [Story]
_linkAll stories assocAuthors assocTags =
  let
    idTagMap = fromListWith (++) [(k, [v]) | (k, v) <- assocTags]
    idAuthorMap = fromList assocAuthors
    getTagsFor pgs = fromMaybe [] $ Data.Map.lookup (pgStoryID pgs) idTagMap
    getAuthorFor pgs = fromJust $ Data.Map.lookup (pgStoryID pgs) idAuthorMap
    getStory' sg = mkStoryFromDB sg (getAuthorFor sg) (getTagsFor sg)
 in
    map getStory' stories


_fromPGStories :: [PGStory] -> WithConfig [Story]
_fromPGStories stories = do
  let
    storyIDs = map pgStoryID stories

  storyTagMap :: [(Int, Tag)] <- runDB $ tagsForStories storyIDs
  authors :: [(Int, Author)] <- runDB $ authorsForStories storyIDs

  return $ _linkAll stories authors storyTagMap



-- QUERIES

storyQuery :: Query StoryRead
storyQuery = queryTable storyTable


storyTagsQuery :: Query StoryTagRead
storyTagsQuery = queryTable storyTagTable


-- TODO: How would you do it?
randomStory :: Query StoryRead
randomStory = undefined


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
