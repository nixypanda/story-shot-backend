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
  , getStoryIDs
  , getTagsForStory
  , getTagIDsForStory
  , getTagsForStories
  , getTagIDsForStories
  , updateStories
  , updateStory
  , deleteStories
  , deleteStory
  ) where


import qualified Data.Int as DI
import qualified Control.Arrow as Arrow
import qualified Data.Map as M
import qualified Data.Maybe as DM

import qualified Opaleye as O

import qualified Init as I
import qualified Type.Pagination as TP
import qualified Type.Story as TS
import qualified Type.StoryTag as TSt
import qualified Type.Tag as TT
import qualified Storage.Utils as SU
import qualified Storage.Tag as ST



-- CREATE

createStories :: [TS.StoryInsert] -> I.AppT [TS.PGStory]
createStories stories = do
  stories' <- SU.runDBInsertR TS.storyTable (map TS.mkStoryWrite stories) id
  let
    storyTags = concat $ zipWith (\sr si -> map (TSt.mkStoryTag $ TS.pgStoryID sr) (TS.tagIDs si)) stories' stories
  _ <- SU.runDBInsert TSt.storyTagTable storyTags
  return stories'


createStory :: TS.StoryInsert -> I.AppT TS.PGStory
createStory = fmap head . createStories . return



-- RETRIVE

getStory :: Int -> I.AppT (Maybe TS.PGStory)
getStory = fmap DM.listToMaybe . SU.runDB . singleStory


getStories :: TP.CursorParam -> I.AppT [TS.PGStory]
getStories cur = SU.runDB (cursorPaginatedStoryQuery cur)


getStoryIDs :: I.AppT [Int]
getStoryIDs = SU.runDB storyIDs


getTagIDsForStory :: Int -> I.AppT [Int]
getTagIDsForStory sid = SU.runDB (tagIDsForStory sid)


getTagsForStory :: Int -> I.AppT [TT.Tag]
getTagsForStory sid = SU.runDB (tagsForStory sid) 


_toMap :: (Ord a) => [(a, b)] -> M.Map a [b]
_toMap xs = M.fromListWith (++) [(k, [v]) | (k, v) <- xs]


getTagIDsForStories :: [Int] -> I.AppT (M.Map Int [TT.TagS])
getTagIDsForStories sid = fmap (map TT.mkTagS) . _toMap <$> SU.runDB (tagIDsForStories sid)


getTagsForStories :: [Int] -> I.AppT (M.Map Int [TT.Tag])
getTagsForStories sid = _toMap <$> SU.runDB (tagsForStories sid) 



-- UPDATE

updateStory :: TS.StoryPut -> I.AppT (Maybe TS.Story)
updateStory = fmap DM.listToMaybe . updateStories . return


-- TODO: FIX ME
updateStories :: [TS.StoryPut] -> I.AppT [TS.Story]
updateStories = undefined
  -- let
  --   storyIDs = map TS.storyID stories
  --   storyMap = fromList $ zip storyIDs stories
  --
  --   getID :: TS.StoryRead -> Int
  --   getID sr = read $ show $ TS.storyColID sr
  --
  --   updateF storyRead = TS.mkStoryWrite' (storyMap ! getID storyRead) storyRead
  --   predicate storyRead = map (O.constant . TS.storyID) stories `O.in_` TS.storyColID storyRead
  --
  -- SU.runDBUpdateR TS.storyTable updateF predicate id >>= _fromPGStories



-- DELETE

deleteStory :: Int -> I.AppT DI.Int64
deleteStory = deleteStories . return


deleteStories :: [Int] -> I.AppT DI.Int64
deleteStories ids =
  let
    storyTagP st = map O.constant ids `O.in_` TSt.storyColID st
    storyP sic = map O.constant ids `O.in_` TS.storyColID sic
  in
    SU.runDBDelete TSt.storyTagTable storyTagP >> SU.runDBDelete TS.storyTable storyP



-- QUERIES

storyQuery :: O.Query TS.StoryRead
storyQuery = O.queryTable TS.storyTable


storyIDs :: O.Query (O.Column O.PGInt4)
storyIDs = proc () -> do
  row <- storyQuery -< ()
  Arrow.returnA -< TS.storyColID row


cursorPaginatedStoryQuery :: TP.CursorParam -> O.Query TS.StoryRead
cursorPaginatedStoryQuery TP.CursorParam{..} = O.limit sizeCursor $ proc () -> do
  row <- storyQuery -< ()
  O.restrict -< TS.storyColID row O..> O.constant nextCursor
  Arrow.returnA -< row


storyTagsQuery :: O.Query TSt.StoryTagRead
storyTagsQuery = O.queryTable TSt.storyTagTable


singleStory :: Int -> O.Query TS.StoryRead
singleStory idA = proc () -> do
  row <- storyQuery -< ()
  O.restrict -< TS.storyColID row O..== O.constant idA
  Arrow.returnA -< row



-- Get TT.Tags

storyTagJoin :: O.QueryArr TT.TagRead (O.Column O.PGInt4)
storyTagJoin = proc tag -> do
  storyTag <- storyTagsQuery -< ()
  O.restrict -< TT.tagColID tag O..== TSt.tagColID storyTag
  Arrow.returnA -< TSt.storyColID storyTag


tagsForStories :: [Int] -> O.Query (O.Column O.PGInt4, TT.TagRead)
tagsForStories storyIDs' = proc () -> do
  tag <- ST.tagQuery -< ()
  sid <- storyTagJoin -< tag
  O.restrict -< map O.constant storyIDs' `O.in_` sid
  Arrow.returnA -< (sid, tag)


tagIDsForStories :: [Int] -> O.Query (O.Column O.PGInt4, O.Column O.PGInt4)
tagIDsForStories storyIDs' = proc () -> do
  row <- storyTagsQuery -< ()
  O.restrict -< map O.constant storyIDs' `O.in_` TSt.storyColID row
  Arrow.returnA -< (TSt.storyColID row, TSt.tagColID row)


tagIDsForStory :: Int -> O.Query (O.Column O.PGInt4)
tagIDsForStory sid = proc () -> do
  row <- storyTagsQuery -< ()
  O.restrict -< TSt.storyColID row O..== O.constant sid
  Arrow.returnA -< TSt.tagColID row


tagsForStory :: Int -> O.Query TT.TagRead
tagsForStory sid' = proc () -> do
  tag <- ST.tagQuery -< ()
  stid <- storyTagJoin -< tag
  O.restrict -< O.constant sid' O..== stid
  Arrow.returnA -< tag
