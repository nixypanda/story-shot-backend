{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


module Resource.Story
  ( getStories
  , getStory
  , getRandomStory
  , createStories
  , createStory
  , updateStories
  , updateStory
  , deleteStories
  , deleteStory
  ) where


import qualified Control.Monad.IO.Class as MIO
import qualified Data.Map as M

import qualified Data.Text as Text
import qualified Data.Random as Rand

import qualified Init as I
import qualified Class.Includes as CI
import qualified Type.Pagination as TP
import qualified Type.AppError as TAe
import qualified Type.Story as TS
import qualified Type.Author as TA
import qualified Type.Tag as TT
import qualified Storage.Story as SS
import qualified Storage.Author as SA
import qualified Library.Link as LL



-- CREATE

createStory :: [StoryIncludes] -> TS.StoryInsert -> I.AppT (Maybe TS.Story)
createStory includes si = do
  pgstory <- SS.createStory si
  _fromPGStory includes pgstory


createStories :: [StoryIncludes] -> [TS.StoryInsert] -> I.AppT [TS.Story]
createStories includes sis =
  SS.createStories sis >>= _fromPGStories includes



-- RETRIVE

getStories :: TP.CursorParam -> [StoryIncludes] -> I.AppT [TS.Story]
getStories cur includes =
  SS.getStories cur >>= _fromPGStories includes


getStory :: Int -> [StoryIncludes] -> I.AppT (Maybe TS.Story)
getStory sid includes = do
  mstory <- SS.getStory sid
  _fromMPGStory includes mstory


getRandomStory :: [StoryIncludes] -> I.AppT (Maybe TS.Story)
getRandomStory includes = do
  storyIDs <- SS.getStoryIDs
  case storyIDs of
    [] -> return Nothing
    xs -> do
      randomStoryIndex <- MIO.liftIO $ Rand.sample $ Rand.randomElement xs
      mstory <- SS.getStory randomStoryIndex
      _fromMPGStory includes mstory


_fromMPGStory :: [StoryIncludes] -> Maybe TS.PGStory -> I.AppT (Maybe TS.Story)
_fromMPGStory includes mstory =
  case mstory of
    Nothing      -> return Nothing
    Just pgstory -> do
      mlstory <- _fromPGStory includes pgstory
      case mlstory of
        Nothing -> return Nothing
        Just lstory -> return $ Just lstory


_fromPGStory :: [StoryIncludes] -> TS.PGStory -> I.AppT (Maybe TS.Story)
_fromPGStory includes pgstory =
  LL.fromPG
    TS.mkLinkedStory
    [(IAuthor, SA.getAuthor, TA.mkAuthorS, TS.storyAuthorID pgstory)]
    [(ITags, SS.getTagsForStory, SS.getTagIDsForStory, TT.mkTagS, TS.pgStoryID pgstory)]
    includes
    pgstory


_fromPGStories :: [StoryIncludes] -> [TS.PGStory] -> I.AppT [TS.Story]
_fromPGStories includes stories =
  let
    storyIDs = map TS.pgStoryID stories
    authorIDs = map TS.storyAuthorID stories
    storyAuthorIDMap = M.fromList $ zip storyIDs authorIDs
  in
    LL.fromPGs
      TS.mkLinkedStory
      [(IAuthor, SA.getMultiAuthors, TA.mkAuthorS, storyAuthorIDMap)]
      [(ITags, SS.getTagsForStories, SS.getTagIDsForStories, fmap TS.pgStoryID stories)]
      includes
      stories



-- UPDATE

updateStory :: TS.StoryPut -> I.AppT (Maybe TS.Story)
updateStory = SS.updateStory


updateStories :: [TS.StoryPut] -> I.AppT [TS.Story]
updateStories = SS.updateStories



-- DELETE

deleteStory :: Int -> I.AppT Int
deleteStory = fmap fromIntegral . SS.deleteStory


deleteStories :: [Int] -> I.AppT Int
deleteStories = fmap fromIntegral . SS.deleteStories


-- Query Params Processing

data StoryIncludes
  = IAuthor
  | ITags
  deriving (Show, Eq)


instance CI.Includes StoryIncludes where
  getAll = [IAuthor, ITags]
  singles = [IAuthor]
  multiples = [ITags]

  fromString :: Text.Text -> Either TAe.ClientError StoryIncludes
  fromString "author" = Right IAuthor
  fromString "tag" = Right ITags
  fromString _ = Left TAe.InvalidQueryParams
