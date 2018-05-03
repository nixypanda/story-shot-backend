{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


module Resource.Story
  ( getStoryResources
  , getStoryResource
  , getRandomStoryResource
  , createStoryResources
  , createStoryResource
  , updateStoryResources
  , updateStoryResource
  , deleteStoryResources
  , deleteStoryResource
  ) where


import qualified Control.Monad.IO.Class as MIO
import qualified Data.Map as M
import qualified Data.Maybe as DM

import qualified Data.Text as Text
import qualified Data.Random as Rand

import qualified Init as I
import qualified Class.Includes as CI
import qualified Type.Or as Or
import qualified Type.Doc as TD
import qualified Type.Pagination as TP
import qualified Type.AppError as TAe
import qualified Type.Story as TS
import qualified Type.Author as TA
import qualified Type.Tag as TT
import qualified Type.Meta as TM
import qualified Storage.Story as SS
import qualified Storage.Author as SA
import qualified Library.Link as LL



-- CREATE

createStoryResource :: Either TAe.ClientError [StoryIncludes] -> TS.StoryInsert -> I.AppT (TD.MaybeResource TS.Story)
createStoryResource (Left e) _ = return . Left $ TAe.docError e
createStoryResource (Right includes) si = do
  pgstory <- SS.createStory si
  _fromMPGStory includes (Just pgstory)


createStoryResources :: Either TAe.ClientError [StoryIncludes] -> [TS.StoryInsert] -> I.AppT (TD.MaybeResource TS.Story)
createStoryResources (Left e) _ = return . Left $ TAe.docError e
createStoryResources (Right includes) sis =
  SS.createStories sis >>= fmap (Right . TM.docMulti) . _fromPGStories includes



-- RETRIVE

getStoryResources :: TP.CursorParam -> Either TAe.ClientError [StoryIncludes] -> I.AppT (TD.MaybeResource TS.Story)
getStoryResources _ (Left e) = return . Left $ TAe.docError e
getStoryResources cur (Right includes) =
  SS.getStories cur >>= fmap (Right . TM.docMulti) . _fromPGStories includes


getStoryResource :: Int -> Either TAe.ClientError [StoryIncludes] -> I.AppT (TD.MaybeResource TS.Story)
getStoryResource _ (Left e) = return $ Left $ TAe.docError e
getStoryResource sid (Right includes) = do
  mstory <- SS.getStory sid
  _fromMPGStory includes mstory


getRandomStoryResource :: Either TAe.ClientError [StoryIncludes] -> I.AppT (TD.MaybeResource TS.Story)
getRandomStoryResource (Left e) = return $ Left $ TAe.docError e
getRandomStoryResource (Right includes) = do
  storyIDs <- SS.getStoryIDs
  case storyIDs of
    [] -> return . Left . TAe.docError $ TAe.ResourceNotFound
    xs -> do
      randomStoryIndex <- MIO.liftIO $ Rand.sample $ Rand.randomElement xs
      mstory <- SS.getStory randomStoryIndex
      _fromMPGStory includes mstory


_fromMPGStory :: [StoryIncludes] -> Maybe TS.PGStory -> I.AppT (TD.MaybeResource TS.Story)
_fromMPGStory includes mstory =
  case mstory of
    Nothing      -> return . Left . TAe.docError $ TAe.ResourceNotFound
    Just pgstory -> do
      mlstory <- _fromPGStory includes pgstory
      case mlstory of
        Nothing -> return $ Left $ TAe.docError TAe.ResourceNotFound
        Just lstory -> return $ Right $ TM.indexDoc' lstory


_fromPGStory :: [StoryIncludes] -> TS.PGStory -> I.AppT (Maybe TS.Story)
_fromPGStory includes pgstory =
  LL.fromPG
    TS.mkLinkedStoryResource
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
      TS.mkLinkedStoryResource
      [(IAuthor, SA.getMultiAuthors, TA.mkAuthorS, storyAuthorIDMap)]
      [(ITags, SS.getTagsForStories, SS.getTagIDsForStories, fmap TS.pgStoryID stories)]
      includes
      stories



-- UPDATE

updateStoryResource :: TS.StoryPut -> I.AppT (TD.MaybeResource TS.Story)
updateStoryResource = fmap TM.docOrError . SS.updateStory


updateStoryResources :: [TS.StoryPut] -> I.AppT (TD.Doc TS.Story)
updateStoryResources = fmap TM.docMulti . SS.updateStories



-- DELETE

deleteStoryResource :: Int -> I.AppT (TD.MaybeResource TS.Story)
deleteStoryResource = fmap TM.docMetaOrError . SS.deleteStory


deleteStoryResources :: [Int] -> I.AppT (TD.Doc TS.Story)
deleteStoryResources = fmap (TM.docMeta . fromIntegral) . SS.deleteStories


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
