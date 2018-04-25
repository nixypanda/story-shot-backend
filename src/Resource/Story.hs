{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
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

import qualified Data.Map as M
import qualified Data.Maybe as DM

import qualified Init as I
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


-- CREATE

createStoryResource :: Either TAe.ClientError [TS.StoryIncludes]
                    -> TS.StoryInsert
                    -> I.WithConfig (Either (TD.ErrorDocument TS.Story) (TD.Document TS.Story))
createStoryResource (Left e) _ = return $ Left $ TAe.docError e
createStoryResource (Right includes) si = SS.createStory si >>= fmap (Right . TM.indexDocument') . _fromPGStory includes


createStoryResources :: Either TAe.ClientError [TS.StoryIncludes]
                     -> [TS.StoryInsert]
                     -> I.WithConfig (Either (TD.ErrorDocument TS.Story) (TD.Document TS.Story))
createStoryResources (Left e) _ = return $ Left $ TAe.docError e
createStoryResources (Right includes) sis = SS.createStories sis >>= fmap (Right . TM.docMulti) . _fromPGStories includes



-- RETRIVE

getStoryResources :: TP.CursorParam
                  -> Either TAe.ClientError [TS.StoryIncludes]
                  -> I.WithConfig (Either (TD.ErrorDocument TS.Story) (TD.Document TS.Story))
getStoryResources _ (Left e) = return $ Left $ TAe.docError e
getStoryResources cur (Right includes) = SS.getStories cur >>= fmap (Right . TM.docMulti) . _fromPGStories includes


getStoryResource :: Int
                 -> Either TAe.ClientError [TS.StoryIncludes]
                 -> I.WithConfig (Either (TD.ErrorDocument TS.Story) (TD.Document TS.Story))
getStoryResource _ (Left e) = return $ Left $ TAe.docError e
getStoryResource sid (Right includes) = do
  mstory <- SS.getStory sid
  case mstory of
    Nothing ->
      return $ Left $ TAe.docError TAe.ResourceNotFound

    Just pgstory ->
      Right . TM.indexDocument' <$> _fromPGStory includes pgstory


getRandomStoryResource :: Either TAe.ClientError [TS.StoryIncludes]
                       -> I.WithConfig (Either (TD.ErrorDocument TS.Story) (TD.Document TS.Story))
getRandomStoryResource (Left e) = return $ Left $ TAe.docError e
getRandomStoryResource (Right includes) = do
  mstory <- SS.getRandomStory
  case mstory of
    Nothing -> 
      return $ Left $ TAe.docError TAe.ResourceNotFound

    Just pgStory ->
      Right . TM.indexDocument' <$> _fromPGStory includes pgStory



_fromPGStory :: [TS.StoryIncludes] -> TS.PGStory -> I.WithConfig TS.Story
_fromPGStory includes pgstory = do
  let
    sid = TS.pgStoryID pgstory
    aid = TS.storyAuthorID pgstory
  eauthor <- if TS.IAuthor `elem` includes
                then do
                  mauthor <- SA.getAuthor aid
                  case mauthor of
                    Nothing -> error $ "An author should exist with ID: " ++ show aid
                    Just author -> return . Or.Or $ Right author
                else return . Or.Or . Left $ TA.mkAuthorS aid
  etags <- if TS.ITags `elem` includes
              then (Or.Or . Right) <$> SS.getTagsForStory sid
              else (Or.Or . Left . map TT.mkTagS) <$> SS.getTagIDsForStory sid
  return $ TS.mkStoryFromDB pgstory eauthor etags

_linkAll :: [TS.PGStory]
         -> Either (M.Map Int TA.AuthorS) (M.Map Int TA.Author)
         -> Either (M.Map Int [TT.TagS]) (M.Map Int [TT.Tag])
         -> [TS.Story]
_linkAll stories idAuthorMap idTagMap =
  let
    _convert :: Either (M.Map k a) (M.Map k b) -> M.Map k (Or.Or a b)
    _convert (Left m) = fmap (Or.Or . Left) m
    _convert (Right m) = fmap (Or.Or . Right) m

    getTagsFor :: Int -> Or.Or [TT.TagS] [TT.Tag]
    getTagsFor pid = DM.fromMaybe (Or.Or $ Left []) . M.lookup pid $ _convert idTagMap

    getAuthorFor :: Int -> Or.Or TA.AuthorS TA.Author
    getAuthorFor pid = DM.fromJust $ M.lookup pid $ _convert idAuthorMap

    getStory' sg = TS.mkStoryFromDB sg (getAuthorFor pid) (getTagsFor pid)
      where pid = TS.pgStoryID sg
  in
    map getStory' stories


_fromPGStories :: [TS.StoryIncludes] -> [TS.PGStory] -> I.WithConfig [TS.Story]
_fromPGStories includes stories = do
  let
    storyIDs = map TS.pgStoryID stories
    authorIDs = map TS.storyAuthorID stories
    storyAuthorIDMap = M.fromList $ zip storyIDs authorIDs

  storyTagMap <- if TS.ITags `elem` includes
                    then Right <$> SS.getTagsForStories storyIDs
                    else Left <$> SS.getTagIDsForStories storyIDs
  authorsMap <- if TS.IAuthor `elem` includes
                   then do
                     authors <- SA.getMultiAuthors authorIDs
                     let
                       authorIDToAuthorMap = M.fromList [(TA.authorID a, a) | a <- authors]
                     return . Right $ fmap (authorIDToAuthorMap M.!) storyAuthorIDMap
                   else return . Left $ fmap TA.mkAuthorS storyAuthorIDMap

  return $ _linkAll stories authorsMap storyTagMap



-- UPDATE

updateStoryResource :: TS.StoryPut -> I.WithConfig (Either (TD.ErrorDocument TS.Story) (TD.Document TS.Story))
updateStoryResource = 
  fmap TM.docOrError . SS.updateStory


updateStoryResources :: [TS.StoryPut] -> I.WithConfig (TD.Document TS.Story)
updateStoryResources =
  fmap TM.docMulti . SS.updateStories



-- DELETE

deleteStoryResource :: Int -> I.WithConfig (Either (TD.ErrorDocument TS.Story) (TD.Document TS.Story))
deleteStoryResource =
    fmap TM.docMetaOrError . SS.deleteStory


deleteStoryResources :: [Int] -> I.WithConfig (TD.Document TS.Story)
deleteStoryResources =
    fmap (TM.docMeta . fromIntegral) . SS.deleteStories
