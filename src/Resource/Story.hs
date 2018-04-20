{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Data.Int as DI
import qualified Data.Map as M
import qualified Data.Maybe as DM
import qualified GHC.Generics as Generics

import qualified Data.Aeson as DA

import qualified Init as I
import qualified Type.Or as Or
import qualified Type.Doc as TD
import qualified Type.Pagination as TP
import qualified Type.AppError as TAe
import qualified Type.Story as TS
import qualified Type.Author as TA
import qualified Type.Tag as TT
import qualified Storage.Story as SS
import qualified Storage.Author as SA


-- CREATE

createStoryResource :: Either TAe.ClientError [TS.StoryIncludes]
                    -> TS.StoryInsert
                    -> I.WithConfig (Either (TD.ErrorDocument TS.Story) (TD.Document TS.Story))
createStoryResource (Left e) _ = return $ Left $ TAe.docError e
createStoryResource (Right includes) si =
  SS.createStory si >>= fmap (Right . indexDocument') . _fromPGStory includes


createStoryResources :: Either TAe.ClientError [TS.StoryIncludes]
                     -> [TS.StoryInsert]
                     -> I.WithConfig (Either (TD.ErrorDocument TS.Story) (TD.Document TS.Story))
createStoryResources (Left e) _ = return $ Left $ TAe.docError e
createStoryResources (Right includes) sis =
  SS.createStories sis >>= fmap (Right . docMulti) . _fromPGStories includes



-- RETRIVE

getStoryResources :: TP.CursorParam
                  -> Either TAe.ClientError [TS.StoryIncludes]
                  -> I.WithConfig (Either (TD.ErrorDocument TS.Story) (TD.Document TS.Story))
getStoryResources _ (Left e) = return $ Left $ TAe.docError e
getStoryResources cur (Right includes) =
  SS.getStories cur >>= fmap (Right . docMulti) . _fromPGStories includes


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
      Right . indexDocument' <$> _fromPGStory includes pgstory


getRandomStoryResource :: Either TAe.ClientError [TS.StoryIncludes]
                       -> I.WithConfig (Either (TD.ErrorDocument TS.Story) (TD.Document TS.Story))
getRandomStoryResource (Left e) = return $ Left $ TAe.docError e
getRandomStoryResource (Right includes) = do
  mstory <- SS.getRandomStory
  case mstory of
    Nothing -> 
      return $ Left $ TAe.docError TAe.ResourceNotFound

    Just pgStory ->
      Right . indexDocument' <$> _fromPGStory includes pgStory



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
  fmap docOrError . SS.updateStory


updateStoryResources :: [TS.StoryPut] -> I.WithConfig (TD.Document TS.Story)
updateStoryResources =
  fmap docMulti . SS.updateStories



-- DELETE

deleteStoryResource :: Int -> I.WithConfig (Either (TD.ErrorDocument TS.Story) (TD.Document TS.Story))
deleteStoryResource =
    fmap docMetaOrError . SS.deleteStory


deleteStoryResources :: [Int] -> I.WithConfig (TD.Document TS.Story)
deleteStoryResources =
    fmap (docMeta . fromIntegral) . SS.deleteStories


-- HELPERS


-- JSON API Related

data StoryMetaData = CursorInfo TP.Cursor | CountInfo Int
  deriving (Eq, Show, Generics.Generic)

instance TD.MetaObject StoryMetaData where
  typeName (CursorInfo _) = "cursor"
  typeName (CountInfo _) = "count"


instance DA.ToJSON StoryMetaData where
  toJSON (CursorInfo cur) = DA.toJSON cur
  toJSON (CountInfo count) = DA.toJSON count


-- Builds the Meta data for the 'index' action
indexMetaData :: [TS.Story] -> TD.Meta
indexMetaData stories = TD.mkMeta (CursorInfo TP.Cursor
  { TP.next = if null stories then 0 else maximum (fmap TS.storyID stories)
  , TP.size = length stories
  })


-- Builds the repsonse TD.Document for the 'index' action
indexDocument :: [TS.Story] -> TD.Meta -> TD.Document TS.Story
indexDocument stories meta =
  TD.mkListDoc stories (Just meta)


indexDocument' :: TS.Story -> TD.Document TS.Story
indexDocument' = TD.mkSingleDoc


docMulti :: [TS.Story] -> TD.Document TS.Story
docMulti stories =
  indexDocument stories $ indexMetaData stories


docMetaOrError :: DI.Int64 -> Either (TD.ErrorDocument TS.Story) (TD.Document TS.Story)
docMetaOrError 0 = Left $ TAe.docError TAe.ResourceNotFound
docMetaOrError 1 = Right $ indexDocument [] $ TD.mkMeta $ CountInfo 1
docMetaOrError _ = error "Impossible"


docMeta :: Int -> TD.Document TS.Story
docMeta =
  indexDocument [] . TD.mkMeta . CountInfo


docOrError :: Maybe TS.Story -> Either (TD.ErrorDocument a) (TD.Document TS.Story)
docOrError Nothing = Left $ TAe.docError TAe.ResourceNotFound
docOrError (Just at) = Right $ indexDocument' at
