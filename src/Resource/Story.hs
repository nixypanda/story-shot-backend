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

import Data.Int (Int64)
import Data.Map (Map, lookup, fromList, (!))
import Data.Maybe (fromMaybe, fromJust)
import GHC.Generics (Generic)

import Data.Aeson (ToJSON(toJSON))
import Type.Doc

import Init (WithConfig)
import Type.Or
import Type.Story
import Type.Author (AuthorS, Author, mkAuthorS, authorID)
import Type.Tag (Tag, TagS, mkTagS)
import Type.Pagination
import Type.AppError
import Storage.Story
import Storage.Author (getAuthor, getMultiAuthors)


-- CREATE

createStoryResource :: Either ClientError [StoryIncludes]
                    -> StoryInsert
                    -> WithConfig (Either (ErrorDocument Story) (Document Story))
createStoryResource (Left e) _ = return $ Left $ docError e
createStoryResource (Right includes) si =
  createStory si >>= fmap (Right . indexDocument') . _fromPGStory includes


createStoryResources :: Either ClientError [StoryIncludes]
                     -> [StoryInsert]
                     -> WithConfig (Either (ErrorDocument Story) (Document Story))
createStoryResources (Left e) _ = return $ Left $ docError e
createStoryResources (Right includes) sis =
  createStories sis >>= fmap (Right . docMulti) . _fromPGStories includes



-- RETRIVE

getStoryResources :: CursorParam
                  -> Either ClientError [StoryIncludes]
                  -> WithConfig (Either (ErrorDocument Story) (Document Story))
getStoryResources _ (Left e) = return $ Left $ docError e
getStoryResources cur (Right includes) =
  getStories cur >>= fmap (Right . docMulti) . _fromPGStories includes


getStoryResource :: Int
                 -> Either ClientError [StoryIncludes]
                 -> WithConfig (Either (ErrorDocument Story) (Document Story))
getStoryResource _ (Left e) = return $ Left $ docError e
getStoryResource sid (Right includes) = do
  mstory <- getStory sid
  case mstory of
    Nothing ->
      return $ Left $ docError ResourceNotFound

    Just pgstory ->
      Right . indexDocument' <$> _fromPGStory includes pgstory


getRandomStoryResource :: Either ClientError [StoryIncludes]
                       -> WithConfig (Either (ErrorDocument Story) (Document Story))
getRandomStoryResource (Left e) = return $ Left $ docError e
getRandomStoryResource (Right includes) = do
  mstory <- getRandomStory
  case mstory of
    Nothing -> 
      return $ Left $ docError ResourceNotFound

    Just pgStory ->
      Right . indexDocument' <$> _fromPGStory includes pgStory



_fromPGStory :: [StoryIncludes] -> PGStory -> WithConfig Story
_fromPGStory includes pgstory = do
  let
    sid = pgStoryID pgstory
    aid = storyAuthorID pgstory
  eauthor <- if IAuthor `elem` includes
                then do
                  mauthor <- getAuthor aid
                  case mauthor of
                    Nothing -> error $ "An author should exist with ID: " ++ show aid
                    Just author -> return . Or $ Right author
                else return . Or . Left $ mkAuthorS aid
  etags <- if ITags `elem` includes
              then (Or . Right) <$> getTagsForStory sid
              else (Or . Left . map mkTagS) <$> getTagIDsForStory sid
  return $ mkStoryFromDB pgstory eauthor etags

_linkAll :: [PGStory]
         -> Either (Map Int AuthorS) (Map Int Author)
         -> Either (Map Int [TagS]) (Map Int [Tag])
         -> [Story]
_linkAll stories idAuthorMap idTagMap =
  let
    _convert :: Either (Map k a) (Map k b) -> Map k (Or a b)
    _convert (Left m) = fmap (Or . Left) m
    _convert (Right m) = fmap (Or . Right) m

    getTagsFor :: Int -> Or [TagS] [Tag]
    getTagsFor pid = fromMaybe (Or $ Left []) . Data.Map.lookup pid $ _convert idTagMap

    getAuthorFor :: Int -> Or AuthorS Author
    getAuthorFor pid = fromJust $ Data.Map.lookup pid $ _convert idAuthorMap

    getStory' sg = mkStoryFromDB sg (getAuthorFor pid) (getTagsFor pid)
      where pid = pgStoryID sg
  in
    map getStory' stories


_fromPGStories :: [StoryIncludes] -> [PGStory] -> WithConfig [Story]
_fromPGStories includes stories = do
  let
    storyIDs = map pgStoryID stories
    authorIDs = map storyAuthorID stories
    storyAuthorIDMap = fromList $ zip storyIDs authorIDs

  storyTagMap <- if ITags `elem` includes
                    then Right <$> getTagsForStories storyIDs
                    else Left <$> getTagIDsForStories storyIDs
  authorsMap <- if IAuthor `elem` includes
                   then do
                     authors <- getMultiAuthors authorIDs
                     let
                       authorIDToAuthorMap = fromList [(authorID a, a) | a <- authors]
                     return . Right $ fmap (authorIDToAuthorMap !) storyAuthorIDMap
                   else return . Left $ fmap mkAuthorS storyAuthorIDMap

  return $ _linkAll stories authorsMap storyTagMap



-- UPDATE

updateStoryResource :: StoryPut -> WithConfig (Either (ErrorDocument Story) (Document Story))
updateStoryResource = 
  fmap docOrError . updateStory


updateStoryResources :: [StoryPut] -> WithConfig (Document Story)
updateStoryResources =
  fmap docMulti . updateStories



-- DELETE

deleteStoryResource :: Int -> WithConfig (Either (ErrorDocument Story) (Document Story))
deleteStoryResource =
    fmap docMetaOrError . deleteStory


deleteStoryResources :: [Int] -> WithConfig (Document Story)
deleteStoryResources =
    fmap (docMeta . fromIntegral) . deleteStories


-- HELPERS


-- JSON API Related

data StoryMetaData = CursorInfo Cursor | CountInfo Int
  deriving (Eq, Show, Generic)

instance MetaObject StoryMetaData where
  typeName (CursorInfo _) = "cursor"
  typeName (CountInfo _) = "count"


instance ToJSON StoryMetaData where
  toJSON (CursorInfo cur) = toJSON cur
  toJSON (CountInfo count) = toJSON count


-- Builds the Meta data for the 'index' action
indexMetaData :: [Story] -> Meta
indexMetaData stories = mkMeta (CursorInfo Cursor
  { next = if null stories then 0 else maximum (fmap storyID stories)
  , size = length stories
  })


-- Builds the repsonse Document for the 'index' action
indexDocument :: [Story] -> Meta -> Document Story
indexDocument stories meta =
  mkListDoc stories (Just meta)


indexDocument' :: Story -> Document Story
indexDocument' = mkSingleDoc


docMulti :: [Story] -> Document Story
docMulti stories =
  indexDocument stories $ indexMetaData stories


docMetaOrError :: Int64 -> Either (ErrorDocument Story) (Document Story)
docMetaOrError 0 = Left $ docError ResourceNotFound
docMetaOrError 1 = Right $ indexDocument [] $ mkMeta $ CountInfo 1
docMetaOrError _ = error "Impossible"


docMeta :: Int -> Document Story
docMeta =
  indexDocument [] . mkMeta . CountInfo


docOrError :: Maybe Story -> Either (ErrorDocument a) (Document Story)
docOrError Nothing = Left $ docError ResourceNotFound
docOrError (Just at) = Right $ indexDocument' at
