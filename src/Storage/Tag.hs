{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Storage.Tag
  ( getTags
  , getTag
  , tagQuery
  , createTag
  , createTags
  , updateTag
  , updateTags
  , deleteTag
  , deleteTags
  ) where

import Control.Arrow (returnA)
import Data.Int (Int64)
import Data.Maybe (listToMaybe, catMaybes)

import Opaleye
  ( Query
  , (.==)
  , constant
  , restrict
  , queryTable
  )

import Type.Tag
import Init (WithConfig)
import Storage.Utils
  ( runDB
  , runDBInsertR
  , runDBUpdateR
  , runDBDelete
  )



-- CREATE

createTag :: TagInsert -> WithConfig Tag
createTag =
  fmap head . createTags . return


createTags :: [TagInsert] -> WithConfig [Tag]
createTags tags =
  runDBInsertR tagTable (map mkTagWrite' tags) id



-- RETRIVE

tagQuery :: Query TagRead
tagQuery = queryTable tagTable

singleTag :: Int -> Query TagRead
singleTag idA = proc () -> do
  row <- tagQuery -< ()
  restrict -< tagColID row .== constant idA

  returnA -< row


getTags :: WithConfig [Tag]
getTags = runDB tagQuery


getTag :: Int -> WithConfig (Maybe Tag)
getTag = fmap listToMaybe . runDB . singleTag


-- UPDATE

updateTags :: [TagPut] -> WithConfig [Tag]
updateTags =
  fmap catMaybes . mapM updateTag


updateTag :: TagPut -> WithConfig (Maybe Tag)
updateTag tag =
  let
    updateF _ = mkTagWrite tag
    predicate aRow = tagColID aRow .== constant (tagID tag)
  in
    listToMaybe <$> runDBUpdateR tagTable updateF predicate id



-- DELETE

deleteTag :: Int -> WithConfig Int64
deleteTag id' =
  runDBDelete tagTable (\aic -> tagColID aic .== constant id')


deleteTags :: [Int] -> WithConfig Int64
deleteTags =
  fmap sum . mapM deleteTag
