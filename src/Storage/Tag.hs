{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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

import qualified Control.Arrow as Arrow
import qualified Data.Int as DI
import qualified Data.Maybe as DM

import qualified Opaleye as O

import qualified Init as I
import qualified Type.Pagination as TP
import qualified Type.Tag as TT
import qualified Storage.Utils as SU



-- CREATE

createTag :: TT.TagInsert -> I.WithConfig TT.Tag
createTag =
  fmap head . createTags . return


createTags :: [TT.TagInsert] -> I.WithConfig [TT.Tag]
createTags tags =
  SU.runDBInsertR TT.tagTable (map TT.mkTagWrite' tags) id



-- RETRIVE

tagQuery :: O.Query TT.TagRead
tagQuery = O.queryTable TT.tagTable


cursorPaginatedTagQuery :: TP.CursorParam -> O.Query TT.TagRead
cursorPaginatedTagQuery TP.CursorParam{..} = O.limit sizeCursor $ proc () -> do
  row <- tagQuery -< ()
  O.restrict -< TT.tagColID row O..> O.constant nextCursor

  Arrow.returnA -< row


singleTag :: Int -> O.Query TT.TagRead
singleTag idA = proc () -> do
  row <- tagQuery -< ()
  O.restrict -< TT.tagColID row O..== O.constant idA

  Arrow.returnA -< row


getTags :: TP.CursorParam -> I.WithConfig [TT.Tag]
getTags = SU.runDB . cursorPaginatedTagQuery


getTag :: Int -> I.WithConfig (Maybe TT.Tag)
getTag = fmap DM.listToMaybe . SU.runDB . singleTag


-- UPDATE

updateTags :: [TT.TagPut] -> I.WithConfig [TT.Tag]
updateTags =
  fmap DM.catMaybes . mapM updateTag


updateTag :: TT.TagPut -> I.WithConfig (Maybe TT.Tag)
updateTag tag =
  let
    updateF _ = TT.mkTagWrite tag
    predicate aRow = TT.tagColID aRow O..== O.constant (TT.tagID tag)
  in
    DM.listToMaybe <$> SU.runDBUpdateR TT.tagTable updateF predicate id



-- DELETE

deleteTag :: Int -> I.WithConfig DI.Int64
deleteTag id' =
  SU.runDBDelete TT.tagTable (\aic -> TT.tagColID aic O..== O.constant id')


deleteTags :: [Int] -> I.WithConfig DI.Int64
deleteTags =
  fmap sum . mapM deleteTag
