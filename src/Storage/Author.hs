{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Storage.Author
  ( getAuthors
  , getAuthor
  , getMultiAuthors
  , authorQuery
  , createAuthor
  , createAuthors
  , updateAuthor
  , updateAuthors
  , deleteAuthor
  , deleteAuthors
  ) where


import qualified Control.Arrow as Arrow
import qualified Data.Int as DI
import qualified Data.Maybe as DM

import qualified Opaleye as O

import qualified Type.Pagination as TP
import qualified Type.Author as TA
import qualified Init as I
import qualified Storage.Utils as SU


-- CREATE

createAuthor :: TA.AuthorInsert -> I.WithConfig TA.Author
createAuthor =
  fmap head . createAuthors . return


createAuthors :: [TA.AuthorInsert] -> I.WithConfig [TA.Author]
createAuthors authors =
  SU.runDBInsertR TA.authorTable (map TA.mkAuthorWrite' authors) id


-- RETRIVE


authorQuery :: O.Query TA.AuthorRead
authorQuery = O.queryTable TA.authorTable

multiAuthorQuery :: [Int] -> O.Query TA.AuthorRead
multiAuthorQuery aids = proc () -> do
  row <- authorQuery -< ()
  O.restrict -< map O.constant aids `O.in_` TA.authorColID row
  Arrow.returnA -< row

cursorPaginatedAuthorQuery :: TP.CursorParam -> O.Query TA.AuthorRead
cursorPaginatedAuthorQuery TP.CursorParam{..} = O.limit sizeCursor $ proc () -> do
  row <- authorQuery -< ()
  O.restrict -< TA.authorColID row O..> O.constant nextCursor
  Arrow.returnA -< row

singleAuthor :: Int -> O.Query TA.AuthorRead
singleAuthor idA = proc () -> do
  row <- authorQuery -< ()
  O.restrict -< TA.authorColID row O..== O.constant idA
  Arrow.returnA -< row

getAuthors :: TP.CursorParam -> I.WithConfig [TA.Author]
getAuthors = SU.runDB . cursorPaginatedAuthorQuery

getAuthor :: Int -> I.WithConfig (Maybe TA.Author)
getAuthor = fmap DM.listToMaybe . SU.runDB . singleAuthor

getMultiAuthors :: [Int] -> I.WithConfig [TA.Author]
getMultiAuthors = SU.runDB . multiAuthorQuery



-- UPDATE

updateAuthors :: [TA.AuthorPut] -> I.WithConfig [TA.Author]
updateAuthors =
  fmap DM.catMaybes . mapM updateAuthor


updateAuthor :: TA.AuthorPut -> I.WithConfig (Maybe TA.Author)
updateAuthor author =
  let
    updateF _ = TA.mkAuthorWrite author
    predicate aRow = TA.authorColID aRow O..== O.constant (TA.authorID author)
  in
    DM.listToMaybe <$> SU.runDBUpdateR TA.authorTable updateF predicate id



-- DELETE

deleteAuthor :: Int -> I.WithConfig DI.Int64
deleteAuthor id' =
  SU.runDBDelete TA.authorTable (\aic -> TA.authorColID aic O..== O.constant id')


deleteAuthors :: [Int] -> I.WithConfig DI.Int64
deleteAuthors =
  fmap sum . mapM deleteAuthor
