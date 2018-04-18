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


import Control.Arrow (returnA)
import Data.Int (Int64)
import Data.Maybe (listToMaybe, catMaybes)

import Opaleye
  ( Query
  , QueryArr
  , (.==)
  , (.>)
  , in_
  , constant
  , limit
  , queryTable
  , restrict
  )

import Type.Pagination
import Type.Author
import Init (WithConfig)
import Storage.Utils
  ( runDB
  , runDBInsertR
  , runDBUpdateR
  , runDBDelete
  )


-- CREATE

createAuthor :: AuthorInsert -> WithConfig Author
createAuthor =
  fmap head . createAuthors . return


createAuthors :: [AuthorInsert] -> WithConfig [Author]
createAuthors authors =
  runDBInsertR authorTable (map mkAuthorWrite' authors) id


-- RETRIVE


authorQuery :: Query AuthorRead
authorQuery = queryTable authorTable

multiAuthorQuery :: [Int] -> Query AuthorRead
multiAuthorQuery aids = proc () -> do
  row <- authorQuery -< ()
  restrict -< map constant aids `in_` authorColID row
  returnA -< row

cursorPaginatedAuthorQuery :: CursorParam -> Query AuthorRead
cursorPaginatedAuthorQuery CursorParam{..} = limit sizeCursor $ proc () -> do
  row <- authorQuery -< ()
  restrict -< authorColID row .> constant nextCursor
  returnA -< row

singleAuthor :: Int -> Query AuthorRead
singleAuthor idA = proc () -> do
  row <- authorQuery -< ()
  restrict -< authorColID row .== constant idA
  returnA -< row

getAuthors :: CursorParam -> WithConfig [Author]
getAuthors = runDB . cursorPaginatedAuthorQuery

getAuthor :: Int -> WithConfig (Maybe Author)
getAuthor = fmap listToMaybe . runDB . singleAuthor

getMultiAuthors :: [Int] -> WithConfig [Author]
getMultiAuthors = runDB . multiAuthorQuery



-- UPDATE

updateAuthors :: [AuthorPut] -> WithConfig [Author]
updateAuthors =
  fmap catMaybes . mapM updateAuthor


updateAuthor :: AuthorPut -> WithConfig (Maybe Author)
updateAuthor author =
  let
    updateF _ = mkAuthorWrite author
    predicate aRow = authorColID aRow .== constant (authorID author)
  in
    listToMaybe <$> runDBUpdateR authorTable updateF predicate id



-- DELETE

deleteAuthor :: Int -> WithConfig Int64
deleteAuthor id' =
  runDBDelete authorTable (\aic -> authorColID aic .== constant id')


deleteAuthors :: [Int] -> WithConfig Int64
deleteAuthors =
  fmap sum . mapM deleteAuthor
