{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Storage.Author
  ( getAuthors
  , getAuthor
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
  , (.==)
  , constant
  , restrict
  , queryTable
  )

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

singleAuthor :: Int -> Query AuthorRead
singleAuthor idA = proc () -> do
  row <- authorQuery -< ()
  restrict -< authorColID row .== constant idA

  returnA -< row


getAuthors :: WithConfig [Author]
getAuthors = runDB authorQuery


getAuthor :: Int -> WithConfig (Maybe Author)
getAuthor = fmap listToMaybe . runDB . singleAuthor



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
