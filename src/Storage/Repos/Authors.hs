{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Storage.Repos.Authors
  ( createAuthor
  , createAuthors
  ) where


import qualified Control.Arrow   as Arrow
import qualified Data.Int        as DI
import qualified Data.Maybe      as DM

import qualified Opaleye         as O

import qualified Init            as I
import qualified Storage.Utils   as SU

import Domain.Types.Author
import Storage.Types.Author



-- CREATE

createAuthor :: AuthorInsert -> I.AppT Author
createAuthor = fmap toAuthor . _createAuthor


createAuthors :: [AuthorInsert] -> I.AppT [Author]
createAuthors = fmap (fmap toAuthor) . _createAuthors


_createAuthor :: AuthorInsert -> I.AppT AuthorModel
_createAuthor =
  fmap head . _createAuthors . return


_createAuthors :: [AuthorInsert] -> I.AppT [AuthorModel]
_createAuthors authors =
  SU.runDBInsertR authorTable (map fromAuthorInsert authors) Prelude.id

