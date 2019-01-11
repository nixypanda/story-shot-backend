{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}


module Storage.Types.Author
  ( AuthorModel
  , AuthorWrite
  , AuthorRead
  , authorTable
  , mkAuthorWrite
  , mkAuthorWrite'
  , authorID
  , authorColID
  , authorName
  , authorColName
  ) where


import Prelude hiding (id)

import qualified Data.Time                  as DT
import qualified GHC.Generics               as Generics

import qualified Data.Profunctor.Product.TH as ProductProfunctor
import qualified Data.Text                  as Text
import qualified Opaleye                    as O

import Domain.Types (AuthorInsert(..), AuthorPut(..))



-- Strangely Polymorphic data type (Internal Use)

data AuthorModel' authorID authorName createdAt updatedAt =
  AuthorModel
    { _authorID   :: authorID
    , _authorName :: authorName
    , _createdAt  :: createdAt
    , _updatedAt  :: updatedAt
    } deriving (Eq, Show, Generics.Generic)


-- Types that Will be used
type AuthorModel = AuthorModel' Int Text.Text DT.UTCTime DT.UTCTime
type AuthorWrite = AuthorModel'
  (Maybe (O.Column O.PGInt4))
  (O.Column O.PGText)
  (Maybe (O.Column O.PGTimestamptz))
  (Maybe (O.Column O.PGTimestamptz))
type AuthorRead = AuthorModel'
  (O.Column O.PGInt4)
  (O.Column O.PGText)
  (O.Column O.PGTimestamptz)
  (O.Column O.PGTimestamptz)


-- Magic
$(ProductProfunctor.makeAdaptorAndInstance "pAuthor" ''AuthorModel')


-- Opaleye table binding
authorTable :: O.Table AuthorWrite AuthorRead
authorTable = O.Table "authors" $
  pAuthor
    AuthorModel
      { _authorID = O.optional "id"
      , _authorName = O.required "name"
      , _createdAt = O.optional "created_at"
      , _updatedAt = O.optional "updated_at"
      }



-- Some Helpers

mkAuthorWrite :: AuthorPut -> AuthorWrite
mkAuthorWrite AuthorPut{..} = AuthorModel
  { _authorID = O.constant $ Just id
  , _authorName = O.constant name
  , _createdAt = Nothing
  , _updatedAt = Nothing
  }


mkAuthorWrite' :: AuthorInsert -> AuthorWrite
mkAuthorWrite' AuthorInsert{..} = AuthorModel
  { _authorID = Nothing
  , _authorName = O.constant name
  , _createdAt = Nothing
  , _updatedAt = Nothing
  }


authorID :: AuthorModel' Int b c d -> Int
authorID = _authorID


authorName :: AuthorModel' a Text.Text c d -> Text.Text
authorName = _authorName


authorColID :: AuthorRead -> O.Column O.PGInt4
authorColID = _authorID


authorColName :: AuthorRead -> O.Column O.PGText
authorColName = _authorName
