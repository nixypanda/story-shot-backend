{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Type.Author
  ( Author
  , AuthorInsert
  , AuthorPut
  , AuthorWrite
  , AuthorRead
  , authorTable
  , mkAuthorPut
  , mkAuthorWrite
  , mkAuthorWrite'
  , authorID
  , authorColID
  , authorName
  , authorColName
  , authorLinks
  , authorIdentifier
  ) where

import Data.Monoid ((<>))
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import Data.Text (pack)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import Data.Aeson
  ( ToJSON(..)
  , FromJSON(..)
  , object
  , withObject
  , (.=)
  , (.:)
  )
import Network.JSONApi
  ( Links
  , ResourcefulEntity (..)
  , Identifier(..)
  , mkLinks
  )
import Opaleye
  ( Column
  , PGInt4
  , PGText
  , Table(Table)
  , PGTimestamptz
  , required
  , optional
  , constant
  )

import Utils (toURL)
import Class.Versioned


-- Strangely Polymorphic data type (Internal Use)

data Author' authorID authorName createdAt updatedAt =
  Author
    { _authorID :: authorID
    , _authorName :: authorName
    , _createdAt :: createdAt
    , _updatedAt :: updatedAt
    } deriving (Eq, Show, Generic)


-- Types that Will be used
type Author = Author' Int Text UTCTime UTCTime
type AuthorPut = Author' Int Text () ()
type AuthorInsert = Author' () Text () ()
type AuthorWrite = Author'
  (Maybe (Column PGInt4))
  (Column PGText)
  (Maybe (Column PGTimestamptz))
  (Maybe (Column PGTimestamptz))
type AuthorRead = Author'
  (Column PGInt4)
  (Column PGText)
  (Column PGTimestamptz)
  (Column PGTimestamptz)


instance Versioned Author where
  createdAt = _createdAt
  updatedAt = _updatedAt

-- Magic
$(makeAdaptorAndInstance "pAuthor" ''Author')

-- Opaleye table binding
authorTable :: Table AuthorWrite AuthorRead
authorTable = Table "authors" $
  pAuthor
    Author
      { _authorID = optional "id"
      , _authorName = required "name"
      , _createdAt = optional "created_at"
      , _updatedAt = optional "updated_at"
      }


-- Some Helpers

mkAuthorPut :: Int -> Text -> AuthorPut
mkAuthorPut aid name = Author
  { _authorID = aid
  , _authorName = name
  , _createdAt = ()
  , _updatedAt = ()
  }

mkAuthorWrite :: AuthorPut -> AuthorWrite
mkAuthorWrite Author{..} = Author
  { _authorID = constant $ Just _authorID
  , _authorName = constant _authorName
  , _createdAt = Nothing
  , _updatedAt = Nothing
  }

mkAuthorWrite' :: AuthorInsert -> AuthorWrite
mkAuthorWrite' Author{..} = Author
  { _authorID = Nothing
  , _authorName = constant _authorName
  , _createdAt = Nothing
  , _updatedAt = Nothing
  }

authorID :: Author' Int b c d -> Int
authorID = _authorID

authorName :: Author' a Text c d -> Text
authorName = _authorName

authorColID :: AuthorRead -> Column PGInt4
authorColID = _authorID

authorColName :: AuthorRead -> Column PGText
authorColName = _authorName


-- JSON

instance ToJSON Author where
  toJSON Author{..} = object
    [ "id" .= _authorID
    , "name" .= _authorName
    , "created-at" .= _createdAt
    , "updated-at" .= _updatedAt
    ]

instance FromJSON Author where
  parseJSON = withObject "author" $ \o -> Author
    <$> o .: "id"
    <*> o .: "name"
    <*> o .: "created-at"
    <*> o .: "updated-at"

instance FromJSON AuthorInsert where
  parseJSON = withObject "author" $ \o -> Author
    <$> pure ()
    <*> o .: "name"
    <*> pure ()
    <*> pure ()

instance FromJSON AuthorPut where
  parseJSON = withObject "author" $ \o -> Author
    <$> o .: "id"
    <*> o .: "name"
    <*> pure ()
    <*> pure ()

-- JSON API

authorIdentifier :: Author -> Identifier
authorIdentifier author = Identifier
  (pack . show . authorID $ author)
  "Author"
  (resourceMetaData author)

instance ResourcefulEntity Author where
  resourceIdentifier = pack . show . authorID
  resourceType _ = "Author"
  resourceLinks = Just . authorLinks
  resourceMetaData _ = Nothing
  resourceRelationships _ = Nothing

authorLinks :: Author -> Links
authorLinks author = mkLinks [("self", selfLink)]
  where
    selfLink = toURL selfPath
    selfPath = "/author/" <> show (authorID author)
