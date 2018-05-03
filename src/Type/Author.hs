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
  , AuthorS
  , authorTable
  , mkAuthorS
  , mkAuthorInsert
  , mkAuthorPut
  , mkAuthorWrite
  , mkAuthorWrite'
  , authorID
  , authorColID
  , authorName
  , authorColName
  , validAuthorPutObject
  , validAuthorInsertObject
  ) where


import Data.Monoid ((<>))
import Data.Aeson ((.=), (.:))

import qualified Data.Time as DT
import qualified GHC.Generics as Generics

import qualified Data.Text as Text
import qualified Data.Profunctor.Product.TH as ProductProfunctor
import qualified Data.Aeson as Aeson
import qualified Opaleye as O

import qualified Class.Resource as CR



-- Strangely Polymorphic data type (Internal Use)

data Author' authorID authorName createdAt updatedAt =
  Author
    { _authorID :: authorID
    , _authorName :: authorName
    , _createdAt :: createdAt
    , _updatedAt :: updatedAt
    } deriving (Eq, Show, Generics.Generic)


-- Types that Will be used
type Author = Author' Int Text.Text DT.UTCTime DT.UTCTime
type AuthorS = Author' Int () () ()
type AuthorPut = Author' Int Text.Text () ()
type AuthorInsert = Author' () Text.Text () ()
type AuthorWrite = Author'
  (Maybe (O.Column O.PGInt4))
  (O.Column O.PGText)
  (Maybe (O.Column O.PGTimestamptz))
  (Maybe (O.Column O.PGTimestamptz))
type AuthorRead = Author'
  (O.Column O.PGInt4)
  (O.Column O.PGText)
  (O.Column O.PGTimestamptz)
  (O.Column O.PGTimestamptz)


instance CR.Resource Author where
  rid = _authorID
  type' _ = "author"
  createdAt = _createdAt
  updatedAt = _updatedAt

instance CR.ShortResource AuthorS where
  srid = _authorID
  srType' _ = "author"


-- Magic
$(ProductProfunctor.makeAdaptorAndInstance "pAuthor" ''Author')


-- Opaleye table binding
authorTable :: O.Table AuthorWrite AuthorRead
authorTable = O.Table "authors" $
  pAuthor
    Author
      { _authorID = O.optional "id"
      , _authorName = O.required "name"
      , _createdAt = O.optional "created_at"
      , _updatedAt = O.optional "updated_at"
      }



-- Some Helpers

mkAuthorInsert :: Text.Text -> AuthorInsert
mkAuthorInsert name = Author
  { _authorID = ()
  , _authorName = name
  , _createdAt = ()
  , _updatedAt = ()
  }

mkAuthorPut :: Int -> Text.Text -> AuthorPut
mkAuthorPut aid name = Author
  { _authorID = aid
  , _authorName = name
  , _createdAt = ()
  , _updatedAt = ()
  }


mkAuthorWrite :: AuthorPut -> AuthorWrite
mkAuthorWrite Author{..} = Author
  { _authorID = O.constant $ Just _authorID
  , _authorName = O.constant _authorName
  , _createdAt = Nothing
  , _updatedAt = Nothing
  }


mkAuthorWrite' :: AuthorInsert -> AuthorWrite
mkAuthorWrite' Author{..} = Author
  { _authorID = Nothing
  , _authorName = O.constant _authorName
  , _createdAt = Nothing
  , _updatedAt = Nothing
  }


mkAuthorS :: Int -> AuthorS
mkAuthorS aid = Author
  { _authorID = aid
  , _authorName = ()
  , _createdAt = ()
  , _updatedAt = ()
  }


authorID :: Author' Int b c d -> Int
authorID = _authorID


authorName :: Author' a Text.Text c d -> Text.Text
authorName = _authorName


authorColID :: AuthorRead -> O.Column O.PGInt4
authorColID = _authorID


authorColName :: AuthorRead -> O.Column O.PGText
authorColName = _authorName



-- JSON

instance Aeson.ToJSON Author where
  toJSON Author{..} = Aeson.object
    [ "id" .= _authorID
    , "name" .= _authorName
    , "created-at" .= _createdAt
    , "updated-at" .= _updatedAt
    , "type" .= ("author" :: Text.Text)
    , "link" .= ((Text.pack $ "/author/" <> show _authorID) :: Text.Text)
    ]


instance Aeson.ToJSON AuthorS where
  toJSON Author{..} = Aeson.object
    [ "id" .= _authorID
    , "type" .= ("author" :: Text.Text)
    , "link" .= ((Text.pack $ "/author/" <> show _authorID) :: Text.Text)
    ]


instance Aeson.FromJSON AuthorS where
  parseJSON = Aeson.withObject "author" $ \o -> Author
    <$> o .: "id"
    <*> pure ()
    <*> pure ()
    <*> pure ()


instance Aeson.FromJSON Author where
  parseJSON = Aeson.withObject "author" $ \o -> Author
    <$> o .: "id"
    <*> o .: "name"
    <*> o .: "created-at"
    <*> o .: "updated-at"


instance Aeson.FromJSON AuthorInsert where
  parseJSON = Aeson.withObject "author" $ \o -> Author
    <$> pure ()
    <*> o .: "name"
    <*> pure ()
    <*> pure ()


instance Aeson.FromJSON AuthorPut where
  parseJSON = Aeson.withObject "author" $ \o -> Author
    <$> o .: "id"
    <*> o .: "name"
    <*> pure ()
    <*> pure ()



-- Valid Request Hints

validAuthorInsertObject :: Aeson.Value
validAuthorInsertObject = Aeson.object
  [ "name" .= ("The name you want to give to the author you are creating" :: Text.Text)
  ]


validAuthorPutObject :: Aeson.Value
validAuthorPutObject = Aeson.object
  [ "id" .= ("The id of the author which should be in the DB" :: Text.Text)
  , "name" .= ("The name you want to give to the author with the above id" :: Text.Text)
  ]
