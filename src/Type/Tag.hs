{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Type.Tag
  ( Tag
  , TagInsert
  , TagPut
  , TagWrite
  , TagRead
  , TagS
  , tagTable
  , mkTagS
  , mkTagPut
  , mkTagWrite
  , mkTagWrite'
  , tagID
  , tagColID
  , tagName
  , tagColName
  , tagGenre
  , validTagInsertObject
  , validTagPutObject
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
import qualified Type.Genre as TG



-- Strangely Polymorphic data type (Internal Use)

data Tag' id' name genre createdAt updatedAt =
  Tag
    { _tagID :: id'
    , _tagName :: name
    , _tagGenre :: genre
    , _createdAt :: createdAt
    , _updatedAt :: updatedAt
    } deriving (Eq, Show, Generics.Generic)


-- Types that Will be used

type Tag = Tag' Int Text.Text TG.Genre DT.UTCTime DT.UTCTime
type TagS = Tag' Int () () () ()
type TagPut = Tag' Int Text.Text TG.Genre () ()
type TagInsert = Tag' () Text.Text TG.Genre () ()
type TagRead = Tag'
  (O.Column O.PGInt4)
  (O.Column O.PGText)
  (O.Column O.PGText)
  (O.Column O.PGTimestamptz)
  (O.Column O.PGTimestamptz)
type TagWrite = Tag'
  (Maybe (O.Column O.PGInt4))
  (O.Column O.PGText)
  (O.Column O.PGText)
  (Maybe (O.Column O.PGTimestamptz))
  (Maybe (O.Column O.PGTimestamptz))


instance CR.Resource Tag where
  rid  = _tagID
  createdAt = _createdAt
  updatedAt = _updatedAt
  type' _ = "tag"



-- Magic

$(ProductProfunctor.makeAdaptorAndInstance "pTag" ''Tag')



-- Opaleye table binding

tagTable :: O.Table TagWrite TagRead
tagTable = O.Table "tags" $
  pTag
    Tag
      { _tagID = O.optional "id"
      , _tagName = O.required "name"
      , _tagGenre = O.required "genre"
      , _createdAt = O.optional "created_at"
      , _updatedAt = O.optional "updated_at"
      }



-- Some Helpers

mkTagPut :: Int -> Text.Text -> TG.Genre -> TagPut
mkTagPut tid name genre = Tag
  { _tagID = tid
  , _tagName = name
  , _tagGenre = genre
  , _createdAt = ()
  , _updatedAt = ()
  }


mkTagS :: Int -> TagS
mkTagS tid = Tag
  { _tagID = tid
  , _tagName = ()
  , _tagGenre = ()
  , _createdAt = ()
  , _updatedAt = ()
  }


mkTagWrite' :: TagInsert -> TagWrite
mkTagWrite' Tag{..} = Tag
  { _tagID = Nothing
  , _tagName = O.constant _tagName
  , _tagGenre = O.constant _tagGenre
  , _createdAt = Nothing
  , _updatedAt = Nothing
  }


mkTagWrite :: TagPut -> TagWrite
mkTagWrite Tag{..} = Tag
  { _tagID = O.constant $ Just _tagID
  , _tagName = O.constant _tagName
  , _tagGenre = O.constant _tagGenre
  , _createdAt = Nothing
  , _updatedAt = Nothing
  }


tagID :: Tag' Int b c d e -> Int
tagID = _tagID


tagName :: Tag' a Text.Text c d e -> Text.Text
tagName = _tagName


tagGenre :: Tag' a b TG.Genre d e -> TG.Genre
tagGenre = _tagGenre


tagColID :: TagRead -> O.Column O.PGInt4
tagColID = _tagID


tagColName :: TagRead -> O.Column O.PGText
tagColName = _tagName



-- JSON

instance Aeson.ToJSON Tag where
  toJSON Tag{..} = Aeson.object
    [ "id" .= _tagID
    , "name" .= _tagName
    , "genre" .= _tagGenre
    , "created-at" .= _createdAt
    , "updated-at" .= _updatedAt
    , "type" .= ("tag" :: Text.Text)
    , "link" .= ((Text.pack $ "/tag/" <> show _tagID) :: Text.Text)
    ]


instance Aeson.ToJSON TagS where
  toJSON Tag{..} = Aeson.object
    [ "id" .= _tagID
    , "type" .= ("tag" :: Text.Text)
    , "link" .= ((Text.pack $ "/tag/" <> show _tagID) :: Text.Text)
    ]


instance Aeson.FromJSON TagS where
  parseJSON = Aeson.withObject "tag" $ \o -> Tag
    <$> o .: "id"
    <*> pure ()
    <*> pure ()
    <*> pure ()
    <*> pure ()


instance Aeson.FromJSON Tag where
  parseJSON = Aeson.withObject "tag" $ \o -> Tag
    <$> o .: "id"
    <*> o .: "name"
    <*> o .: "genre"
    <*> o .: "created-at"
    <*> o .: "updated-at"


instance Aeson.FromJSON TagInsert where
  parseJSON = Aeson.withObject "tag" $ \o -> Tag
    <$> pure ()
    <*> o .: "name"
    <*> o .: "genre"
    <*> pure ()
    <*> pure ()


instance Aeson.FromJSON TagPut where
  parseJSON = Aeson.withObject "tag" $ \o -> Tag
    <$> o .: "id"
    <*> o .: "name"
    <*> o .: "genre"
    <*> pure ()
    <*> pure ()


-- Valid Request Hints

validTagInsertObject :: Aeson.Value
validTagInsertObject = Aeson.object
  [ "name" .= ("The name you want to give to this tag you are creating" :: Text.Text)
  , "genre" .= ("One of " ++ show TG.allGenres)
  ]


validTagPutObject :: Aeson.Value
validTagPutObject = Aeson.object
  [ "id" .= ("The id of the tag which should be in the DB" :: Text.Text)
  , "name" .= ("The name you want to give to the tag with the above id" :: Text.Text)
  , "genre" .= ("One of " ++ show TG.allGenres)
  ]
