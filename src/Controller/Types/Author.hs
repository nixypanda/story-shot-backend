{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Controller.Types.Author where


import           Data.Aeson   ((.=))
import           Data.Monoid  ((<>))

import qualified Data.Time    as DT
import qualified GHC.Generics as Generics

import qualified Data.Aeson   as Aeson
import qualified Data.Text    as Text


newtype AuthorInsert = AuthorInsert
    { name :: Text.Text
    } deriving (Eq, Show, Generics.Generic)

newtype AuthorBase = AuthorBase
    { id :: Int
    } deriving (Eq, Show, Generics.Generic)

data AuthorPut = AuthorPut
    { id      :: Int
    , newName :: Text.Text
    } deriving (Eq, Show, Generics.Generic)


data AuthorResource = AuthorResource
    { id        :: Int
    , name      :: Text.Text
    , createdAt :: DT.UTCTime
    , updatedAt :: DT.UTCTime
    } deriving (Eq, Show, Generics.Generic)


instance Aeson.FromJSON AuthorPut
instance Aeson.FromJSON AuthorInsert
instance Aeson.FromJSON AuthorBase
instance Aeson.FromJSON AuthorResource


instance Aeson.ToJSON AuthorResource where
  toJSON AuthorResource{..} = Aeson.object
    [ "id" .= id
    , "name" .= name
    , "created-at" .= createdAt
    , "updated-at" .= updatedAt
    , "type" .= ("author" :: Text.Text)
    , "link" .= ((Text.pack $ "/author/" <> show id) :: Text.Text)
    ]


instance Aeson.ToJSON AuthorBase where
  toJSON AuthorBase{..} = Aeson.object
    [ "id" .= id
    , "type" .= ("author" :: Text.Text)
    , "link" .= ((Text.pack $ "/author/" <> show id) :: Text.Text)
    ]


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
