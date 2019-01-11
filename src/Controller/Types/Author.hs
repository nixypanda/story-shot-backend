{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Controller.Types.Author where


import           Data.Aeson   ((.=))
import           Data.Monoid  ((<>))
import           Prelude      hiding (id)

import qualified Data.Aeson   as Aeson
import qualified Data.Text    as Text
import           Domain.Types (Author (..), AuthorBase (..), AuthorInsert,
                               AuthorPut)


instance Aeson.FromJSON AuthorPut
instance Aeson.FromJSON AuthorInsert
instance Aeson.FromJSON AuthorBase
instance Aeson.FromJSON Author


instance Aeson.ToJSON Author where
  toJSON Author{..} = Aeson.object
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
