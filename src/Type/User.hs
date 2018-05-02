{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}


module Type.User
  ( User
  , UserInsert
  , UserPut
  , UserPut'
  , UserWrite
  , UserRead
  , UserS
  , PGUser
  , UserIncludes(..)
  , userTable
  , userName
  , mkUserWrite
  , mkUserFromDB
  , mkUserWrite'
  , mkUserPut
  , userID
  , pgUserID
  , userAuthorID
  , userColID
  , validUserPutObject
  , validUserInsertObject
  ) where


import Data.Monoid ((<>))
import Data.Aeson ((.=), (.:))

import qualified Data.Time as DT
import qualified GHC.Generics as Generics
import qualified Data.Either as DEither

import qualified Data.Text as Text
import qualified Data.Profunctor.Product.TH as ProductProfunctor
import qualified Data.Aeson as Aeson
import qualified Opaleye as O

import qualified Class.Resource as CR
import qualified Class.Includes as CI
import qualified Type.AppError as TAe
import qualified Type.Or as TO
import qualified Type.Author as TA



-- Strangely Polymorphic data type (Internal Use)

data User' userID userName author createdAt updatedAt = User
  { _userID :: userID
  , _userName :: userName
  , _userAuthor :: author
  , _createdAt :: createdAt
  , _updatedAt :: updatedAt
  } deriving (Eq, Show, Generics.Generic)


data PGUser' userID userName author createdAt updatedAt = PGUser
  { _pgUserID :: userID
  , _pgUserName :: userName
  , _pgUserAuthor :: author
  , _pgCreatedAt :: createdAt
  , _pgUpdatedAt :: updatedAt
  } deriving (Eq, Show, Generics.Generic)



-- Types that Will be used
type User = User' Int Text.Text (TO.Or TA.AuthorS TA.Author) DT.UTCTime DT.UTCTime
type UserS = User' Int () () () ()
type UserPut = User' Int Text.Text Int () ()
type UserPut' = User' () Text.Text Int () ()
type UserInsert = User' () Text.Text () () ()

type PGUser = PGUser' Int Text.Text Int DT.UTCTime DT.UTCTime
type UserWrite = PGUser'
  (Maybe (O.Column O.PGInt4))
  (O.Column O.PGText)
  (O.Column O.PGInt4)
  (Maybe (O.Column O.PGTimestamptz))
  (Maybe (O.Column O.PGTimestamptz))
type UserRead = PGUser'
  (O.Column O.PGInt4)
  (O.Column O.PGText)
  (O.Column O.PGInt4)
  (O.Column O.PGTimestamptz)
  (O.Column O.PGTimestamptz)


instance CR.Resource User where
  identity = _userID
  createdAt = _createdAt
  updatedAt = _updatedAt


-- Magic
$(ProductProfunctor.makeAdaptorAndInstance "pUser" ''PGUser')


-- Opaleye table binding
userTable :: O.Table UserWrite UserRead
userTable = O.Table "users" $ pUser
  PGUser
    { _pgUserID = O.optional "id"
    , _pgUserName = O.required "name"
    , _pgUserAuthor = O.required "author"
    , _pgCreatedAt = O.optional "created_at"
    , _pgUpdatedAt = O.optional "updated_at"
    }



-- Some Helpers

mkUserFromDB :: PGUser -> TO.Or TA.AuthorS TA.Author -> User
mkUserFromDB PGUser{..} author' = User
  { _userID = _pgUserID
  , _userName = _pgUserName
  , _userAuthor = author'
  , _createdAt = _pgCreatedAt
  , _updatedAt = _pgUpdatedAt
  }


mkUserWrite :: UserPut -> UserWrite
mkUserWrite User{..} = PGUser
  { _pgUserID = O.constant $ Just _userID
  , _pgUserName = O.constant _userName
  , _pgUserAuthor = O.constant _userAuthor
  , _pgCreatedAt = Nothing
  , _pgUpdatedAt = Nothing
  }


mkUserWrite' :: UserInsert -> TA.Author -> UserWrite
mkUserWrite' User{..} author = PGUser
  { _pgUserID = Nothing
  , _pgUserName = O.constant _userName
  , _pgUserAuthor = O.constant $ TA.authorID author
  , _pgCreatedAt = Nothing
  , _pgUpdatedAt = Nothing
  }


mkUserPut :: Int -> UserPut' -> UserPut
mkUserPut uID User{..} = User
  { _userID = uID
  , _userName = _userName
  , _userAuthor = _userAuthor
  , _createdAt = ()
  , _updatedAt = ()
  }


userID :: User' Int b c d e -> Int
userID = _userID


pgUserID :: PGUser' Int b c d e -> Int
pgUserID = _pgUserID


userColID :: PGUser' (O.Column O.PGInt4) b c d e -> O.Column O.PGInt4
userColID = _pgUserID


userAuthorID :: PGUser -> Int
userAuthorID = _pgUserAuthor


userName :: User' a Text.Text c d e -> Text.Text
userName = _userName

-- JSON

instance Aeson.ToJSON User where
  toJSON User{..} = Aeson.object
    [ "id" .= _userID
    , "name" .= _userName
    , "author" .= _userAuthor
    , "created-at" .= _createdAt
    , "updated-at" .= _updatedAt
    , "type" .= ("user" :: Text.Text)
    , "link" .= ((Text.pack $ "/user/" <> show _userID) :: Text.Text)
    ]


instance Aeson.ToJSON UserS where
  toJSON User{..} = Aeson.object
    [ "id" .= _userID
    , "type" .= ("user" :: Text.Text)
    , "link" .= ((Text.pack $ "/user/" <> show _userID) :: Text.Text)
    ]


instance Aeson.FromJSON UserInsert where
  parseJSON = Aeson.withObject "user" $ \o -> User
    <$> pure ()
    <*> o .: "name"
    <*> pure ()
    <*> pure ()
    <*> pure ()


instance Aeson.FromJSON UserPut where
  parseJSON = Aeson.withObject "user" $ \o -> User
    <$> o .: "id"
    <*> o .: "name"
    <*> o .: "author"
    <*> pure ()
    <*> pure ()


instance Aeson.FromJSON UserPut' where
  parseJSON = Aeson.withObject "user" $ \o -> User
    <$> pure ()
    <*> o .: "name"
    <*> o .: "author"
    <*> pure ()
    <*> pure ()


-- Valid Request Hints

validUserInsertObject :: Aeson.Value
validUserInsertObject = Aeson.object
  [ "name" .= ("The name you want to give to the user you are creating" :: Text.Text)
  , "auhor" .= ("The author id (will create a new author" :: Text.Text)
  ]


validUserPutObject :: Aeson.Value
validUserPutObject = Aeson.object
  [ "id" .= ("The id of the user which should be in the DB" :: Text.Text)
  , "name" .= ("The name you want to give to the user with the above id" :: Text.Text)
  ]



-- Query Params Processing

data UserIncludes
  = IAuthor
  deriving (Show, Eq)


instance CI.Includes UserIncludes where
  getAll = [IAuthor]

  fromCSV :: Text.Text -> Either TAe.ClientError [UserIncludes]
  fromCSV =
    let
      fromString :: Text.Text -> Either TAe.ClientError UserIncludes
      fromString "author" = Right IAuthor
      fromString _ = Left TAe.InvalidQueryParams
      
      f :: ([TAe.ClientError], [UserIncludes]) -> Either TAe.ClientError [UserIncludes]
      f (x:_, _) = Left x
      f (_, ys) = Right ys
   in
      f . DEither.partitionEithers . map fromString . Text.splitOn ","
