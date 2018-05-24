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
  , userTable
  , userName
  , userColName
  , userPass
  , getSalt
  -- , mkUserWrite
  , mkUserFromDB
  , mkUserWrite'
  , mkUserPut
  , mkLinkedUser
  , mkUserS
  , userID
  , pgUserID
  , pgUserPass
  , userAuthorID
  , userColID
  , validUserPutObject
  , validUserInsertObject
  ) where


import Data.Monoid ((<>))
import Data.Aeson ((.=), (.:))

import qualified Data.Time as DT
import qualified GHC.Generics as Generics
import qualified Data.ByteString as B

import qualified Data.Text as Text
import qualified Data.Profunctor.Product.TH as ProductProfunctor
import qualified Data.Aeson as Aeson
import qualified Opaleye as O

import qualified Class.Resource as CR
import qualified Type.Or as TO
import qualified Type.Author as TA



-- Strangely Polymorphic data type (Internal Use)

data User' id' username displayName password author createdAt updatedAt = User
  { _userID :: id'
  , _userName :: username
  , _userDN :: displayName
  , _userPass :: password
  , _userAuthor :: author
  , _createdAt :: createdAt
  , _updatedAt :: updatedAt
  } deriving (Eq, Show, Generics.Generic)


data PGUser' userID userName displayName password author salt createdAt updatedAt = PGUser
  { _pgUserID :: userID
  , _pgUserName :: userName
  , _pgUserDN :: displayName
  , _pgUserPass :: password
  , _pgUserAuthor :: author
  , _pgDynamicSalt :: salt
  , _pgCreatedAt :: createdAt
  , _pgUpdatedAt :: updatedAt
  } deriving (Eq, Show, Generics.Generic)



-- Types that Will be used
type User = User' Int Text.Text Text.Text B.ByteString (TO.Or TA.AuthorS TA.Author) DT.UTCTime DT.UTCTime
type UserS = User' Int () () () () () ()
type UserPut = User' Int (Maybe Text.Text) (Maybe Text.Text) (Maybe Text.Text) (Maybe Int) () ()
type UserPut' = User' () (Maybe Text.Text) (Maybe Text.Text) (Maybe Text.Text) (Maybe Int) () ()
type UserInsert = User' () Text.Text Text.Text Text.Text () () ()

type PGUser = PGUser' Int Text.Text Text.Text B.ByteString Int B.ByteString DT.UTCTime DT.UTCTime
type UserWrite = PGUser'
  (Maybe (O.Column O.PGInt4))
  (O.Column O.PGText)
  (O.Column O.PGText)
  (O.Column O.PGBytea)
  (O.Column O.PGInt4)
  (O.Column O.PGBytea)
  (Maybe (O.Column O.PGTimestamptz))
  (Maybe (O.Column O.PGTimestamptz))
type UserRead = PGUser'
  (O.Column O.PGInt4)
  (O.Column O.PGText)
  (O.Column O.PGText)
  (O.Column O.PGBytea)
  (O.Column O.PGInt4)
  (O.Column O.PGBytea)
  (O.Column O.PGTimestamptz)
  (O.Column O.PGTimestamptz)


instance CR.Resource User where
  rid = _userID
  type' _ = "user" :: Text.Text
  createdAt = _createdAt
  updatedAt = _updatedAt


instance CR.UnlinkedResource PGUser where
  urid = _pgUserID

instance CR.LinkedResource User where
  lrid = _userID


mkLinkedUser :: PGUser -> TO.Or TA.AuthorS TA.Author -> User
mkLinkedUser PGUser{..} author' = User
  { _userID = _pgUserID
  , _userName = _pgUserName
  , _userDN = _pgUserDN
  , _userPass = _pgUserPass
  , _userAuthor = author'
  , _createdAt = _pgCreatedAt
  , _updatedAt = _pgUpdatedAt
  }


-- Magic
$(ProductProfunctor.makeAdaptorAndInstance "pUser" ''PGUser')


-- Opaleye table binding
userTable :: O.Table UserWrite UserRead
userTable = O.Table "users" $ pUser
  PGUser
    { _pgUserID = O.optional "id"
    , _pgUserName = O.required "username"
    , _pgUserDN = O.required "displayname"
    , _pgUserPass = O.required "password"
    , _pgUserAuthor = O.required "author"
    , _pgDynamicSalt = O.required "salt"
    , _pgCreatedAt = O.optional "created_at"
    , _pgUpdatedAt = O.optional "updated_at"
    }



-- Some Helpers

mkUserS :: Int -> UserS
mkUserS uid = User
  { _userID     = uid
  , _userName   = ()
  , _userDN     = ()
  , _userPass   = ()
  , _userAuthor = ()
  , _createdAt  = ()
  , _updatedAt  = ()
  }


mkUserFromDB :: PGUser -> TO.Or TA.AuthorS TA.Author -> User
mkUserFromDB PGUser{..} author' = User
  { _userID = _pgUserID
  , _userName = _pgUserName
  , _userDN = _pgUserDN
  , _userPass = _pgUserPass
  , _userAuthor = author'
  , _createdAt = _pgCreatedAt
  , _updatedAt = _pgUpdatedAt
  }


-- mkUserWrite :: UserPut -> UserRead -> UserWrite
-- mkUserWrite User{..} PGUser{..} = PGUser
--   { _pgUserID      = O.constant $ Just _userID
--   , _pgUserName    = maybe _pgUserName O.constant _userName
--   , _pgUserDN      = maybe _pgUserDN O.constant _userDN
--   , _pgUserPass    = maybe _pgUserPass O.constant _userPass
--   , _pgUserAuthor  = maybe _pgUserAuthor O.constant _userAuthor
--   , _pgDynamicSalt = _pgDynamicSalt
--   , _pgCreatedAt   = Nothing
--   , _pgUpdatedAt   = Nothing
--   }


mkUserWrite' :: UserInsert -> TA.Author -> B.ByteString -> B.ByteString -> UserWrite
mkUserWrite' User{..} author salt hashedPassword = PGUser
  { _pgUserID = Nothing
  , _pgUserName = O.constant _userName
  , _pgUserDN = O.constant _userDN
  , _pgUserPass = O.constant hashedPassword
  , _pgUserAuthor = O.constant $ TA.authorID author
  , _pgDynamicSalt = O.constant salt
  , _pgCreatedAt = Nothing
  , _pgUpdatedAt = Nothing
  }


mkUserPut :: Int -> UserPut' -> UserPut
mkUserPut uID User{..} = User
  { _userID = uID
  , _userName = _userName
  , _userDN = _userDN
  , _userPass = _userPass
  , _userAuthor = _userAuthor
  , _createdAt = ()
  , _updatedAt = ()
  }


userID :: User' Int b c d e f g -> Int
userID = _userID


pgUserID :: PGUser' Int b c d e f g h -> Int
pgUserID = _pgUserID


pgUserPass :: PGUser -> B.ByteString
pgUserPass = _pgUserPass


userPass :: User' a b c Text.Text e f g -> Text.Text
userPass = _userPass


getSalt :: PGUser -> B.ByteString
getSalt = _pgDynamicSalt


userColID :: PGUser' (O.Column O.PGInt4) b c d e f g h -> O.Column O.PGInt4
userColID = _pgUserID


userAuthorID :: PGUser -> Int
userAuthorID = _pgUserAuthor


userName :: User' a Text.Text c d e f g -> Text.Text
userName = _userName

userColName :: PGUser' a (O.Column O.PGText) c d e f g h -> O.Column O.PGText
userColName = _pgUserName


-- JSON

instance Aeson.ToJSON User where
  toJSON User{..} = Aeson.object
    [ "id" .= _userID
    , "username" .= _userName
    , "display-name" .= _userDN
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
    <*> o .: "username"
    <*> o .: "display-name"
    <*> (o .: "password")
    <*> pure ()
    <*> pure ()
    <*> pure ()


instance Aeson.FromJSON UserPut where
  parseJSON = Aeson.withObject "user" $ \o -> User
    <$> o .: "id"
    <*> o .: "username"
    <*> o .: "display-name"
    <*> o .: "password"
    <*> o .: "author"
    <*> pure ()
    <*> pure ()


instance Aeson.FromJSON UserPut' where
  parseJSON = Aeson.withObject "user" $ \o -> User
    <$> pure ()
    <*> o .: "username"
    <*> o .: "display-name"
    <*> o .: "password"
    <*> o .: "author"
    <*> pure ()
    <*> pure ()


-- Valid Request Hints

validUserInsertObject :: Aeson.Value
validUserInsertObject = Aeson.object
  [ "username" .= ("The unique identifier used for tagging etc." :: Text.Text)
  , "display-name" .= ("The name used for dispaly purposes" :: Text.Text)
  , "password" .= ("The passord for the user (stored as is for now)" :: Text.Text)
  ]


validUserPutObject :: Aeson.Value
validUserPutObject = Aeson.object
  [ "id" .= ("The id of the user which should be in the DB" :: Text.Text)
  , "username" .= ("The unique identifier used for tagging etc." :: Text.Text)
  , "display-name" .= ("The name used for dispaly purposes" :: Text.Text)
  , "password" .= ("The passord for the user (stored as is for now)" :: Text.Text)
  ]

