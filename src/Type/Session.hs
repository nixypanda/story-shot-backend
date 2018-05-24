{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Type.Session
  ( Session
  , PGSession
  , sessionTable
  , mkLinkedSession
  , mkSessionForUser
  ) where


import Data.Monoid ((<>))
import Data.Aeson ((.=))

import qualified Data.Time as DT
import qualified GHC.Generics as Generics

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Profunctor.Product.TH as ProductProfunctor
import qualified Opaleye as O

import qualified Class.Resource as CR
import qualified Type.Or as Or
import qualified Type.User as TU


data Session' sessionID displayID user createdAt updatedAt = Session
  { _sessionID :: sessionID
  , _sessionDisplayID :: displayID
  , _sessionUser :: user
  , _createdAt :: createdAt
  , _updatedAt :: updatedAt
  } deriving (Eq, Show, Generics.Generic)


data PGSession' sessionID displayID user createdAt updatedAt = PGSession
  { _pgSessionID :: sessionID
  , _pgSessionDisplayID :: displayID
  , _pgSessionUser :: user
  , _pgCreatedAt :: createdAt
  , _pgUpdatedAt :: updatedAt
  } deriving (Eq, Show, Generics.Generic)


type Session = Session' Int Text.Text (Or.Or TU.UserS TU.User) DT.UTCTime DT.UTCTime

type PGSession = PGSession' Int Text.Text Int DT.UTCTime DT.UTCTime
type SessionWrite = PGSession'
  (Maybe (O.Column O.PGInt4))
  (O.Column O.PGText)
  (O.Column O.PGInt4)
  (Maybe (O.Column O.PGTimestamptz))
  (Maybe (O.Column O.PGTimestamptz))
type SessionRead = PGSession'
  (O.Column O.PGInt4)
  (O.Column O.PGText)
  (O.Column O.PGInt4)
  (O.Column O.PGTimestamptz)
  (O.Column O.PGTimestamptz)


-- Magic
$(ProductProfunctor.makeAdaptorAndInstance "pSession" ''PGSession')


sessionTable :: O.Table SessionWrite SessionRead
sessionTable = O.Table "sessions" $ pSession
  PGSession
    { _pgSessionID = O.optional "id"
    , _pgSessionDisplayID = O.required "session_id"
    , _pgSessionUser = O.required "user_"
    , _pgCreatedAt = O.optional "created_at"
    , _pgUpdatedAt = O.optional "updated_at"
    }


instance CR.Resource Session where
  rid = _sessionID
  type' _ = "session" :: Text.Text
  createdAt = _createdAt
  updatedAt = _updatedAt


mkLinkedSession :: PGSession -> Or.Or TU.UserS TU.User -> Session
mkLinkedSession PGSession{..} user' = Session
  { _sessionID = _pgSessionID
  , _sessionDisplayID = _pgSessionDisplayID
  , _sessionUser = user'
  , _createdAt = _pgCreatedAt
  , _updatedAt = _pgUpdatedAt
  }

mkSessionForUser :: Int -> String -> SessionWrite
mkSessionForUser userID session = PGSession
  { _pgSessionID = Nothing
  , _pgSessionDisplayID = O.constant session
  , _pgSessionUser = O.constant userID
  , _pgCreatedAt = Nothing
  , _pgUpdatedAt = Nothing
  }

-- JSON

instance Aeson.ToJSON Session where
  toJSON Session{..} = Aeson.object
    [ "id" .= _sessionID
    , "session-id" .= _sessionDisplayID
    , "user" .= _sessionUser
    , "created-at" .= _createdAt
    , "updated-at" .= _updatedAt
    , "type" .= ("session" :: Text.Text)
    , "link" .= ((Text.pack $ "/session/" <> show _sessionID) :: Text.Text)
    ]
