{-# OPTIONS_GHC -Wall #-}

module Environment
  ( Environment(..)
  , EnvVars(..)
  , readEnv
  ) where

import Database.PostgreSQL.Simple (ConnectInfo(..))

import Utils (fromMaybeEnv)


data Environment
  = Development
  | Production
  | Test
  deriving (Eq, Read, Show)


data EnvVars = EnvVars
  { dbConfig :: ConnectInfo
  , appPort :: Int
  , environ :: Environment
  }


getEnvironment :: IO Environment
getEnvironment =
  fromMaybeEnv Development "SCOTTY_ENV"


getDBConfig :: IO ConnectInfo
getDBConfig = ConnectInfo
  <$> fromMaybeEnv "localhost" "DB_URL"
  <*> fromMaybeEnv 5432 "DB_PORT"
  <*> fromMaybeEnv "sherub" "DB_USER"
  <*> fromMaybeEnv "story-shot" "DB_PASS"
  <*> fromMaybeEnv "story-shot" "DB_NAME"


-- Read Environment

readEnv :: IO EnvVars
readEnv = EnvVars
  <$> getDBConfig
  <*> fromMaybeEnv 8081 "SCOTTY_PORT"
  <*> getEnvironment
