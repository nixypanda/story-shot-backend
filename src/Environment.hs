{-# OPTIONS_GHC -Wall #-}

module Environment
  ( Environment(..)
  , EnvVars(..)
  , readEnv
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import qualified Database.PostgreSQL.Simple as PGS

import qualified Utils


data Environment
  = Development
  | Production
  | Test
  deriving (Eq, Read, Show)


data EnvVars = EnvVars
  { dbConfig :: PGS.ConnectInfo
  , appPort :: Int
  , environ :: Environment
  , dbPassSalt :: B.ByteString
  }


getStaticSalt :: IO B.ByteString
getStaticSalt =
  BC.pack <$> Utils.fromMaybeEnv "510ry5h01" "STATIC_SALT"


getEnvironment :: IO Environment
getEnvironment =
  Utils.fromMaybeEnv Development "SCOTTY_ENV"


getDBConfig :: IO PGS.ConnectInfo
getDBConfig = PGS.ConnectInfo
  <$> Utils.fromMaybeEnv "localhost" "DB_URL"
  <*> Utils.fromMaybeEnv 5432 "DB_PORT"
  <*> Utils.fromMaybeEnv "sherub" "DB_USER"
  <*> Utils.fromMaybeEnv "story-shot" "DB_PASS"
  <*> Utils.fromMaybeEnv "story-shot" "DB_NAME"


-- Read Environment

readEnv :: IO EnvVars
readEnv = EnvVars
  <$> getDBConfig
  <*> Utils.fromMaybeEnv 8081 "SCOTTY_PORT"
  <*> getEnvironment
  <*> getStaticSalt
