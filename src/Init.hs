{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Init
  ( Error
  , ActionA
  , ScottyA
  , WithConfig
  , Config(..)
  , Environment(..)
  , getConfig
  , loggingM
  , getOptions
  ) where

{-
   I had no idea where to shove these functions/types/etc. so here it goes.
 -}

import Control.Monad.Reader (ReaderT)

import Data.Default (def)
import Data.Pool (Pool, createPool)
import Data.Text.Lazy (Text)
import Database.PostgreSQL.Simple (Connection, connect, close)
import Network.Wai.Handler.Warp
  ( Settings
  , defaultSettings
  , setPort
  )
import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Web.Scotty.Trans
  ( ActionT
  , Options
  , ScottyT
  , settings
  , verbose
  )

import Environment (Environment(..), EnvVars(..), readEnv)


-- TYPES

type WithConfig = ReaderT Config IO
type ActionA = ActionT Error WithConfig ()
type ScottyA = ScottyT Error WithConfig ()
type Error = Text

-- Setup

data Config = Config
  { connPool :: Pool Connection
  , port :: Int
  , environment :: Environment
  }


-- Get all the required configuration to run this application

getConfig :: IO Config
getConfig = do
  env <- readEnv
  pgPool <- createPool (connect $ dbConfig env) close 1 10 10
  return Config
    { connPool = pgPool
    , port = appPort env
    , environment = environ env
    }


getOptions :: Config -> Options
getOptions c@Config{..} = def
  { settings = getSettings c
  , verbose = fromEnum $ environment == Development
  }


-- HELPERS --

loggingM :: Environment -> Middleware
loggingM Development = logStdoutDev
loggingM Production = logStdout
loggingM Test = id


-- Application Config

getSettings :: Config -> Settings
getSettings Config{..} =
  setPort port defaultSettings
