{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Init
  ( Error
  , ActionA
  , ScottyA
  , WithConfig
  , Config(..)
  , E.Environment(..)
  , getConfig
  , loggingM
  , getOptions
  ) where

{-
   I had no idea where to shove these functions/types/etc. so here it goes.
 -}

import qualified Control.Monad.Reader as ReaderTrans

import qualified Data.Default as Default
import qualified Data.Pool as Pool
import qualified Data.Text.Lazy as LazyText
import qualified Database.PostgreSQL.Simple as PGS
import qualified Network.Wai.Handler.Warp as NetworkHandler
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.RequestLogger as MiddlewareLogging
import qualified Web.Scotty.Trans as Scotty

import qualified Environment as E


-- TYPES

type WithConfig = ReaderTrans.ReaderT Config IO
type ActionA    = Scotty.ActionT Error WithConfig ()
type ScottyA    = Scotty.ScottyT Error WithConfig ()
type Error      = LazyText.Text

-- Setup

data Config = Config
  { connPool :: Pool.Pool PGS.Connection
  , port :: Int
  , environment :: E.Environment
  }


-- Get all the required configuration to run this application

getConfig :: IO Config
getConfig = do
  env <- E.readEnv
  pgPool <- Pool.createPool (PGS.connect $ E.dbConfig env) PGS.close 1 10 10
  return Config
    { connPool = pgPool
    , port = E.appPort env
    , environment = E.environ env
    }


getOptions :: Config -> Scotty.Options
getOptions c@Config{..} = Default.def
  { Scotty.settings = getSettings c
  , Scotty.verbose = fromEnum $ environment == E.Development
  }


-- HELPERS --

loggingM :: E.Environment -> Wai.Middleware
loggingM E.Development = MiddlewareLogging.logStdoutDev
loggingM E.Production  = MiddlewareLogging.logStdout
loggingM E.Test = id


-- Application Config

getSettings :: Config -> NetworkHandler.Settings
getSettings Config{..} =
  NetworkHandler.setPort port NetworkHandler.defaultSettings
