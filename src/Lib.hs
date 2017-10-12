{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( startServer
  ) where

import Control.Monad.Trans.Reader (runReaderT)

import Network.Wai.Middleware.Cors (simpleCors)
import Web.Scotty.Trans

import Controller.Basic

import qualified Controller.Author as AC
import qualified Controller.Tag as TC
import qualified Controller.Story as SC

import Init
  ( Config(..)
  , ScottyA
  , getConfig
  , loggingM
  , getOptions
  )

application :: Config -> ScottyA
application c = do
  let e = environment c

  middleware $ loggingM e
  middleware simpleCors
  defaultHandler $ defaultH e

  get "/" indexRoute
  get "/health" health

  post   "/story"     SC.post
  get    "/story"     SC.getBatch
  get    "/story/:id" SC.get
  put    "/story"     SC.putBatch
  put    "/story/:id" SC.put
  delete "/story"     SC.deleteBatch
  delete "/story/:id" SC.delete

  post   "/tag"     TC.post
  get    "/tag"     TC.getBatch
  get    "/tag/:id" TC.get
  put    "/tag"     TC.putBatch
  put    "/tag/:id" TC.put
  delete "/tag"     TC.deleteBatch
  delete "/tag/:id" TC.delete

  post   "/author"     AC.post
  get    "/author"     AC.getBatch
  get    "/author/:id" AC.get
  put    "/author"     AC.putBatch
  put    "/author/:id" AC.put
  delete "/author"     AC.deleteBatch
  delete "/author/:id" AC.delete

  notFound notFoundA


startServer :: IO ()
startServer = do
  appConfig <- getConfig
  let
    opts = getOptions appConfig
    r m = runReaderT m appConfig

  scottyOptsT opts r $ application appConfig

