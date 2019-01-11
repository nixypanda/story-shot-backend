{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Lib
  ( startServer
  ) where

import qualified Control.Monad.Trans.Reader  as ReaderTrans

import qualified Network.Wai                 as WAI
import qualified Network.Wai.Middleware.Cors as WaiCORS
import qualified Web.Scotty.Trans            as Scotty

import qualified Controller.Author           as AC
import qualified Controller.Basic            as CB
import qualified Controller.Session          as SessionC
import qualified Controller.Story            as SC
import qualified Controller.Tag              as TC
import qualified Controller.User             as UC

import qualified Init                        as I


corsPolicy :: WaiCORS.CorsResourcePolicy
corsPolicy = WaiCORS.simpleCorsResourcePolicy
  { WaiCORS.corsRequestHeaders = ["Authorization"]
  }


corsMiddleware :: WAI.Middleware
corsMiddleware = WaiCORS.cors $ const $ Just corsPolicy


application :: I.Config -> I.ScottyA
application c = do
  let e = I.environment c

  Scotty.middleware $ I.loggingM e
  Scotty.middleware corsMiddleware
  Scotty.defaultHandler $ CB.defaultH e

  Scotty.get "/" CB.indexRoute
  Scotty.get "/health" CB.health

  Scotty.post   "/story"        SC.post
  Scotty.get    "/story"        SC.getBatch
  Scotty.get    "/story/:id"    SC.get
  Scotty.get    "/story/random" SC.getRandom
  Scotty.put    "/story"        SC.putBatch
  Scotty.put    "/story/:id"    SC.put
  Scotty.delete "/story"        SC.deleteBatch
  Scotty.delete "/story/:id"    SC.delete

  Scotty.post   "/tag"     TC.post
  Scotty.get    "/tag"     TC.getBatch
  Scotty.get    "/tag/:id" TC.get
  Scotty.put    "/tag"     TC.putBatch
  Scotty.put    "/tag/:id" TC.put
  Scotty.delete "/tag"     TC.deleteBatch
  Scotty.delete "/tag/:id" TC.delete

  Scotty.post   "/author"     AC.post
  Scotty.get    "/author"     AC.getBatch
  Scotty.get    "/author/:id" AC.get
  Scotty.put    "/author"     AC.putBatch
  Scotty.put    "/author/:id" AC.put
  Scotty.delete "/author"     AC.deleteBatch
  Scotty.delete "/author/:id" AC.delete

  Scotty.post   "/user"       UC.post
  Scotty.get    "/user"       UC.getBatch
  Scotty.get    "/user/:id"   UC.get
  Scotty.put    "/user"       UC.putBatch
  Scotty.put    "/user/:id"   UC.put
  Scotty.delete "/user"       UC.deleteBatch
  Scotty.delete "/user/:id"   UC.delete
  Scotty.get    "/user/login" SessionC.login

  Scotty.notFound CB.notFoundA


startServer :: IO ()
startServer = do
  appConfig <- I.getConfig
  let
    opts = I.getOptions appConfig
    r m = ReaderTrans.runReaderT m appConfig

  Scotty.scottyOptsT opts r $ application appConfig

