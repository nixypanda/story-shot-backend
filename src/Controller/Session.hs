{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Controller.Session
  ( login
  ) where


import qualified Control.Monad.Trans as MonadT

import qualified Web.Scotty.Trans as Scotty
import qualified Network.HTTP.Types.Status as HTTPStatus

import qualified Init as I
import qualified Type.Meta as TM
import qualified Type.Doc as TD
import qualified Type.Session as TS
import qualified Type.AppError as TAe
import qualified Resource.Session as RS
import qualified Controller.Utils as CU
import qualified Library.Auth as Auth


-- TODO: Monad Transformers to clean up this repeted shit

login :: I.ActionA
login = do
  r <- Scotty.request
  let
    auth = Auth.extractAuth r

  res :: TD.MaybeResource TS.Session <-
    case auth of
      Left e -> do
        Scotty.status HTTPStatus.status401
        return . Left $ TAe.docError e

      Right (username, password) -> do
        includes <- CU.extractIncludes
        case includes of
          Left e -> do
            Scotty.status HTTPStatus.status400
            return . Left $ TAe.docError e

          Right incs -> do
            sessionResource <- MonadT.lift $ RS.createSessionFromDetails incs username password
            case sessionResource of
              Left e -> do
                Scotty.status HTTPStatus.status400
                return . Left $ TAe.docError e

              Right sr ->
                return . Right $ TM.indexDoc' sr

  either Scotty.json Scotty.json res
