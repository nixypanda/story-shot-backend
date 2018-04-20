{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Controller.Basic
  ( defaultH
  , notFoundA
  , indexRoute
  , health
  , invalidPayload
  ) where


import qualified Data.Aeson as DA
import qualified Network.HTTP.Types.Status as HTTPStatus
import qualified Web.Scotty.Trans as WST

import qualified Init as I


defaultH :: I.Environment -> I.Error -> I.ActionA
defaultH env err = do
  WST.status HTTPStatus.status500
  WST.json $ case env of
       I.Development -> DA.object ["error" DA..= WST.showError err]
       I.Production -> DA.Null
       I.Test -> DA.object ["error" DA..= WST.showError err]


notFoundA :: I.ActionA
notFoundA = do
  WST.status HTTPStatus.notFound404
  WST.json $ DA.object ["error" DA..= DA.String "No rule defined"]

indexRoute :: I.ActionA
indexRoute =
  WST.json $ DA.object ["app" DA..= DA.String "Story Shot"]


health :: I.ActionA
health = do
  WST.status HTTPStatus.status200
  WST.finish


invalidPayload :: DA.Value -> a -> WST.ActionT I.Error I.WithConfig b
invalidPayload example _ = do
  WST.status HTTPStatus.status400
  WST.json $ DA.object ["error" DA..= DA.String "Invalid payload", "schema" DA..= example]
  WST.finish
