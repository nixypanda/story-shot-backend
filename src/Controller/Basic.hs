{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Controller.Basic
  ( defaultH
  , notFoundA
  , indexRoute
  , health
  , invalidPayload
  ) where


import Data.Aeson
  ( Value(..)
  , object
  , (.=)
  )
import Network.HTTP.Types.Status
  ( status200
  , status400
  , notFound404
  , status500
  )
import Web.Scotty.Trans
  ( ActionT
  , finish
  , json
  , status
  , showError
  )

import Init
  ( ActionA
  , Error
  , Environment(..)
  , WithConfig
  )


defaultH :: Environment -> Error -> ActionA
defaultH env err = do
  status status500
  json $ case env of
       Development -> object ["error" .= showError err]
       Production -> Null
       Test -> object ["error" .= showError err]


notFoundA :: ActionA
notFoundA = do
  status notFound404
  json $ object ["error" .= String "No rule defined"]

indexRoute :: ActionA
indexRoute =
  json $ object ["app" .= String "Story Shot"]


health :: ActionA
health = do
  status status200
  finish


invalidPayload :: a -> ActionT Error WithConfig b
invalidPayload _ = do
  status status400
  json $ object ["error" .= String "Invalid payload"]
  finish
