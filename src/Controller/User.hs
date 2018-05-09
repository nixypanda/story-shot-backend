{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Controller.User
  ( post
  , postBatch
  , getBatch
  , get
  , putBatch
  , put
  , deleteBatch
  , delete
  ) where


import qualified Control.Monad.Trans as MonadT

import qualified Data.Aeson as DA
import qualified Web.Scotty.Trans as Scotty

import qualified Init as I
import qualified Type.User as TU
import qualified Resource.User as RA
import qualified Controller.Utils as CU



-- CREATE

post :: I.ActionA
post = do
  includes <- CU.extractIncludes
  user' :: TU.UserInsert <- CU.extractData TU.validUserInsertObject
  userResource <- MonadT.lift $ RA.createUserResource includes user'
  either Scotty.json Scotty.json userResource


postBatch :: I.ActionA
postBatch = do
  includes <- CU.extractIncludes
  users :: [TU.UserInsert] <- CU.extractData TU.validUserInsertObject
  userResources <- MonadT.lift $ RA.createUserResources includes users
  either Scotty.json Scotty.json userResources



-- RETRIVE

getBatch :: I.ActionA
getBatch = do
  qparams <- Scotty.params
  includes <- CU.extractIncludes
  ar <- MonadT.lift $ RA.getUserResources (CU.cursorPagination qparams) includes
  either Scotty.json Scotty.json ar


get :: I.ActionA
get = do
  userId' <- Scotty.param "id"
  includes <- CU.extractIncludes
  userResource <- MonadT.lift $ RA.getUserResource userId' includes
  either Scotty.json Scotty.json userResource



-- UPDATE

put :: I.ActionA
put = do
  userId' :: Int <- Scotty.param "id"
  user' :: TU.UserPut' <- CU.extractData TU.validUserInsertObject
  let
    user'' = TU.mkUserPut userId' user'

  userResource <- MonadT.lift $ RA.updateUserResource user''
  either Scotty.json Scotty.json userResource


putBatch :: I.ActionA
putBatch = do
  users :: [TU.UserPut] <- CU.extractData TU.validUserPutObject
  userResources <- MonadT.lift $ RA.updateUserResources users
  Scotty.json userResources



-- DELETE

deleteBatch :: I.ActionA
deleteBatch = do
  users :: [Int] <- CU.extractData deleteBatchExample
  userResources <- MonadT.lift $ RA.deleteUserResources users
  Scotty.json userResources


delete :: I.ActionA
delete = do
  userId' <- Scotty.param "id"
  userResource <- MonadT.lift $ RA.deleteUserResource userId'
  either Scotty.json Scotty.json userResource



-- HELPERS

deleteBatchExample :: DA.Value
deleteBatchExample = undefined
