{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings   #-}
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

import qualified Web.Scotty.Trans    as Scotty

import qualified Controller.Utils    as CU
import qualified Init                as I
import qualified Resource.User       as RU
import qualified Type.Doc            as TD
import qualified Type.Meta           as TM
import qualified Type.User           as TU



-- CREATE

post :: I.ActionA
post = do
  includes <- CU.extractIncludes
  userInsertObj :: TU.UserInsert <- CU.extractData TU.validUserInsertObject
  userResource :: TD.MaybeResource TU.User <- CU.executeAction includes (\incs ->
    fmap (Right . TM.indexDoc') . MonadT.lift $ RU.createUser incs userInsertObj)
  either Scotty.json Scotty.json userResource


postBatch :: I.ActionA
postBatch = do
  includes <- CU.extractIncludes
  userInsertObjs :: [TU.UserInsert] <- CU.extractData TU.validUserInsertObject
  userResources :: TD.MaybeResource TU.User <- CU.executeAction includes (\incs ->
    fmap (Right . TM.docMulti) . MonadT.lift $ RU.createUsers incs userInsertObjs)
  either Scotty.json Scotty.json userResources



-- RETRIVE

getBatch :: I.ActionA
getBatch = do
  qparams <- Scotty.params
  includes <- CU.extractIncludes
  userResources :: TD.MaybeResource TU.User <- CU.executeAction includes (\incs ->
    fmap (Right . TM.docMulti) . MonadT.lift $ RU.getUsers (CU.cursorPagination qparams) incs)
  either Scotty.json Scotty.json userResources


get :: I.ActionA
get = do
  userId' <- Scotty.param "id"
  includes <- CU.extractIncludes
  userResource :: TD.MaybeResource TU.User <- CU.executeAction includes (\incs ->
    fmap TM.docOrError . MonadT.lift $ RU.getUser userId' incs)
  either Scotty.json Scotty.json userResource



-- UPDATE

put :: I.ActionA
put = do
  userId :: Int <- Scotty.param "id"
  userPutObj' :: TU.UserPut' <- CU.extractData TU.validUserInsertObject
  let userPutObj = TU.mkUserPut userId userPutObj'
  maybeUpdatedUser <- MonadT.lift $ RU.updateUser userPutObj
  let userResource = TM.docOrError maybeUpdatedUser
  either Scotty.json Scotty.json userResource


putBatch :: I.ActionA
putBatch = do
  userPutObjs :: [TU.UserPut] <- CU.extractData TU.validUserPutObject
  updatedUsers <- MonadT.lift $ RU.updateUsers userPutObjs
  let userResources = TM.docMulti updatedUsers
  Scotty.json userResources



-- DELETE

deleteBatch :: I.ActionA
deleteBatch = do
  userIds :: [Int] <- CU.extractData CU.deleteBatchExample
  totalDeleted <- MonadT.lift $ RU.deleteUsers userIds
  let userMetaInfo :: TD.MaybeResource TU.User = TM.metaDocFromInt totalDeleted
  Scotty.json userMetaInfo


delete :: I.ActionA
delete = do
  userId <- Scotty.param "id"
  totalDeleted <- MonadT.lift $ RU.deleteUsers userId
  let userMetaInfo :: TD.MaybeResource TU.User = TM.metaDocFromInt totalDeleted
  either Scotty.json Scotty.json userMetaInfo
