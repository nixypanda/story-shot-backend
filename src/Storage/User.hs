{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}


module Storage.User
  ( getUsers
  , getUser
  , getMultiUsers
  , userQuery
  , createUser
  , createUsers
  , updateUser
  , updateUsers
  , deleteUser
  , deleteUsers
  ) where


import qualified Control.Arrow as Arrow
import qualified Data.Int as DI
import qualified Data.Maybe as DM

import qualified Opaleye as O

import qualified Type.Pagination as TP
import qualified Type.User as TU
import qualified Type.Author as TA
import qualified Init as I
import qualified Storage.Utils as SU



-- CREATE

createUser :: TU.UserInsert -> TA.Author -> I.AppT TU.PGUser
createUser user author =
  head <$> createUsers [user] [author]


createUsers :: [TU.UserInsert] -> [TA.Author] -> I.AppT [TU.PGUser]
createUsers users authors =
  SU.runDBInsertR TU.userTable (zipWith TU.mkUserWrite' users authors) id



-- RETRIVE

userQuery :: O.Query TU.UserRead
userQuery = O.queryTable TU.userTable


multiUserQuery :: [Int] -> O.Query TU.UserRead
multiUserQuery aids = proc () -> do
  row <- userQuery -< ()
  O.restrict -< map O.constant aids `O.in_` TU.userColID row
  Arrow.returnA -< row


cursorPaginatedUserQuery :: TP.CursorParam -> O.Query TU.UserRead
cursorPaginatedUserQuery TP.CursorParam{..} = O.limit sizeCursor $ proc () -> do
  row <- userQuery -< ()
  O.restrict -< TU.userColID row O..> O.constant nextCursor
  Arrow.returnA -< row


singleUser :: Int -> O.Query TU.UserRead
singleUser idA = proc () -> do
  row <- userQuery -< ()
  O.restrict -< TU.userColID row O..== O.constant idA
  Arrow.returnA -< row


getUsers :: TP.CursorParam -> I.AppT [TU.PGUser]
getUsers = SU.runDB . cursorPaginatedUserQuery


getUser :: Int -> I.AppT (Maybe TU.PGUser)
getUser = fmap DM.listToMaybe . SU.runDB . singleUser


getMultiUsers :: [Int] -> I.AppT [TU.PGUser]
getMultiUsers = SU.runDB . multiUserQuery



-- UPDATE

updateUsers :: [TU.UserPut] -> I.AppT [TU.PGUser]
updateUsers =
  fmap DM.catMaybes . mapM updateUser


updateUser :: TU.UserPut -> I.AppT (Maybe TU.PGUser)
updateUser user =
  let
    updateF _ = TU.mkUserWrite user
    predicate aRow = TU.userColID aRow O..== O.constant (TU.userID user)
  in
    DM.listToMaybe <$> SU.runDBUpdateR TU.userTable updateF predicate id



-- DELETE

deleteUser :: Int -> I.AppT DI.Int64
deleteUser id' = SU.runDBDelete TU.userTable (\aic -> TU.userColID aic O..== O.constant id')


deleteUsers :: [Int] -> I.AppT DI.Int64
deleteUsers =
  fmap sum . mapM deleteUser
