{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}


module Storage.User
  ( getUsers
  , getUser
  , getUserFromUsername
  , getMultiUsers
  , userQuery
  , createUser
  , createUsers
  , updateUser
  , updateUsers
  , deleteUser
  , deleteUsers
  ) where


import Opaleye ((.==), (.>))

import qualified Control.Arrow as Arrow
import qualified Control.Monad as M
import qualified Control.Monad.Trans as MonadT
import qualified Data.Int as DI
import qualified Data.Maybe as DM
import qualified Data.List as DL
import qualified Control.Monad.Trans.Reader as ReaderTrans

import qualified Opaleye as O
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encode

import qualified Type.Pagination as TP
import qualified Type.User as TU
import qualified Type.Author as TA
import qualified Init as I
import qualified Storage.Utils as SU
import qualified Library.Auth as Auth


-- CREATE

createUser :: TU.UserInsert -> TA.Author -> I.AppT TU.PGUser
createUser user author =
  head <$> createUsers [user] [author]


createUsers :: [TU.UserInsert] -> [TA.Author] -> I.AppT [TU.PGUser]
createUsers users authors = do
  let passwords = fmap (Encode.encodeUtf8 . TU.userPass) users
  dSalts <- MonadT.liftIO $ M.replicateM (length authors) Auth.createDynamicSalt
  sSalt <- ReaderTrans.asks I.staticSalt
  hashedPasswords <- MonadT.liftIO . sequence $ zipWith3 Auth.createPassword (repeat sSalt) dSalts passwords
  SU.runDBInsertR TU.userTable (DL.zipWith4 TU.mkUserWrite' users authors dSalts hashedPasswords) id



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
  O.restrict -< TU.userColID row .> O.constant nextCursor
  Arrow.returnA -< row


singleUser :: Int -> O.Query TU.UserRead
singleUser idA = proc () -> do
  row <- userQuery -< ()
  O.restrict -< TU.userColID row .== O.constant idA
  Arrow.returnA -< row


userFromUsername :: Text.Text -> O.Query TU.UserRead
userFromUsername username = proc () -> do
  row <- userQuery -< ()
  O.restrict -< TU.userColName row .== O.constant username
  Arrow.returnA -< row


getUsers :: TP.CursorParam -> I.AppT [TU.PGUser]
getUsers = SU.runDB . cursorPaginatedUserQuery


getUser :: Int -> I.AppT (Maybe TU.PGUser)
getUser = fmap DM.listToMaybe . SU.runDB . singleUser


getUserFromUsername :: Text.Text -> I.AppT (Maybe TU.PGUser)
getUserFromUsername = fmap DM.listToMaybe . SU.runDB . userFromUsername


getMultiUsers :: [Int] -> I.AppT [TU.PGUser]
getMultiUsers = SU.runDB . multiUserQuery



-- UPDATE

updateUsers :: [TU.UserPut] -> I.AppT [TU.PGUser]
updateUsers =
  fmap DM.catMaybes . mapM updateUser


updateUser :: TU.UserPut -> I.AppT (Maybe TU.PGUser)
updateUser = undefined
  -- let
  --   updateF _ = TU.mkUserWrite user
  --   predicate aRow = TU.userColID aRow .== O.constant (TU.userID user)
  -- in
  --   DM.listToMaybe <$> SU.runDBUpdateR TU.userTable updateF predicate id



-- DELETE

deleteUser :: Int -> I.AppT DI.Int64
deleteUser id' = SU.runDBDelete TU.userTable (\aic -> TU.userColID aic .== O.constant id')


deleteUsers :: [Int] -> I.AppT DI.Int64
deleteUsers =
  fmap sum . mapM deleteUser
