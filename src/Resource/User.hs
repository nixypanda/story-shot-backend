{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}


module Resource.User
  ( getUsers
  , getUser
  , createUsers
  , createUser
  , updateUsers
  , updateUser
  , deleteUsers
  , deleteUser
  ) where

import qualified Data.Map as M

import qualified Data.Text as Text

import qualified Init as I
import qualified Class.Includes as CI
import qualified Class.Resource as CR
import qualified Type.Pagination as TP
import qualified Type.Or as Or
import qualified Type.User as TU
import qualified Type.Author as TA
import qualified Type.AppError as TAe
import qualified Storage.User as SU
import qualified Storage.Author as SA
import qualified Library.Link as LL



-- CREATE

createUser :: [UserIncludes] -> TU.UserInsert -> I.AppT TU.User
createUser includes user = do
  author <- SA.createAuthor $ TA.mkAuthorInsert $ TU.userName user
  user' <- SU.createUser user author
  return . head $ _linkAll includes [user'] [author]


createUsers :: [UserIncludes] -> [TU.UserInsert] -> I.AppT [TU.User]
createUsers includes users = do
  authors <- SA.createAuthors $ fmap (TA.mkAuthorInsert . TU.userName) users
  users' <- SU.createUsers users authors
  return $ _linkAll includes users' authors



-- RETRIVE

getUsers :: TP.CursorParam -> [UserIncludes]-> I.AppT [TU.User]
getUsers cur includes =
  SU.getUsers cur >>= _fromPGUsers includes


getUser :: Int -> [UserIncludes] -> I.AppT (Maybe TU.User)
getUser sid includes = do
  mstory <- SU.getUser sid
  case mstory of
    Nothing      -> return Nothing
    Just pguser -> do
      mauthor <- LL.getResourceForResource includes (IAuthor, SA.getAuthor, TA.mkAuthorS, TU.userAuthorID pguser)
      case mauthor of
        Nothing -> return Nothing
        Just author -> return . Just $ TU.mkLinkedUser pguser author



_linkAll :: [UserIncludes] -> [TU.PGUser] -> [TA.Author] -> [TU.User]
_linkAll [] users _ = zipWith TU.mkUserFromDB users $ fmap (Or.Or . Left . TA.mkAuthorS . TU.userAuthorID) users
_linkAll [IAuthor] users authors =
  let
    aMap = M.fromList [(TA.authorID author, author) | author <- authors]
    authors' = fmap (Or.Or . Right . (aMap M.!) . TU.userAuthorID) users
  in
    zipWith TU.mkUserFromDB users authors'
_linkAll _ _ _ = error "Undefined is not a function"


_fromPGUsers :: [UserIncludes] -> [TU.PGUser] -> I.AppT [TU.User]
_fromPGUsers includes users = do
  let
    userIDs = map TU.pgUserID users
    authorIDs = map TU.userAuthorID users
    userAuthorIDMap = M.fromList $ zip userIDs authorIDs
  authors <- LL.getResourceForResources includes (IAuthor, SA.getMultiAuthors, TA.mkAuthorS, userAuthorIDMap)
  let
    mkLinkedResource pg = TU.mkLinkedUser pg (LL.getResource pid authors)
      where pid = CR.urid pg
  return $ fmap mkLinkedResource users


-- UPDATE

updateUser :: TU.UserPut -> I.AppT (Maybe TU.User)
updateUser = undefined -- fmap TM.docOrError . SU.updateUser


updateUsers :: [TU.UserPut] -> I.AppT [TU.User]
updateUsers = undefined -- fmap TM.docMulti . SU.updateUsers



-- DELETE

deleteUser :: Int -> I.AppT Int
deleteUser = fmap fromIntegral . SU.deleteUser


deleteUsers :: [Int] -> I.AppT Int
deleteUsers = fmap fromIntegral . SU.deleteUsers



-- Query Params Processing

data UserIncludes
  = IAuthor
  deriving (Show, Eq)


instance CI.Includes UserIncludes where
  getAll = [IAuthor]
  singles = [IAuthor]
  multiples = []

  fromString :: Text.Text -> Either TAe.ClientError UserIncludes
  fromString "author" = Right IAuthor
  fromString _ = Left TAe.InvalidQueryParams
