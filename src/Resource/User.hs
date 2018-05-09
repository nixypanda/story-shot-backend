{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}


module Resource.User
  ( getUserResources
  , getUserResource
  , createUserResources
  , createUserResource
  , updateUserResources
  , updateUserResource
  , deleteUserResources
  , deleteUserResource
  ) where

import qualified Data.Map as M

import qualified Data.Text as Text

import qualified Init as I
import qualified Class.Includes as CI
import qualified Type.Pagination as TP
import qualified Type.Meta as TM
import qualified Type.Or as Or
import qualified Type.Doc as TD
import qualified Type.User as TU
import qualified Type.Author as TA
import qualified Type.AppError as TAe
import qualified Storage.User as SU
import qualified Storage.Author as SA
import qualified Library.Link as LL



-- CREATE

createUserResource :: Either TAe.ClientError [UserIncludes] -> TU.UserInsert -> I.AppT (TD.MaybeResource TU.User)
createUserResource (Left e) _ = return . Left $ TAe.docError e
createUserResource (Right includes) user = do
  author <- SA.createAuthor $ TA.mkAuthorInsert $ TU.userName user
  user' <- SU.createUser user author
  return . Right . TM.indexDoc' . head $ _linkAll includes [user'] [author]


createUserResources :: Either TAe.ClientError [UserIncludes] -> [TU.UserInsert] -> I.AppT (TD.MaybeResource TU.User)
createUserResources (Left e) _ = return . Left $ TAe.docError e
createUserResources (Right includes) users = do
  authors <- SA.createAuthors $ fmap (TA.mkAuthorInsert . TU.userName) users
  users' <- SU.createUsers users authors
  return . Right . TM.docMulti $ _linkAll includes users' authors



-- RETRIVE

getUserResources :: TP.CursorParam -> Either TAe.ClientError [UserIncludes]-> I.AppT (TD.MaybeResource TU.User)
getUserResources _ (Left e) = return . Left $ TAe.docError e
getUserResources cur (Right includes) = do
  users <- SU.getUsers cur
  (Right . TM.docMulti) <$> _fromPGUsers includes users


getUserResource :: Int -> Either TAe.ClientError [UserIncludes] -> I.AppT (TD.MaybeResource TU.User)
getUserResource _ (Left e) = return $ Left $ TAe.docError e
getUserResource sid (Right includes) = do
  mstory <- SU.getUser sid
  case mstory of
    Nothing      -> return $ Left $ TAe.docError TAe.ResourceNotFound
    Just pguser -> do
      muser <- LL.fromPG
        TU.mkLinkedUserResource
        [(IAuthor, SA.getAuthor, TA.mkAuthorS, TU.userAuthorID pguser)]
        []
        includes
        pguser
      case muser of
        Nothing -> return $ Left $ TAe.docError TAe.ResourceNotFound
        Just user -> return $ Right $ TM.indexDoc' user



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
_fromPGUsers includes users =
  let
    userIDs = map TU.pgUserID users
    authorIDs = map TU.userAuthorID users
    userAuthorIDMap = M.fromList $ zip userIDs authorIDs
  in
    LL.fromPGs
      TU.mkLinkedUserResource
      [(IAuthor, SA.getMultiAuthors, TA.mkAuthorS, userAuthorIDMap)]
      []
      includes
      users


-- UPDATE

updateUserResource :: TU.UserPut -> I.AppT (TD.MaybeResource TU.User)
updateUserResource = undefined -- fmap TM.docOrError . SU.updateUser


updateUserResources :: [TU.UserPut] -> I.AppT (TD.MaybeResource TU.User)
updateUserResources = undefined -- fmap TM.docMulti . SU.updateUsers



-- DELETE

deleteUserResource :: Int -> I.AppT (TD.MaybeResource TU.User)
deleteUserResource = fmap TM.docMetaOrError . SU.deleteUser


deleteUserResources :: [Int] -> I.AppT (TD.Doc TU.User)
deleteUserResources = fmap (TM.docMeta . fromIntegral) . SU.deleteUsers



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
