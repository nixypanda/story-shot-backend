{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module Resource.Session
  ( createSessionFromDetails
  ) where


import           Data.Monoid                ((<>))

import qualified Control.Monad.Trans.Reader as ReaderT
import qualified Crypto.BCrypt              as BCrypt
import qualified Data.ByteString            as B
import qualified Data.Maybe                 as DM

import qualified Data.Text                  as Text

import qualified Init                       as I
import qualified Resource.User              as RU
import qualified Storage.Session            as SS
import qualified Storage.User               as SU
import qualified Type.AppError              as TAe
import qualified Type.Include               as Include
import qualified Type.Or                    as Or
import qualified Type.Session               as TS
import qualified Type.User                  as TU


createSessionFromDetails :: [Include.Include] -> Text.Text -> B.ByteString -> I.AppT (Either TAe.ClientError TS.Session)
createSessionFromDetails includes username password = do
  mpguser <- SU.getUserFromUsername username
  case mpguser of
    Nothing -> return $ Left $ TAe.InvalidInput "Invalid Username"
    Just pguser -> do
      sSalt <- ReaderT.asks I.staticSalt
      let dSalt = TU.getSalt pguser
          originalHashedPassword = TU.pgUserPass pguser
      if BCrypt.validatePassword originalHashedPassword (password <> sSalt <> dSalt)
         then do
           let uid = TU.pgUserID pguser
           session <- SS.createSessionForUser uid
           user' <- if Include.User `elem` includes
                       then do
                         muser <- RU.getUser uid includes
                         let user = DM.fromJust muser
                         return $ Or.Or $ Right user
                       else return . Or.Or . Left $ TU.mkUserS uid
           return . Right $ TS.mkLinkedSession session user'
         else return $ Left $ TAe.InvalidInput "Invalid Password"
