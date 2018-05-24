{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Library.Auth
  ( extractAuth
  , createDynamicSalt
  , createPassword
  , createSessionKey
  ) where


import Data.Monoid ((<>))

import qualified Control.Monad as M
import Data.Char as Char

import qualified Data.Text as Text
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as Encoding
import qualified Data.Random as Random
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString.Base64 as B64
import qualified Network.Wai as Wai
import qualified Crypto.BCrypt as BCrypt

import qualified Type.AppError as TAe


type AuthHeader =
  (Text.Text, B.ByteString)


extractAuth :: Wai.Request -> Either TAe.ClientError AuthHeader
extractAuth r =
  let
    headers = Wai.requestHeaders r
  in
    case [v | (k, v) <- headers, k == "Authorization"] of
      [auth] ->
        case P.parseOnly authHeaderParser auth of
          Left e -> Left $ TAe.InvalidInput $ Text.pack e
          Right res -> Right res

      _ -> Left TAe.RequiresAuth



isBase64Char :: (Ord a, Num a) => a -> Bool
isBase64Char w
  =  (w >= 47 && w <= 57 )
  || (w >= 64 && w <= 90 )
  || (w >= 97 && w <= 122)
  || (w == 43 || w == 61 )


authHeaderParser :: P.Parser AuthHeader
authHeaderParser = do
  b64     <- P.string "Basic " *> P.takeWhile1 isBase64Char
  decoded <- either fail pure $ B64.decode b64
  case B.split 58 decoded of
    [uname, pwd] ->  pure (Encoding.decodeUtf8 uname, pwd)
    _ -> fail "Could not unpack auth header into username and password components"


base64List :: String
base64List = fmap Char.chr $ [43] ++ [47..57] ++ [61] ++ [64..90] ++ [97..122]


randomBase64Seq :: Int -> Random.RVar String
randomBase64Seq size = M.replicateM size $ Random.randomElement base64List


createSessionKey :: Int -> IO String
createSessionKey size = Random.runRVar (randomBase64Seq size) Random.StdRandom



-- Password

createDynamicSalt :: IO B.ByteString
createDynamicSalt = do
  salt <- BCrypt.genSaltUsingPolicy BCrypt.fastBcryptHashingPolicy
  case salt of
    Nothing -> error "Hashing password failure"
    Just ps -> pure ps


type StaticSalt  = B.ByteString
type DynamicSalt = B.ByteString
type Password    = B.ByteString

createPassword :: StaticSalt -> DynamicSalt -> Password -> IO Password
createPassword sSalt dSalt pass = do
  mPass <- BCrypt.hashPasswordUsingPolicy BCrypt.fastBcryptHashingPolicy (pass <> sSalt <> dSalt)
  case mPass of
    Nothing -> error "Hashing password failure"
    Just ps -> pure ps
