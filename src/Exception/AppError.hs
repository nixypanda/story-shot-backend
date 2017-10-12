{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Exception.AppError
  ( APIError(..)
  , ClientError(..)
  ) where

import Control.Exception (Exception)
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Typeable (Typeable)

import Database.PostgreSQL.Simple
  ( SqlError(..)
  )
import Network.JSONApi.Error (Error(..))
import Network.HTTP.Types.Status (status404, status500, statusCode)


data ClientError
  = ResourceNotFound
  deriving (Show, Typeable)


instance Exception ClientError

-- Exception Typeclass to represent a failing API request
class (Exception e) => APIError e where
  toErrorDoc :: e -> Error a


instance APIError SqlError where
  toErrorDoc :: SqlError -> Error a
  toErrorDoc SqlError{..} =
    Error
      { Network.JSONApi.Error.id = Nothing
      , links = Nothing
      , status = Just . pack . show . statusCode $ status500
      , code = Just . pack . show $ sqlExecStatus
      , title = Just . decodeUtf8 $ sqlErrorMsg
      , detail = Just . decodeUtf8 $ sqlErrorDetail
      , meta = Nothing
      }


instance APIError ClientError where
  toErrorDoc :: ClientError -> Error a
  toErrorDoc ResourceNotFound =
    Error
      { Network.JSONApi.Error.id = Nothing
      , links = Nothing
      , status = Just . pack . show . statusCode $ status404
      , code = Nothing
      , title = Just "Resource Not Found"
      , detail = Just "The requested resource does not exist"
      , meta = Nothing
      }

