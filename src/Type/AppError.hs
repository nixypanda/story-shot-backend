{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Type.AppError
  ( APIError(..)
  , ClientError(..)
  , ServerError(..)
  , docError
  ) where

import Control.Exception (Exception)
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Typeable (Typeable)

import Database.PostgreSQL.Simple
  ( SqlError(..)
  )
import Network.HTTP.Types.Status
  ( status400
  , status404
  , status500
  , statusCode
  )
import Type.Doc


data ClientError
  = ResourceNotFound
  | InvalidQueryParams
  deriving (Show, Typeable)

instance Exception ClientError

data ServerError
  = NoAuthorForStory
  deriving (Show, Typeable)

instance Exception ServerError

-- Exception Typeclass to represent a failing API request
class (Exception e) => APIError e where
  toError :: e -> Error a


instance APIError SqlError where
  toError :: SqlError -> Error a
  toError SqlError{..} =
    Error
      { Type.Doc.id = Nothing
      , status = Just . pack . show . statusCode $ status500
      , code = Just . pack . show $ sqlExecStatus
      , title = Just . decodeUtf8 $ sqlErrorMsg
      , detail = Just . decodeUtf8 $ sqlErrorDetail
      , meta = Nothing
      }


instance APIError ClientError where
  toError :: ClientError -> Error a
  toError ResourceNotFound =
    Error
      { Type.Doc.id = Nothing
      , status = Just . pack . show . statusCode $ status404
      , code = Nothing
      , title = Just "Resource Not Found"
      , detail = Just "The requested resource does not exist"
      , meta = Nothing
      }
  toError InvalidQueryParams =
    Error
      { Type.Doc.id = Nothing
      , status = Just . pack . show . statusCode $ status400
      , code = Nothing
      , title = Just "Invalid Query Parameters"
      , detail = Just "Query Parameters not supported for API request"
      , meta = Nothing
      }


instance APIError ServerError where
  toError :: ServerError -> Error a
  toError NoAuthorForStory =
    Error
      { Type.Doc.id = Nothing
      , status = Just . pack . show . statusCode $ status500
      , code = Nothing
      , title = Just "Internal Server Error"
      , detail = Just "No Author is associated with this story"
      , meta = Nothing
      }


-- Error Document JSON Response
docError :: APIError e => e -> ErrorDocument a
docError e = ErrorDocument (toError e) Nothing
