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

import Data.Monoid ((<>))

import qualified Control.Exception as Exception
import qualified Data.Typeable as Typeable

import qualified Data.Default as Default
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextE
import qualified Database.PostgreSQL.Simple as PGS
import qualified Network.HTTP.Types.Status as HTTPStatus

import qualified Type.Doc as TD


data ClientError
  = ResourceNotFound
  | InvalidQueryParams [Text.Text]
  | InvalidInput Text.Text
  | RequiresAuth
  deriving (Show, Typeable.Typeable)


instance Exception.Exception ClientError


data ServerError
  = NoAuthorForStory
  deriving (Show, Typeable.Typeable)

instance Exception.Exception ServerError


-- Exception Typeclass to represent a failing API request
class (Exception.Exception e) => APIError e where
  toError :: e -> TD.Error a


instance APIError PGS.SqlError where
  toError :: PGS.SqlError -> TD.Error a
  toError PGS.SqlError{..} =
    Default.def
      { TD.status = toText HTTPStatus.status500
      , TD.code = Just . Text.pack . show $ sqlExecStatus
      , TD.title = Just . TextE.decodeUtf8 $ sqlErrorMsg
      , TD.detail = Just . TextE.decodeUtf8 $ sqlErrorDetail
      }


instance APIError ClientError where
  toError :: ClientError -> TD.Error a
  toError ResourceNotFound =
    Default.def
      { TD.status = toText HTTPStatus.status404
      , TD.title = Just "Resource Not Found"
      }
  toError (InvalidQueryParams vals) =
    Default.def
      { TD.status = toText HTTPStatus.status400
      , TD.title = Just "Invalid Query Parameters"
      , TD.detail = Just $ "Query Parameters not supported for API request: " <> Text.unwords vals
      }
  toError (InvalidInput e) =
    Default.def
      { TD.status = toText HTTPStatus.status400
      , TD.title = Just "Invalid Input"
      , TD.detail = Just e
      }
  toError RequiresAuth =
    Default.def
      { TD.status = toText HTTPStatus.status401
      , TD.title = Just "Requires Authentication"
      }

instance APIError ServerError where
  toError :: ServerError -> TD.Error a
  toError NoAuthorForStory =
    Default.def
      { TD.status = toText HTTPStatus.status500
      , TD.title = Just "Internal Server Error"
      , TD.detail = Just "No Author is associated with this story"
      }


-- Error Doc JSON Response
docError :: APIError e => e -> TD.ErrorDoc a
docError e = TD.ErrorDoc (toError e) Nothing


-- Helpers

toText :: HTTPStatus.Status -> Maybe Text.Text
toText = Just . Text.pack . show . HTTPStatus.statusCode


