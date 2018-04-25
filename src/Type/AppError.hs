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


import qualified Control.Exception as Exception
import qualified Data.Typeable as Typeable

import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextE
import qualified Database.PostgreSQL.Simple as PGS
import qualified Network.HTTP.Types.Status as HTTPStatus

import qualified Type.Doc as TD


data ClientError
  = ResourceNotFound
  | InvalidQueryParams
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
    TD.Error
      { TD.id = Nothing
      , TD.status = Just . Text.pack . show . HTTPStatus.statusCode $ HTTPStatus.status500
      , TD.code = Just . Text.pack . show $ sqlExecStatus
      , TD.title = Just . TextE.decodeUtf8 $ sqlErrorMsg
      , TD.detail = Just . TextE.decodeUtf8 $ sqlErrorDetail
      , TD.meta = Nothing
      }


instance APIError ClientError where
  toError :: ClientError -> TD.Error a
  toError ResourceNotFound =
    TD.Error
      { TD.id = Nothing
      , TD.status = Just . Text.pack . show . HTTPStatus.statusCode $ HTTPStatus.status404
      , TD.code = Nothing
      , TD.title = Just "Resource Not Found"
      , TD.detail = Just "The requested resource does not exist"
      , TD.meta = Nothing
      }
  toError InvalidQueryParams =
    TD.Error
      { TD.id = Nothing
      , TD.status = Just . Text.pack . show . HTTPStatus.statusCode $ HTTPStatus.status400
      , TD.code = Nothing
      , TD.title = Just "Invalid Query Parameters"
      , TD.detail = Just "Query Parameters not supported for API request"
      , TD.meta = Nothing
      }


instance APIError ServerError where
  toError :: ServerError -> TD.Error a
  toError NoAuthorForStory =
    TD.Error
      { TD.id = Nothing
      , TD.status = Just . Text.pack . show . HTTPStatus.statusCode $ HTTPStatus.status500
      , TD.code = Nothing
      , TD.title = Just "Internal Server Error"
      , TD.detail = Just "No Author is associated with this story"
      , TD.meta = Nothing
      }


-- Error Doc JSON Response
docError :: APIError e => e -> TD.ErrorDoc a
docError e = TD.ErrorDoc (toError e) Nothing
