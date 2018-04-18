{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Doc
  where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, Object, toJSON, (.=))
import Data.HashMap.Strict as HM
import Data.Default
import Data.Text (Text)

import qualified Data.Aeson as AE


-- We will say it's a lightweight version of https://github.com/toddmohney/json-api
-- insted of saying this code was ripped from the aforementioned repo. CAPISCE


-- meta
{- |
Type representing a JSON-API meta object.
Meta is an abstraction around an underlying Map consisting of
resource-specific metadata.
Example JSON:
@
  "meta": {
    "copyright": "Copyright 2015 Example Corp.",
    "authors": [
      "Andre Dawson",
      "Kirby Puckett",
      "Don Mattingly",
      "Ozzie Guillen"
    ]
  }
@
Specification: <http://jsonapi.org/format/#document-meta>
-}
data Meta = Meta Object
  deriving (Show, Eq, Generic)

instance ToJSON Meta
instance FromJSON Meta

instance Monoid Meta where
  mappend (Meta a) (Meta b) = Meta $ HM.union a b
  mempty = Meta HM.empty

{- |
Convienience class for constructing a Meta type
Example usage:
@
  data Pagination = Pagination
    { currentPage :: Int
    , totalPages :: Int
    } deriving (Show, Generic)
  instance ToJSON Pagination
  instance MetaObject Pagination where
    typeName _ = "pagination"
@
-}
class (ToJSON a) => MetaObject a where
  typeName :: a -> Text

{- |
Convienience constructor function for the Meta type
Useful on its own or in combination with Meta's monoid instance
Example usage:
See MetaSpec.hs for an example
-}
mkMeta :: (MetaObject a) => a -> Meta
mkMeta obj = Meta $ HM.singleton (typeName obj) (toJSON obj)


{- |
The 'Resource' type encapsulates the underlying 'Resource'
Included in the top-level 'Document', the 'Resource' may be either
a singleton resource or a list.
For more information see: <http://jsonapi.org/format/#document-top-level>
-}
data Document a
  = Singleton a
  | List [a] (Maybe Meta)
  deriving (Show, Eq, Generic)


instance (ToJSON a) => ToJSON (Document a) where
  toJSON (Singleton res) =
    AE.object
    [ "data"  .= res
    ]
  toJSON (List res meta) =
    AE.object
    [ "data"  .= res
    , "meta"  .= meta
    ]

mkSingleDoc :: a -> Document a
mkSingleDoc = Singleton

mkListDoc :: [a] -> Maybe Meta -> Document a
mkListDoc = List


-- ERROR
{- |
Type for providing application-specific detail to unsuccessful API
responses.
Specification: <http://jsonapi.org/format/#error-objects>
-}
data Error a =
  Error { id     :: Maybe Text
        , status :: Maybe Text
        , code   :: Maybe Text
        , title  :: Maybe Text
        , detail :: Maybe Text
        , meta   :: Maybe Meta
        }
  deriving (Show, Eq, Generic)


instance ToJSON a   => ToJSON (Error a)
instance FromJSON a => FromJSON (Error a)


instance Default (Error a) where
  def = Error
    { Type.Doc.id     = Nothing
    , status = Nothing
    , code   = Nothing
    , title  = Nothing
    , detail = Nothing
    , meta   = Nothing
    }


{- |
The 'ErrorDocument' type represents the alternative form of the top-level
JSON-API requirement.
@error@ attribute - a descriptive object encapsulating application-specific
error detail.
For more information see: <http://jsonapi.org/format/#errors>
-}
data ErrorDocument a = ErrorDocument
  { _error :: Error a
  , _errorMeta  :: Maybe Meta
  } deriving (Show, Eq, Generic)

instance (ToJSON a) => ToJSON (ErrorDocument a) where
  toJSON (ErrorDocument err meta) =
    AE.object
    [ "error" .= err
    , "meta"  .= meta
    ]
