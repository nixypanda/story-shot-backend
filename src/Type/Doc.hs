{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module Type.Doc
  where


import Data.Aeson ((.=))

import qualified GHC.Generics as Generics

import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as DA
import qualified Data.Default as Default
import qualified Data.Text as Text



-- We will say it's a lightweight version of https://github.com/toddmohney/json-api
-- insted of saying this code was ripped from the aforementioned repo. CAPISCE


-- META

{-|
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
data Meta = Meta DA.Object
  deriving (Show, Eq, Generics.Generic)


instance DA.ToJSON Meta
instance DA.FromJSON Meta


instance Monoid Meta where
  mappend (Meta a) (Meta b) = Meta $ HM.union a b
  mempty = Meta HM.empty



{-|
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
class (DA.ToJSON a) => MetaObject a where
  typeName :: a -> Text.Text



{-|
  Convienience constructor function for the Meta type
  Useful on its own or in combination with Meta's monoid instance
  Example usage:
  See MetaSpec.hs for an example
-}
mkMeta :: (MetaObject a) => a -> Meta
mkMeta obj = Meta $ HM.singleton (typeName obj) (DA.toJSON obj)



{-|
  The 'Resource' type encapsulates the underlying 'Resource'
  Included in the top-level 'Doc', the 'Resource' may be either
  a singleton resource or a list.
  For more information see: <http://jsonapi.org/format/#document-top-level>
-}
data Doc a
  = Singleton a
  | List [a] (Maybe Meta)
  deriving (Show, Eq, Generics.Generic)


instance (DA.ToJSON a) => DA.ToJSON (Doc a) where
  toJSON (Singleton res) =
    DA.object
    [ "data"  .= res
    ]
  toJSON (List res meta) =
    DA.object
    [ "data"  .= res
    , "meta"  .= meta
    ]


mkSingleDoc :: a -> Doc a
mkSingleDoc = Singleton


mkListDoc :: [a] -> Maybe Meta -> Doc a
mkListDoc = List



-- ERROR

{-|
  Type for providing application-specific detail to unsuccessful API
  responses.
  Specification: <http://jsonapi.org/format/#error-objects>
-}
data Error a =
  Error { id     :: Maybe Text.Text
        , status :: Maybe Text.Text
        , code   :: Maybe Text.Text
        , title  :: Maybe Text.Text
        , detail :: Maybe Text.Text
        , meta   :: Maybe Meta
        }
  deriving (Show, Eq, Generics.Generic)


instance DA.ToJSON a   => DA.ToJSON (Error a)
instance DA.FromJSON a => DA.FromJSON (Error a)


instance Default.Default (Error a) where
  def = Error
    { Type.Doc.id     = Nothing
    , status = Nothing
    , code   = Nothing
    , title  = Nothing
    , detail = Nothing
    , meta   = Nothing
    }


{-|
  The 'ErrorDoc' type represents the alternative form of the top-level
  JSON-API requirement.
  @error@ attribute - a descriptive object encapsulating application-specific
  error detail.
  For more information see: <http://jsonapi.org/format/#errors>
-}
data ErrorDoc a = ErrorDoc
  { _error :: Error a
  , _errorMeta  :: Maybe Meta
  } deriving (Show, Eq, Generics.Generic)


instance (DA.ToJSON a) => DA.ToJSON (ErrorDoc a) where
  toJSON (ErrorDoc err meta) =
    DA.object
    [ "error" .= err
    , "meta"  .= meta
    ]


-- Aliasing

type MaybeResource a = Either (ErrorDoc a) (Doc a)

