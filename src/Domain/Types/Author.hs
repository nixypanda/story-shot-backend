{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Domain.Types.Author
    ( Author(..)
    , AuthorBase(..)
    , AuthorInsert(..)
    , AuthorPut(..)
    ) where


import qualified Data.Text    as Text
import qualified Data.Time    as DT
import qualified GHC.Generics as Generics


data Author =
  Author
    { id        :: Int
    , name      :: Text.Text
    , createdAt :: DT.UTCTime
    , updatedAt :: DT.UTCTime
    } deriving (Eq, Show, Generics.Generic)


newtype AuthorInsert = AuthorInsert
    { name :: Text.Text
    } deriving (Eq, Show, Generics.Generic)


newtype AuthorBase = AuthorBase
    { id :: Int
    } deriving (Eq, Show, Generics.Generic)


data AuthorPut = AuthorPut
    { id   :: Int
    , name :: Text.Text
    } deriving (Eq, Show, Generics.Generic)


mkAuthorPut :: Int -> Text.Text -> AuthorPut
mkAuthorPut = AuthorPut


mkAuthorInsert :: Text.Text -> AuthorInsert
mkAuthorInsert = AuthorInsert
