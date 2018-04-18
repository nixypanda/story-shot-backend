module Class.Includes
  where

import Data.Text (Text)

import Type.AppError
  ( ClientError(..)
  )


class Includes a where
  getAll :: [a]
  fromCSV :: Text -> Either ClientError [a]
