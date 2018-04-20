module Class.Includes
  where

import qualified Data.Text as DT

import qualified Type.AppError as TAe


class Includes a where
  getAll :: [a]
  fromCSV :: DT.Text -> Either TAe.ClientError [a]
