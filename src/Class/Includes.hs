{-# LANGUAGE OverloadedStrings #-}

module Class.Includes
  where


import qualified Data.Text as Text
import qualified Data.Either as DEither

import qualified Type.AppError as TAe


class Includes a where
  getAll :: [a]
  singles :: [a]
  multiples :: [a]
  fromString :: Text.Text -> Either TAe.ClientError a

  fromCSV :: Text.Text -> Either TAe.ClientError [a]
  fromCSV =
    let
      f :: ([TAe.ClientError], [a]) -> Either TAe.ClientError [a]
      f (x:_, _) = Left x
      f (_, ys) = Right ys
   in
      f . DEither.partitionEithers . map fromString . Text.splitOn ","
