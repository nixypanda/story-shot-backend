{-# LANGUAGE OverloadedStrings #-}

module Type.Include
  ( Include(..)
  , fromCSV
  ) where


import qualified Data.Either   as DEither
import qualified Data.Text     as Text

import qualified Type.AppError as TAe


data Include
  = User
  | Author
  | Tag
  deriving (Show, Eq)


fromString :: Text.Text -> Either TAe.ClientError Include
fromString "user"   = Right User
fromString "author" = Right Author
fromString "tag"    = Right Tag
fromString val      = Left $ TAe.InvalidQueryParams [val]


fromCSV :: Text.Text -> Either TAe.ClientError [Include]
fromCSV =
  let
    f :: ([TAe.ClientError], [Include]) -> Either TAe.ClientError [Include]
    f (x:_, _) = Left x
    f (_, ys)  = Right ys
  in
    f . DEither.partitionEithers . map fromString . Text.splitOn ","
