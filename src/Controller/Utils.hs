{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Controller.Utils
  ( cursorPagination
  , extractData
  , extractIncludes
  ) where


import qualified Data.Default as Def
import qualified Data.Either.Utils as EitherUtils

import qualified Data.Aeson as Aeson
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Read as LazyTextRead
import qualified Web.Scotty.Trans as Scotty

import qualified Init as I
import qualified Type.Pagination as TP
import qualified Controller.Basic as CB
import qualified Class.Includes as CI
import qualified Type.AppError as TAe


cursorPagination :: [Scotty.Param] -> TP.CursorParam
cursorPagination qparams =
  let
    getShit :: LazyText.Text -> Int -> Int
    getShit key default' = either (const default') fst $
      EitherUtils.maybeToEither "Nothing" (lookup key qparams) >>= LazyTextRead.decimal
   in
  Def.def
    { TP.nextCursor = getShit "next_cursor" 0
    , TP.sizeCursor = getShit "size" 10
    }


extractData :: Aeson.FromJSON a => Aeson.Value -> Scotty.ActionT I.Error I.AppT a
extractData validObjExample =
  Scotty.jsonData `Scotty.rescue` CB.invalidPayload validObjExample


extractIncludes :: CI.Includes a => Scotty.ActionT I.Error I.AppT (Either TAe.ClientError [a])
extractIncludes =
  (CI.fromCSV <$> Scotty.param "includes") `Scotty.rescue` (\_ -> return $ Right [])
