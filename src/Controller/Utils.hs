{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Controller.Utils
  ( cursorPagination
  ) where

import qualified Data.Default as Def
import qualified Data.Either.Utils as EitherUtils
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Read as LazyTextRead

import qualified Web.Scotty.Trans as Scotty

import qualified Type.Pagination as TP

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
