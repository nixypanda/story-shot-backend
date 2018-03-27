{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Controller.Utils
  ( cursorPagination
  ) where

import Data.Default (def)
import Data.Either.Utils (maybeToEither)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Read (decimal)

import Web.Scotty.Trans (Param)

import Type.Pagination

cursorPagination :: [Param] -> CursorParam
cursorPagination qparams =
  let
    getShit :: Text -> Int -> Int
    getShit key default' = either (const default') fst $
      maybeToEither "Nothing" (lookup key qparams) >>= decimal
   in
  def
    { nextCursor = getShit "next_cursor" 0
    , sizeCursor = getShit "size" 10
    }
