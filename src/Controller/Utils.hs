{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Controller.Utils
  ( cursorPagination
  , extractData
  , extractIncludes
  , deleteBatchExample
  , executeAction
  ) where


import Data.Aeson ((.=))

import qualified Data.Default as Def
import qualified Data.Either.Utils as EitherUtils

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Read as LazyTextRead
import qualified Web.Scotty.Trans as Scotty

import qualified Init as I
import qualified Type.Pagination as TP
import qualified Type.Doc as TD
import qualified Controller.Basic as CB
import qualified Type.Include as Include
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


extractIncludes :: Scotty.ActionT I.Error I.AppT (Either TAe.ClientError [Include.Include])
extractIncludes =
  (Include.fromCSV <$> Scotty.param "include") `Scotty.rescue` (\_ -> return $ Right [])


deleteBatchExample :: Aeson.Value
deleteBatchExample = Aeson.object
  [ "info" .= ("Please provide an array of IDs" :: Text.Text)
  ]


executeAction :: (TAe.APIError e, Monad m)
               => Either e t -> (t -> m (Either (TD.ErrorDoc a) b)) -> m (Either (TD.ErrorDoc a) b)
executeAction (Left e) _ = return . Left $ TAe.docError e
executeAction (Right includes) f = f includes


