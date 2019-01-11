{-# LANGUAGE OverloadedStrings #-}

module Utils where

import qualified Data.Maybe         as DM (fromJust, fromMaybe)
import qualified System.Environment as SysEnv
import qualified Text.Read          as TextRead

import qualified Network.URL        as N


-- Others

toURL :: String -> N.URL
toURL = DM.fromJust . N.importURL


fromMaybeEnv :: (Read a) => a -> String -> IO a
fromMaybeEnv val =
  fmap (DM.fromMaybe val . (>>= TextRead.readMaybe)) . SysEnv.lookupEnv
