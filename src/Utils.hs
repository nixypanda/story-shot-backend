{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Data.Maybe (fromMaybe, fromJust)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Network.URL (URL, importURL)


-- Others

toURL :: String -> URL
toURL = fromJust . importURL


fromMaybeEnv :: (Read a) => a -> String -> IO a
fromMaybeEnv val =
  fmap (fromMaybe val . (>>= readMaybe)) . lookupEnv
