{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}


module Storage.Session
  ( createSessionForUser
  ) where


import qualified Control.Monad.Trans as MonadT

import qualified Init as I
import qualified Type.Session as TS
import qualified Storage.Utils as SU
import qualified Library.Auth as Auth


createSessionForUser :: Int -> I.AppT TS.PGSession
createSessionForUser userID = head <$> createSessions [userID]


createSessions :: [Int] -> I.AppT [TS.PGSession]
createSessions userIDs = do
  sessionIDs <- MonadT.lift $ mapM (const $ Auth.createSessionKey 55) userIDs
  SU.runDBInsertR TS.sessionTable (zipWith TS.mkSessionForUser userIDs sessionIDs) id

