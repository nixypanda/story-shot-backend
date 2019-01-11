{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Storage.Session
  ( createSessionForUser
  ) where


import qualified Control.Monad.Trans as MonadT

import qualified Init                as I
import qualified Library.Auth        as Auth
import qualified Storage.Utils       as SU
import qualified Type.Session        as TS


createSessionForUser :: Int -> I.AppT TS.PGSession
createSessionForUser userID = head <$> createSessions [userID]


createSessions :: [Int] -> I.AppT [TS.PGSession]
createSessions userIDs = do
  sessionIDs <- MonadT.lift $ mapM (const $ Auth.createSessionKey 55) userIDs
  SU.runDBInsertR TS.sessionTable (zipWith TS.mkSessionForUser userIDs sessionIDs) id

