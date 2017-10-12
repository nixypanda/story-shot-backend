{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}


module Storage.Utils where

import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Reader (asks)
import Data.Int (Int64)

import Data.Profunctor.Product.Default (Default)
import Opaleye
  ( Query
  , QueryRunner
  , Table
  , Column
  , PGBool
  , runQuery
  , runInsertMany
  , runInsertManyReturning
  , runUpdateReturning
  , runDelete
  )

import Init (Config(..), WithConfig)


runDB :: (Default QueryRunner columns haskells) => Query columns -> WithConfig [haskells]
runDB q = do
  conn <- asks connection
  liftIO $ runQuery conn q


runDBInsert :: Table columnsW columnsR
            -> [columnsW]
            -> WithConfig Int64
runDBInsert table rows = do
  conn <- asks connection
  liftIO $ runInsertMany conn table rows

runDBInsertR :: Default QueryRunner columnsReturned haskells
             => Table columnsW columnsR
             -> [columnsW]
             -> (columnsR -> columnsReturned)
             -> WithConfig [haskells]
runDBInsertR table rows f = do
  conn <- asks connection
  liftIO $ runInsertManyReturning  conn table rows f


runDBUpdateR :: (Default QueryRunner columnsReturned haskells)
             => Table columnsW columnsR
             -> (columnsR -> columnsW)
             -> (columnsR -> Column PGBool)
             -> (columnsR -> columnsReturned)
             -> WithConfig [haskells]
runDBUpdateR table f p g = do
  conn <- asks connection
  liftIO $ runUpdateReturning conn table f p g


runDBDelete :: Table a columnsR -> (columnsR -> Column PGBool) -> WithConfig Int64
runDBDelete table predicate = do
  conn <- asks connection
  liftIO $ runDelete conn table predicate

