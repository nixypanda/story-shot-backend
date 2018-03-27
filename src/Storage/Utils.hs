{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}


module Storage.Utils where

import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Reader (asks)
import Data.Int (Int64)
import Data.Pool (withResource)

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


runDB :: (Default QueryRunner columns haskells)
      => Query columns
      -> WithConfig [haskells]
runDB q = do
  pool <- asks connPool
  withResource pool (\conn -> liftIO $ runQuery conn q)


runDBInsert :: Table columnsW columnsR
            -> [columnsW]
            -> WithConfig Int64
runDBInsert table rows = do
  pool <- asks connPool
  withResource pool (\conn -> liftIO $ runInsertMany conn table rows)


runDBInsertR :: Default QueryRunner columnsReturned haskells
             => Table columnsW columnsR
             -> [columnsW]
             -> (columnsR -> columnsReturned)
             -> WithConfig [haskells]
runDBInsertR table rows f = do
  pool <- asks connPool
  withResource pool (\conn -> liftIO $ runInsertManyReturning  conn table rows f)


runDBUpdateR :: (Default QueryRunner columnsReturned haskells)
             => Table columnsW columnsR
             -> (columnsR -> columnsW)
             -> (columnsR -> Column PGBool)
             -> (columnsR -> columnsReturned)
             -> WithConfig [haskells]
runDBUpdateR table f p g = do
  pool <- asks connPool
  withResource pool (\conn -> liftIO $ runUpdateReturning conn table f p g)


runDBDelete :: Table a columnsR
            -> (columnsR -> Column PGBool)
            -> WithConfig Int64
runDBDelete table predicate = do
  pool <- asks connPool
  withResource pool (\conn -> liftIO $ runDelete conn table predicate)

