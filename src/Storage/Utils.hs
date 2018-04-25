{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}


module Storage.Utils
  ( runDB
  , runDBInsert
  , runDBInsertR
  , runDBUpdateR
  , runDBDelete
  ) where


import qualified Control.Monad.Trans as MonadT
import qualified Control.Monad.Trans.Reader as ReaderTrans
import qualified Data.Int as DI

import qualified Data.Pool as Pool
import qualified Data.Profunctor.Product.Default as PPfDefault
import qualified Opaleye as O

import qualified Init as I



runDB :: (PPfDefault.Default O.QueryRunner columns haskells)
      => O.Query columns
      -> I.AppT [haskells]
runDB q = do
  pool <- ReaderTrans.asks I.connPool
  Pool.withResource pool (\conn -> MonadT.liftIO $ O.runQuery conn q)


runDBInsert :: O.Table columnsW columnsR
            -> [columnsW]
            -> I.AppT DI.Int64
runDBInsert table rows = do
  pool <- ReaderTrans.asks I.connPool
  Pool.withResource pool (\conn -> MonadT.liftIO $ O.runInsertMany conn table rows)


runDBInsertR :: PPfDefault.Default O.QueryRunner columnsReturned haskells
             => O.Table columnsW columnsR
             -> [columnsW]
             -> (columnsR -> columnsReturned)
             -> I.AppT [haskells]
runDBInsertR table rows f = do
  pool <- ReaderTrans.asks I.connPool
  Pool.withResource pool (\conn -> MonadT.liftIO $ O.runInsertManyReturning  conn table rows f)


runDBUpdateR :: (PPfDefault.Default O.QueryRunner columnsReturned haskells)
             => O.Table columnsW columnsR
             -> (columnsR -> columnsW)
             -> (columnsR -> O.Column O.PGBool)
             -> (columnsR -> columnsReturned)
             -> I.AppT [haskells]
runDBUpdateR table f p g = do
  pool <- ReaderTrans.asks I.connPool
  Pool.withResource pool (\conn -> MonadT.liftIO $ O.runUpdateReturning conn table f p g)


runDBDelete :: O.Table a columnsR
            -> (columnsR -> O.Column O.PGBool)
            -> I.AppT DI.Int64
runDBDelete table predicate = do
  pool <- ReaderTrans.asks I.connPool
  Pool.withResource pool (\conn -> MonadT.liftIO $ O.runDelete conn table predicate)

