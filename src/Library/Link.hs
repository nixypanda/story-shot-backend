{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Library.Link
  ( fromPG
  , fromPGs
  ) where

import qualified Data.Map as M
import qualified Data.Maybe as DM

import qualified Init as I
import qualified Class.Includes as CI
import qualified Type.Or as Or
import qualified Class.Resource as CR


-- _linkAll :: (CR.UnlinkedResource pg, CR.LinkedResource lr, CR.Resource r, CR.ShortResource sr)
--          => (pg -> [Or.Or sr r] -> [Or.Or [sr] [r]] -> lr)
--          -> [pg]
--          -> [Either (M.Map Int sr) (M.Map Int r)] -> [Either (M.Map Int [sr]) (M.Map Int [r])]
--          -> [lr]
_linkAll linker pgs rMaps rlMaps =
  let
    _convert :: Either (M.Map k a) (M.Map k b) -> M.Map k (Or.Or a b)
    _convert (Left m) = fmap (Or.Or . Left) m
    _convert (Right m) = fmap (Or.Or . Right) m

    getResourceList pid = fmap (DM.fromJust . M.lookup pid . _convert) rMaps
    getResources pid = fmap (DM.fromMaybe (Or.Or $ Left []) . M.lookup pid . _convert) rlMaps

    mkLinkedResource pg = 
      linker pg (getResourceList pid) (getResources pid)
        where pid = CR.urid pg
  in
    fmap mkLinkedResource pgs


-- fromPG :: (CI.Includes cis, Eq cis, CR.LinkedResource lr, CR.UnlinkedResource ur, CR.LinkedResource lr, CR.ShortResource sr)
--         => (ur -> [Or.Or sr r] -> [Or.Or [sr] [r]] -> lr)
--         -> [(cis, Int -> I.AppT (Maybe r), Int -> sr, Int)]
--         -> [(cis, Int -> I.AppT [r], Int -> I.AppT [Int], Int -> sr, Int)]
--         -> [cis]
--         -> ur
--         -> I.AppT (Maybe lr)
fromPG linker singles multiples includes pg = do
  let
    getSingle (include, fetcher, shorter, id') =
      if include `elem` includes
         then fmap (Or.Or . Right) <$> fetcher id'
         else return . Just . Or.Or . Left $ shorter id'

    getAllSingles = (fmap sequence . sequence) $ map getSingle singles

    getMultiple (include, fetcher, fetcher', shorter, pgid) =
      if include `elem` includes
         then (Or.Or . Right) <$> fetcher pgid
         else (Or.Or . Left . map shorter) <$> fetcher' pgid

    getAllMultiples = mapM getMultiple multiples

  msingles <- getAllSingles
  case msingles of
    Nothing -> return Nothing
    Just singles' -> do
      multiples' <- getAllMultiples
      return . Just $ linker pg singles' multiples'


-- fromPGs :: (CI.Includes cis, Eq cis, CR.LinkedResource lr, CR.UnlinkedResource ur, CR.LinkedResource lr, CR.ShortResource sr)
--         => (ur -> [Or.Or sr r] -> [Or.Or [sr] [r]] -> lr)
--         -> [(cis, [Int] -> I.AppT [r], [Int] -> [sr], M.Map Int Int)]
--         -> [(cis, Int -> I.AppT (M.Map Int r), Int -> I.AppT (M.Map Int sr), [Int])]
--         -> [cis]
--         -> [ur]
--         -> I.AppT [lr]
fromPGs linker singles multiples includes pgs = do
  let
    getSingle (include, fetcher, shorter, idMap) =
      if include `elem` includes
         then do
           fullObjs <- fetcher $ M.elems idMap
           let
            idToObjMap = M.fromList [(CR.rid r, r) | r <- fullObjs]
           return . Right $ fmap (idToObjMap M.!) idMap
         else return . Left $ fmap shorter idMap

    getAllSingles = mapM getSingle singles

    getMultiple (include, fetcher, fetcher', pgids) =
      if include `elem` includes
         then Right <$> fetcher pgids
         else Left <$> fetcher' pgids

    getAllMultiples = mapM getMultiple multiples

  singles <- getAllSingles
  multiples <- getAllMultiples
  return $ _linkAll linker pgs singles multiples
