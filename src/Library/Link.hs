{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}


module Library.Link
  ( getResource
  , getResources
  , getResourceForResource
  , getResourceForResources
  , getResourceListForResource
  , getResourceListForResources
  ) where

import qualified Data.Map       as M
import qualified Data.Maybe     as DM

import qualified Class.Resource as CR
import qualified Type.Or        as Or



_convert :: Either (M.Map k a) (M.Map k b) -> M.Map k (Or.Or a b)
_convert (Left m)  = fmap (Or.Or . Left) m
_convert (Right m) = fmap (Or.Or . Right) m


getResource :: Ord k => k -> Either (M.Map k a) (M.Map k b) -> Or.Or a b
getResource pid = DM.fromJust . M.lookup pid . _convert


getResources :: Ord k => k -> Either (M.Map k [a]) (M.Map k [b]) -> Or.Or [a] [b]
getResources pid = DM.fromMaybe (Or.Or $ Left []) . M.lookup pid . _convert


getResourceForResource :: (Foldable t, Monad m, Eq a)
                       => t a -> (a, t1 -> m (Maybe b), t1 -> a1, t1) -> m (Maybe (Or.Or a1 b))
getResourceForResource includes (include, fetcher, shorter, id') =
  if include `elem` includes
     then fmap (Or.Or . Right) <$> fetcher id'
     else return . Just . Or.Or . Left $ shorter id'


getResourceForResources :: (CR.Resource b, Foldable t, Monad m, Eq a)
                        => t a -> (a, [Int] -> m [b], Int -> b1, M.Map k Int) -> m (Either (M.Map k b1) (M.Map k b))
getResourceForResources includes (include, fetcher, shorter, idMap) =
  if include `elem` includes
     then do
       fullObjs <- fetcher $ M.elems idMap
       let
        idToObjMap = M.fromList [(CR.rid r, r) | r <- fullObjs]
       return . Right $ fmap (idToObjMap M.!) idMap
     else return . Left $ fmap shorter idMap


getResourceListForResource :: (Foldable t, Functor f, Eq a)
                           => t a -> (a, t1 -> f b, t1 -> f [a1], a1 -> b1, t1) -> f (Or.Or [b1] b)
getResourceListForResource includes (include, fetcher, fetcher', shorter, pgid) =
  if include `elem` includes
     then (Or.Or . Right) <$> fetcher pgid
     else (Or.Or . Left . map shorter) <$> fetcher' pgid


getResourceListForResources :: (Foldable t, Functor f, Eq a)
                            => t a -> (a, t1 -> f b, t1 -> f a1, t1) -> f (Either a1 b)
getResourceListForResources includes (include, fetcher, fetcher', pgids) =
  if include `elem` includes
     then Right <$> fetcher pgids
     else Left <$> fetcher' pgids
