module Indexes where
import Errors

import Text.Printf
import Control.Monad.Except

import Data.Map(Map)
import qualified Data.Map as Map

import Data.IntMap(IntMap)
import qualified Data.IntMap as IntMap

-- Lookup in maps or nested maps returning errors

mapLookup :: (Ord a, Show a) => String -> a -> Map a b -> ThrowsError b
mapLookup s k = (maybeToError (MapLookupError s (show k))) . (Map.lookup k)

nestedMapLookup :: (Ord a, Show a) => String -> a -> [(Map a b)] -> ThrowsError b
nestedMapLookup s k [] = throwError $ MapLookupError s (show k)
nestedMapLookup s k (m:ms) = (mapLookup s k m)
                             `catchError` (\_ -> nestedMapLookup s k ms)

-- Index type (int-indexed map with append)

data Index a = Index Int (IntMap a)

instance Show a => Show (Index a) where
  show (Index _ map) = unlines $ IntMap.foldWithKey (\k v acc -> (printf "%d: %s" k (show v)) : acc) [] map

instance Functor Index where
  fmap f (Index a map) = (Index a (fmap f map))

newIndex = Index 0 IntMap.empty
indexNextKey (Index n _) = n

indexAppend :: a -> Index a -> Index a
indexAppend val (Index n m) = Index (n+1) (IntMap.insert n val m)

indexLookup :: String -> Int -> Index a -> ThrowsError a
indexLookup s i (Index _ m) = maybeToError (MapLookupError s (show i)) $ IntMap.lookup i m

indexInsert :: Int -> a -> Index a -> Index a
indexInsert i v (Index t m) = Index (max t (i+1)) (IntMap.insert i v m)

indexElems :: Index a -> [a]
indexElems (Index _ i) = IntMap.elems i

indexFold :: (a -> b -> b) -> b -> Index a -> b
indexFold f i (Index _ idx) = IntMap.fold f i idx

indexMap :: (a -> b) -> Index a -> Index b
indexMap f (Index n idx) = Index n (IntMap.map f idx)
