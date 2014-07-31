{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}


module KMeansFork (kmeans, Point, Distance, Cluster(..), computeClusters, euclidD, l1Dist, lInfDist, iterativeSplit, oneBigList) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as G
import qualified Data.List as L
import Data.Function (on)
import System.Random

--- * K-Means clustering algorithm

-- | Type holding an object of any type and its associated feature vector
type Point a = (V.Vector Double, a)

type Point' a = (V.Vector Double, Int ,[[Int]])


-- | Type representing a cluster (group) of vectors by its center and an id
data Cluster = Cluster {
  cid :: !Int,
  center :: !(V.Vector Double)
  } -- deriving (Show,Eq)

data Cluster' = Cluster' {
    cid' :: !Int,
    center' :: !(V.Vector Double, [[Int]])
}


type Distance' = (V.Vector Double, [[Int]]) -> (V.Vector Double, [[Int]]) -> Double

type Distance = V.Vector Double -> V.Vector Double -> Double

{-#INLINE euclidD#-}
euclidD :: Distance
euclidD u v = V.sum $ V.zipWith (\a b -> (a - b)^2) u v


euclidD' :: Distance'
euclidD' u v = if (snd u == [] && snd v == []) then euclidD (fst u) (fst v)
    else a + (fromIntegral b)
        where a = euclidD (fst u) (fst v)
              b = sum $ bitDifference (snd u) (snd v)

{-#INLINE l1Dist#-}
l1Dist :: Distance
l1Dist v1 v2 = V.sum $ V.zipWith diffabs v1 v2
    where diffabs a b = abs (a-b)

{-#INLINE lInfDist#-}
lInfDist :: Distance
lInfDist v1 v2 = V.maximum $ V.zipWith diffabs v1 v2
    where diffabs a b = abs ( a - b)


{-#INLINE iterativeSplit#-}
iterativeSplit :: Int -> [a] -> [[a]]
iterativeSplit k vs = go vs
  where go vs = case L.splitAt n vs of
          (vs', []) -> [vs']
          (vs', vss) -> vs' : go vss
        n = (length vs + k - 1) `div` k

{-#INLINE oneBigList#-}
oneBigList :: Int ->[a] -> [[a]]
oneBigList 1 xs = [xs]
oneBigList k xs = [(take 1 xs)] ++ oneBigList (k-1) (drop 1 xs)

{-#INLINE computeClusters#-}
computeClusters :: [[V.Vector Double]] -> [Cluster]
computeClusters = zipWith Cluster [0..] . map f
  where f (x:xs) = let (n, v) = L.foldl' (\(k, s) v' -> (k+1, V.zipWith (+) s v')) (1, x) xs
                   in V.map (\x -> x / (fromIntegral n)) v

{-#INLINE regroupPoints#-}
regroupPoints :: forall a. [Cluster] -> Distance -> [Point a] -> [[Point a]]
regroupPoints clusters distance points = L.filter (not.null) . G.toList . G.accum (flip (:)) (G.replicate (length clusters) []) . map closest $ points
 where
   closest p = (cid (L.minimumBy (compare `on` (distance (fst p) . center)) clusters)center in the cluster data ,p)

kmeansStep :: [Point a] -> Distance -> [[Point a]] -> [[Point a]]
kmeansStep points distance pgroups = regroupPoints (computeClusters . map (map fst) $ pgroups) distance points

kmeansAux :: [Point a] -> Distance -> [[Point a]] -> [[Point a]]
kmeansAux points distance pgroups = let pss = kmeansStep points distance pgroups in
  case map (map fst) pss == map (map fst) pgroups of
  True -> pgroups
  False -> kmeansAux points distance pss

-- | Performs the k-means clustering algorithm
-- using trying to use 'k' clusters on the given list of points
kmeans :: Int -> Distance -> (Int -> [Point a] -> [[Point a]]) -> [Point a] -> [[Point a]]
kmeans k distance partition points = kmeansAux points distance pgroups
  where pgroups = partition k points


bitDifference :: [[Int]]-> [[Int]] -> [Int]
bitDifference u v = zipWith differenceBetweenBitVector u v

differenceBetweenBitVector:: [Int] -> [Int] -> Int
differenceBetweenBitVector xs ys = length $ filter id $ zipWith (/=) xs ys