module KMeans where

import LoadDataset
import System.Random
import Control.Monad (liftM)
import Data.List (sort, groupBy, sortBy)

type Label = Int
type Example = [Feature]
type Centroid = (Example, Label)
type DSet = [(Example, Label)]

initCentroids :: [Centroid]
initCentroids = [([2.5, 3.0, 4.0, 0.5], 1), ([2.1, 3.1, 4.3, 0.3], 2), ([8.3, 3.3, 4.4, 1.0], 3)]

takeEvery :: Int -> [a] -> [[a]]
takeEvery n = takeWhile (not . null) . map (take n) . iterate (drop n)

_initCentroids n_features k_clusters = do
    g <- newStdGen
    let x = take (n_features * k_clusters) $ (randomRs (0 :: Float, 10 :: Float) g)
    let y = [1..k_clusters]
    return . zip (takeEvery n_features x) $ y  


example :: Example
example = [5.1,3.5,1.4,0.2]

-- | Computa a distância 📏 entre duas instâncias
distance :: Example -> Example -> Float
distance x1 x2 = sqrt . sum . map (\(a, b) -> (a - b) ^ 2) . zip x1 $ x2

-- | Computa a label de uma instância
computeLabel :: Example -> [Centroid] -> Label
computeLabel ex cs = snd . head . sort . map (\(inst, label) -> ((distance ex inst), label) ) $ cs

-- | Computa novas labels baseado nos centroids atuais
step :: [Centroid] -> [Example] -> [(Example, Label)]
step centroids exs = map (\ex -> let label = computeLabel ex centroids in (ex, label)) exs

-- | Computa um novo centroid para instâncias de uma mesma classe
-- calcCentroid :: [(Example, Label)] -> Centroid
-- calcCentroid ds@((_, l):xs) = (map (/ len) x, l)
--     where
--         x = foldr1 (\a b -> zipWith (+) a b) . map fst $ ds
--         len = length ds

-- | Computa novos centroids
-- computeCentroids :: Int -> [(Example, Label)] -> [Centroid]
computeCentroids k ds = map (\l -> f l ds) [1..k]
    where
        f l xs = filter (\(fts, lb) -> lb == l)

initIris :: [(Example, Label)]
initIris = step initCentroids x
    where
        x = dataSetX irisDataSet

-- getCentroids randData = 
--     where
--         classes = [1..k]

kmeans k dataset = do
    let x = dataSetX dataset
    let size = length x
    g <- newStdGen
    let y = take size $ (randomRs (1 :: Int, k :: Int) g)
    let randomized = zip x y
    print $ length randomized
    let r = groupBy (\(_, x) (_, y) -> x == y) randomized
    print $ r !! 2
    return ()