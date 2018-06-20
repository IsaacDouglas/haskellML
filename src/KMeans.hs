module KMeans where

import LoadDataset
import Data.KMeans (kmeans)

mapToFrac :: [[Feature]] -> [[Double]]
mapToFrac = map (map realToFrac)

runKM :: Int -> [[Feature]] -> [[[Double]]]
runKM k dt = kmeans k dataset
    where
        dataset = mapToFrac dt