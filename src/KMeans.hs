module KMeans where

import LoadDataset

type Example = [Feature]
type Centroid = [Feature]

-- | Computa a distância 📏 entre duas instâncias
distance :: Example -> Example -> Float
distance x1 x2 = sqrt . sum . map (\(a, b) -> a - b) . zip x1 $ x2