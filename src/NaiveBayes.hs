module NaiveBayes where

import LoadDataset

-- | Calcula a media
mean :: [Float] -> Float
mean [] = 0.0
mean l = (sum l) / (fromIntegral $ length l)

-- | Calcula a variancia
variance :: [Float] -> Float
variance l = sum [ (q (x - m)) | x <- l ] / (fromIntegral $ (length l) - 1) 
        where 
            m = mean l
            q n = n*n

-- | Recebe o x que é o valor a ser previsto, media e a variancia
posteriorX :: Float -> Float -> Float -> Float
posteriorX x m v = (1 / sqrt (2*pi*v)) *  exp (-1 * (((**) (x - m) 2)) / (2*v)) :: Float

teste :: String
teste = "6,180,12,male;5.92,190,11,male;5.58,170,12,male;5.92,165,10,male;5,100,6,female;5.5,150,8,female;5.42,130,7,female;5.75,150,9,female"

testeDS :: DataSet
testeDS = makeDataSet teste ';'

teste2 :: String
teste2 = "-3,7,3;1,5,3;1,2,3;-2,0,3;2,3,4;-4,0,3;-1,1,3;1,1,4;-2,2,3;2,7,4;-4,1,4;-2,7,4"

teste2DS :: DataSet
teste2DS = makeDataSet teste2 ';'

type PairMV = (Float, Float)
type NaiveInstance = ([PairMV], Float, Class) 
data NaiveData = NaiveData [NaiveInstance] deriving (Show, Eq)
type Prob = (Float, Class)

-- | Retorna uma NaiveInstance para a Class passada
makeNaiveInstance :: DataSet -> Class -> NaiveInstance
makeNaiveInstance d c = ([ (mean x, variance x) | x <- t ], (probPri d c) , c)
                where
                  t = transpose (dataSetX (dataSetByClass d c))

-- | Retorna a matriz transposta
transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

-- | Transforma o DataSet em NaiveData para usar nos calculos
makeNaiveData :: DataSet -> NaiveData
makeNaiveData d = NaiveData [ makeNaiveInstance d c | c <- getClass d ]

-- | Calcula a probabilidade posterior
proPosterior :: [Feature] -> NaiveInstance -> Prob
proPosterior f (p, pri, c) = ((aux f (p, pri, c)), c)
                  where
                    aux [] _ = 1 * pri
                    aux (x:xs) (((m, v):ys), w, z) = (posteriorX x m v) * aux xs (ys, w, z)

-- | Calcula a probabilidade para cada Class da NaiveData
probClass :: [Feature] -> NaiveData -> [Prob]
probClass f (NaiveData n) = [ (proPosterior f x) | x <- n ]

-- | 
predictNaive :: DataSet -> [Feature] -> Class
predictNaive d f = snd (maxP (probClass f (makeNaiveData  d)))

-- | Retorna a maior probabilidade 
maxP :: [Prob] -> Prob
maxP ((p,c):xs) = aux xs (p, c)
                where
                  aux [] r = r
                  aux ((x1,y1):zs) (x, y) | x1 > x = aux zs (x1,y1)
                                          | otherwise = aux zs (x, y)

-- | Retorna a probabilidade a priore de ser de uma classe
probPri :: DataSet -> Class -> Float
probPri d c =  (fromIntegral $ len (dataSetByClass d c)) / (fromIntegral $ len d)
                where 
                  len (DataSet z) = length z