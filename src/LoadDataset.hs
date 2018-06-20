module LoadDataset where

import System.Random (newStdGen, randomRs)
import Data.List (sort, nub)

type Feature = Float
type Class = String
type Instance = ([Feature], Class)

data DataSet = DataSet [Instance] deriving (Show, Eq)

-- | DataSet vazio
initDataSet :: DataSet
initDataSet = DataSet []

-- | Insere uma Instance em um DataSet
insert :: DataSet -> Instance -> DataSet
insert (DataSet []) i = DataSet [i]
insert (DataSet d) i = DataSet (i:d)

-- | Cria um DataSet a partir de uma String
makeDataSet :: String -> Char -> DataSet
makeDataSet s c = DataSet [makeInstance x | x <- (split s c), ((length x) /= 0)]

-- | Cria uma Instance a partir de uma String
makeInstance :: String -> Instance
makeInstance s = ([(read x :: Float) | x <- (take (length f - 1) f)], last f)
                where
                    f = split s ','

-- | Separa uma String em uma lista de String por um Char passado
split :: String -> Char -> [String]
split [] _ = []
split s c = takeWhile (/= c) s : split rest c
          where
            rest = drop 1 . dropWhile (/= c) $ s

-- 7. Attribute Information:
-- 1. sepal length in cm
-- 2. sepal width in cm
-- 3. petal length in cm
-- 4. petal width in cm
-- 5. class: 
   -- Iris Setosa
   -- Iris Versicolour
   -- Iris Virginica

iris :: String
iris = "5.1,3.5,1.4,0.2,Iris-setosa;4.9,3.0,1.4,0.2,Iris-setosa;4.7,3.2,1.3,0.2,Iris-setosa;4.6,3.1,1.5,0.2,Iris-setosa;5.0,3.6,1.4,0.2,Iris-setosa;5.4,3.9,1.7,0.4,Iris-setosa;4.6,3.4,1.4,0.3,Iris-setosa;5.0,3.4,1.5,0.2,Iris-setosa;4.4,2.9,1.4,0.2,Iris-setosa;4.9,3.1,1.5,0.1,Iris-setosa;5.4,3.7,1.5,0.2,Iris-setosa;4.8,3.4,1.6,0.2,Iris-setosa;4.8,3.0,1.4,0.1,Iris-setosa;4.3,3.0,1.1,0.1,Iris-setosa;5.8,4.0,1.2,0.2,Iris-setosa;5.7,4.4,1.5,0.4,Iris-setosa;5.4,3.9,1.3,0.4,Iris-setosa;5.1,3.5,1.4,0.3,Iris-setosa;5.7,3.8,1.7,0.3,Iris-setosa;5.1,3.8,1.5,0.3,Iris-setosa;5.4,3.4,1.7,0.2,Iris-setosa;5.1,3.7,1.5,0.4,Iris-setosa;4.6,3.6,1.0,0.2,Iris-setosa;5.1,3.3,1.7,0.5,Iris-setosa;4.8,3.4,1.9,0.2,Iris-setosa;5.0,3.0,1.6,0.2,Iris-setosa;5.0,3.4,1.6,0.4,Iris-setosa;5.2,3.5,1.5,0.2,Iris-setosa;5.2,3.4,1.4,0.2,Iris-setosa;4.7,3.2,1.6,0.2,Iris-setosa;4.8,3.1,1.6,0.2,Iris-setosa;5.4,3.4,1.5,0.4,Iris-setosa;5.2,4.1,1.5,0.1,Iris-setosa;5.5,4.2,1.4,0.2,Iris-setosa;4.9,3.1,1.5,0.1,Iris-setosa;5.0,3.2,1.2,0.2,Iris-setosa;5.5,3.5,1.3,0.2,Iris-setosa;4.9,3.1,1.5,0.1,Iris-setosa;4.4,3.0,1.3,0.2,Iris-setosa;5.1,3.4,1.5,0.2,Iris-setosa;5.0,3.5,1.3,0.3,Iris-setosa;4.5,2.3,1.3,0.3,Iris-setosa;4.4,3.2,1.3,0.2,Iris-setosa;5.0,3.5,1.6,0.6,Iris-setosa;5.1,3.8,1.9,0.4,Iris-setosa;4.8,3.0,1.4,0.3,Iris-setosa;5.1,3.8,1.6,0.2,Iris-setosa;4.6,3.2,1.4,0.2,Iris-setosa;5.3,3.7,1.5,0.2,Iris-setosa;5.0,3.3,1.4,0.2,Iris-setosa;7.0,3.2,4.7,1.4,Iris-versicolor;6.4,3.2,4.5,1.5,Iris-versicolor;6.9,3.1,4.9,1.5,Iris-versicolor;5.5,2.3,4.0,1.3,Iris-versicolor;6.5,2.8,4.6,1.5,Iris-versicolor;5.7,2.8,4.5,1.3,Iris-versicolor;6.3,3.3,4.7,1.6,Iris-versicolor;4.9,2.4,3.3,1.0,Iris-versicolor;6.6,2.9,4.6,1.3,Iris-versicolor;5.2,2.7,3.9,1.4,Iris-versicolor;5.0,2.0,3.5,1.0,Iris-versicolor;5.9,3.0,4.2,1.5,Iris-versicolor;6.0,2.2,4.0,1.0,Iris-versicolor;6.1,2.9,4.7,1.4,Iris-versicolor;5.6,2.9,3.6,1.3,Iris-versicolor;6.7,3.1,4.4,1.4,Iris-versicolor;5.6,3.0,4.5,1.5,Iris-versicolor;5.8,2.7,4.1,1.0,Iris-versicolor;6.2,2.2,4.5,1.5,Iris-versicolor;5.6,2.5,3.9,1.1,Iris-versicolor;5.9,3.2,4.8,1.8,Iris-versicolor;6.1,2.8,4.0,1.3,Iris-versicolor;6.3,2.5,4.9,1.5,Iris-versicolor;6.1,2.8,4.7,1.2,Iris-versicolor;6.4,2.9,4.3,1.3,Iris-versicolor;6.6,3.0,4.4,1.4,Iris-versicolor;6.8,2.8,4.8,1.4,Iris-versicolor;6.7,3.0,5.0,1.7,Iris-versicolor;6.0,2.9,4.5,1.5,Iris-versicolor;5.7,2.6,3.5,1.0,Iris-versicolor;5.5,2.4,3.8,1.1,Iris-versicolor;5.5,2.4,3.7,1.0,Iris-versicolor;5.8,2.7,3.9,1.2,Iris-versicolor;6.0,2.7,5.1,1.6,Iris-versicolor;5.4,3.0,4.5,1.5,Iris-versicolor;6.0,3.4,4.5,1.6,Iris-versicolor;6.7,3.1,4.7,1.5,Iris-versicolor;6.3,2.3,4.4,1.3,Iris-versicolor;5.6,3.0,4.1,1.3,Iris-versicolor;5.5,2.5,4.0,1.3,Iris-versicolor;5.5,2.6,4.4,1.2,Iris-versicolor;6.1,3.0,4.6,1.4,Iris-versicolor;5.8,2.6,4.0,1.2,Iris-versicolor;5.0,2.3,3.3,1.0,Iris-versicolor;5.6,2.7,4.2,1.3,Iris-versicolor;5.7,3.0,4.2,1.2,Iris-versicolor;5.7,2.9,4.2,1.3,Iris-versicolor;6.2,2.9,4.3,1.3,Iris-versicolor;5.1,2.5,3.0,1.1,Iris-versicolor;5.7,2.8,4.1,1.3,Iris-versicolor;6.3,3.3,6.0,2.5,Iris-virginica;5.8,2.7,5.1,1.9,Iris-virginica;7.1,3.0,5.9,2.1,Iris-virginica;6.3,2.9,5.6,1.8,Iris-virginica;6.5,3.0,5.8,2.2,Iris-virginica;7.6,3.0,6.6,2.1,Iris-virginica;4.9,2.5,4.5,1.7,Iris-virginica;7.3,2.9,6.3,1.8,Iris-virginica;6.7,2.5,5.8,1.8,Iris-virginica;7.2,3.6,6.1,2.5,Iris-virginica;6.5,3.2,5.1,2.0,Iris-virginica;6.4,2.7,5.3,1.9,Iris-virginica;6.8,3.0,5.5,2.1,Iris-virginica;5.7,2.5,5.0,2.0,Iris-virginica;5.8,2.8,5.1,2.4,Iris-virginica;6.4,3.2,5.3,2.3,Iris-virginica;6.5,3.0,5.5,1.8,Iris-virginica;7.7,3.8,6.7,2.2,Iris-virginica;7.7,2.6,6.9,2.3,Iris-virginica;6.0,2.2,5.0,1.5,Iris-virginica;6.9,3.2,5.7,2.3,Iris-virginica;5.6,2.8,4.9,2.0,Iris-virginica;7.7,2.8,6.7,2.0,Iris-virginica;6.3,2.7,4.9,1.8,Iris-virginica;6.7,3.3,5.7,2.1,Iris-virginica;7.2,3.2,6.0,1.8,Iris-virginica;6.2,2.8,4.8,1.8,Iris-virginica;6.1,3.0,4.9,1.8,Iris-virginica;6.4,2.8,5.6,2.1,Iris-virginica;7.2,3.0,5.8,1.6,Iris-virginica;7.4,2.8,6.1,1.9,Iris-virginica;7.9,3.8,6.4,2.0,Iris-virginica;6.4,2.8,5.6,2.2,Iris-virginica;6.3,2.8,5.1,1.5,Iris-virginica;6.1,2.6,5.6,1.4,Iris-virginica;7.7,3.0,6.1,2.3,Iris-virginica;6.3,3.4,5.6,2.4,Iris-virginica;6.4,3.1,5.5,1.8,Iris-virginica;6.0,3.0,4.8,1.8,Iris-virginica;6.9,3.1,5.4,2.1,Iris-virginica;6.7,3.1,5.6,2.4,Iris-virginica;6.9,3.1,5.1,2.3,Iris-virginica;5.8,2.7,5.1,1.9,Iris-virginica;6.8,3.2,5.9,2.3,Iris-virginica;6.7,3.3,5.7,2.5,Iris-virginica;6.7,3.0,5.2,2.3,Iris-virginica;6.3,2.5,5.0,1.9,Iris-virginica;6.5,3.0,5.2,2.0,Iris-virginica;6.2,3.4,5.4,2.3,Iris-virginica;5.9,3.0,5.1,1.8,Iris-virginica"

irisDataSet :: DataSet
irisDataSet = makeDataSet iris ';'

-- | Separa do DataSet a lista de Feature
dataSetX :: DataSet -> [[Feature]]
dataSetX (DataSet d) = [ x | (x, y) <- d ]

-- | Separa do DataSet a lista de Class
dataSetY :: DataSet -> [Class]
dataSetY (DataSet d) = [ y | (x, y) <- d ]

-- | Retorna um DataSet só com a Class escolhida 
dataSetByClass :: DataSet -> Class -> DataSet
dataSetByClass (DataSet d) s = DataSet [ (x, y) | (x, y) <- d, y==s ]

-- | Retorna todas as Class do DataSet
getClass :: DataSet -> [Class]
getClass d = aux (dataSetY d) []
          where
            aux [] l = l
            aux (x:xs) l | elem x l = aux xs l
                        | otherwise = aux xs (x:l)
          
-- | Retorna uma lista contendo todos os DataSet de cada Class do DataSet original
splitByClass :: DataSet -> [DataSet]
splitByClass d = [ (dataSetByClass d x) | x <- class' ]
          where
            class' = getClass d


-- | Split DataSet (Teste, Treinamento)
splitDataSet :: DataSet -> [Integer] -> (DataSet, DataSet)
splitDataSet (DataSet d) v = (DataSet (addList d v 1), DataSet (subList d v 1))

printInstance :: Instance -> String
printInstance i = dropStr (show i) ['"', ']', '[', ')', '(']

printDataSet :: DataSet -> [String]
printDataSet (DataSet d) = [ printInstance i | i <- d ]

-- | Mostra uma lista de Instance na tela
printElements :: [Instance] -> IO()
printElements [] = return ()
printElements (x:xs) = do putStrLn $ dropStr (show x) ['"', ']', '[', ')', '(']
                          printElements xs

-- | Recebe um DataSet e retorna a lista de Instance
listInstance :: DataSet -> [Instance]
listInstance (DataSet d) = d 

-- | Tamanho do DataSet
sizeDS :: DataSet -> Integer
sizeDS (DataSet x) = fromIntegral $ length x

-- | Retorna a lista contendo os valores das posicoes passadas
addList :: [l] -> [Integer] -> Integer -> [l]
addList [] _ _ = []
addList _ [] _ = []
addList (x:xs) (y:ys) v | y == v = x : addList xs ys (v+1)
                        | otherwise = addList xs (y:ys) (v+1)

-- | Retorna a lista sem os valores das posicoes passadas                        
subList :: [l] -> [Integer] -> Integer -> [l]
subList [] _ _ = []
subList l [] _ = l
subList (x:xs) (y:ys) v | y == v = subList xs ys (v+1)
                        | otherwise = x : subList xs (y:ys) (v+1)

-- | Retorna uma lista de valores aleatorios sem repeticao
ordRandom :: Float -> Integer -> IO [Integer]               
ordRandom p n = do 
  g <- newStdGen
  let prob = floor $ (fromIntegral n) * p
  let num = nub $ sort $ take prob (randomRs (1 :: Integer, n :: Integer) g)
  return num

-- | Verifica se a predicao foi correta
checkPredict :: ([Feature] -> Class) -> Instance -> Bool
checkPredict f i = f (fst i) == snd i

-- | Verifica uma lista de de predicao
checkPredicts :: ([Feature] -> Class) -> DataSet -> [Bool]
checkPredicts f (DataSet d) = [ f (fst b) == snd b | b <- d ]

-- | Verifica a predicao com as classes
checkPredicts2 :: ([Feature] -> Class) -> DataSet -> [(Instance, Class)]
checkPredicts2 f (DataSet d) = [ (b, f (fst b)) | b <- d ]

-- | Calcula a acuracia
acurracy :: ([Feature] -> Class) -> DataSet -> Float
acurracy f (DataSet d) = fromIntegral (length (filter (\x -> x) (checkPredicts f (DataSet d)))) / fromIntegral (length d)

-- | Remove determinados Char de uma String
dropStr :: String -> [Char] -> String
dropStr [] _ = []
dropStr (x:xs) c | contains x c = dropStr xs c
                 | otherwise = x: (dropStr xs c)

-- | Verifica se um Char pertence a uma lista de Char
contains :: Char -> [Char] -> Bool
contains c [] = False
contains c (x:xs) | c == x = True
                  | otherwise = contains c xs