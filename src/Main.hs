module Main where

import System.IO  
import LoadDataset
import NaiveBayes
import KMeans
import Data.List (intercalate)

main :: IO ()
main = do
  putStrLn "hello world"
  
  -- Abre o arquivo .data e cria o DataSet
  file <- readFile "data/iris.data"
  let dataSet = makeDataSet file '\n'

  -- variar de 0 a 0.4
  let percentTest = 0.2
  ordd <- ordRandom percentTest $ sizeDS dataSet

  -- Dividir o data set em dois (Teste, Treinamento)
  let tupla = splitDataSet dataSet $ ordd
  let test = fst tupla
  let train = snd tupla 

  -- Treina o NaiveBayes
  let predict = predictNaive train

  -- | Mostra a acuracia
  print $ acurracy predict test

  -- | Escreve no arquivo o DataSet de teste
  writeFile "data/out.data" (intercalate "\n" $ printDataSet test)

  -- print $ predict [5.1,3.5,1.4,0.2] -- Iris-setosa
  -- print $ predict [5.1,2.5,3.0,1.1] -- Iris-versicolor
  -- print $ predict [6.4,3.1,5.5,1.8] -- Iris-virginica
  -- print $ predict [5.9,3.2,4.8,1.8] -- Iris-versicolor classificou como Iris-virginica
  
  -- print $ sizeDS $ fst tupla 
  -- print $ sizeDS $ snd tupla 
  -- print $ checkPredicts predict $ fst tupla
  -- print $ checkPredicts2 predict $ fst tupla
  return ()