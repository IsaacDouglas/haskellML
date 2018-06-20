module Main where

import System.IO  
import LoadDataset
import NaiveBayes
import KMeans

main :: IO ()
main = do
  putStrLn "hello world"
  
  -- Abre o arquivo .data e cria o DataSet
  file <- readFile "data/iris.data"
  let dataSet = makeDataSet file '\n'

  -- Treina o NaiveBayes
  let predict = predictNaive dataSet
  print $ predict [5.1,3.5,1.4,0.2] -- Iris-setosa
  print $ predict [5.1,2.5,3.0,1.1] -- Iris-versicolor
  print $ predict [6.4,3.1,5.5,1.8] -- Iris-virginica

  return ()