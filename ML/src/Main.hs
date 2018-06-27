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
  file <- readFile "../data/iris.data"
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

  -- | Mostra a precisao
  putStrLn $ "Precision: " ++ show (precision predict test)

  -- | DataSet com as classes preditas pelo classificador
  let resultPredict = predictDataSetResult predict test

  -- | Escreve no arquivo o DataSet de teste
  writeFile "../data/out.data" (intercalate "\n" $ printDataSet resultPredict)

  return ()