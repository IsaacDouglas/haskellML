module NaiveBayes where

import LoadDataset

mean :: [Float] -> Float
mean [] = 0.0
mean l = (sum l) / (fromIntegral $ length l)

variance :: [Float] -> Float
variance l = sum [ (q (x - m)) | x <- l ] / (fromIntegral $ (length l) - 1) 
        where 
            m = mean l
            q n = n*n

varianceM :: [Float] -> Float -> Float
varianceM l m = sum [ (q (x - m)) | x <- l ] / (fromIntegral $ (length l) - 1) 
        where 
            q n = n*n 

