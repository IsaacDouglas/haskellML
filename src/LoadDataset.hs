module LoadDataset where

type Feature = Float
type Class = String
type Instance = ([Feature], Class)

data DataSet = DataSet [Instance] deriving (Show)

initDataSet :: DataSet
initDataSet = DataSet []

insert :: DataSet -> Instance -> DataSet
insert (DataSet []) i = DataSet [i]
insert (DataSet d) i = DataSet (i:d)

makeInstance :: String -> Instance
makeInstance s = ([(read x :: Float) | x<- (take (length f - 1) f)], last f)
                where
                    f = split s ','

split :: String -> Char -> [String]
split [] _ = []
split s c = takeWhile (/= c) s : split rest c
          where
            rest = drop 1 . dropWhile (/= c) $ s

instance1 :: Instance
instance1 = ([4.8,3.0,1.4,0.3], "Iris-setosa")

instance2 :: Instance
instance2 = ([6.5,2.8,4.6,1.5], "Iris-versicolor")

instance3 :: Instance
instance3 = ([6.3,3.3,6.0,2.5], "Iris-virginica")

text :: String
text = "4.8,3.0,1.4,0.3,Iris-setosa"