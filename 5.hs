auxFlatten [] [] = []
auxFlatten [] y = y
auxFlatten (d:e) y = (d:e) ++ y

flatten :: [[Int]] -> [Int]
flatten [] = []
--flatten ([]:c) = foldr auxFlatten [] (c)
flatten (a:c) = foldr auxFlatten [] (a:c)

myLength :: String -> Int
myLength [] = 0
myLength (a:b) = foldr (\x y -> y+1) 0 (a:b)

myReverse :: [Int] -> [Int]
myReverse [] = []
myReverse (a:b) = foldr (\x y -> y++[x]) [] (a:b)

auxCount ([]) c = 0
auxCount (a:b) c = length [z | z <- (a:b), z == c]

countIn :: [[Int]] -> Int -> [Int]
countIn [] c = []
countIn [[]] c = [0]
countIn (a:c) x = zipWith auxCount (a:c) (  replicate ( length (a:c)  ) x )

--countIn ((a:b):c) (d:e) = zipWith auxCount ((a:b):c) (d:e)

firstWord :: String -> String
firstWord [] = []
--firstWord (a:b) = takeWhile (\a -> a /= ' ') (dropWhile (\x -> x == ' ') (a:b))
firstWord = takeWhile (/=' ') . dropWhile (== '')

--zipWith auxCount [[],[],[],[]] (replicate (length  [[],[],[],[]]) 1)
