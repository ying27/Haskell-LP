myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (c:d) = [f x | x <- (c:d), True ]


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (a:b) = [x | x <- (a:b), f x]

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f [] [] = []
myZipWith f (a:b) (c:d) = [f x y | (x,y) <- zip (a:b) (c:d), True]

thingify :: [Int] -> [Int] -> [(Int, Int)]
thingify [] [] = []
thingify (a:b) [] = []
thingify [] (a:b) = []
thingify (a:b) (c:d) =  [(x,y) | x <- (a:b), y <- (c:d), mod x y == 0]

factors :: Int -> [Int]
factors a = [x | x <- [1..a], mod a x == 0]
