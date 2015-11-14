myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f a [] = a
myFoldl f a (x:y) =
    myFoldl f q y
    where q = f a x


myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f a [] = a;
myFoldr f a (x:y) = f x (myFoldr f a y)


myIterate :: (a -> a) -> a -> [a]
myIterate f x = [x]++myIterate f (f x)


myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil boo f x
  | not (boo x) = myUntil boo f (f x)
  | otherwise = x


myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (a:b) = [f x | x <- (a:b), True]


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (a:b) = [x | x <- (a:b), f x]


myAll :: (a -> Bool) -> [a] -> Bool
--myAll f (c:d) = null [x | x <- (c:d), not (f x)]
myAll f l = myFoldr (\x y -> f x && y) True l


myAny :: (a -> Bool) -> [a] -> Bool
--myAny f (a:b) = not (null [x | x <- (a:b), f x])
myAny f l = myFoldr (\x y-> f x || y) False l


myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (a:b) (c:d) = (a,c):myZip b d

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f [] [] = []
--myZipWith f (a:b) (c:d) = [f x y | (x,y) <- myZip (a:b) (c:d), True]
myZipWith f a c =  myMap (\(e,r) -> f e r)  (myZip a c)
