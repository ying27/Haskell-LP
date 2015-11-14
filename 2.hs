myLength :: [Int] -> Int
myLength [] = 0
myLength (x:y) = myLength y + 1



myMaximum :: [Int] -> Int
myMaximum (f:[]) = f;
myMaximum (x:y)
	| x > max = x
	| otherwise = max
	where max = myMaximum y



average :: [Int] -> Float
average (x:[]) = (fromIntegral x)
average (x:y) =  (fromIntegral suma) / (fromIntegral n)
	where { n = myLength (x:y);
	     suma = sum (x:y)}



buildPalindrome :: [Int] -> [Int]
buildPalindrome [] = []
buildPalindrome (x:y) =
	inver++(x:y)
	where inver = reverse (x:y)


remove :: [Int] -> [Int] -> [Int]
remove ([]) (rx:ry) = []
remove (rx:ry) ([]) = (rx:ry)
remove ([]) ([]) = []
remove (x:y) (rx:ry)
	| notElem x (rx:ry) = [x] ++ remove y (rx:ry)
	| otherwise =  remove y (rx:ry)



flatten :: [[Int]] -> [Int]
flatten ([]) = []
flatten ([]:r) = flatten r
flatten ((x:y):z) = (x:y)++flatten z



oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens [] = ([],[])
oddsNevens (x:y)
	| mod x 2 /= 0 = ([x]++a,b)
	| otherwise = (a,[x]++b)
	where (a, b) = oddsNevens y


isPrime 0 = False
isPrime 1 = False
isPrime n =
	[] == [elemnt | elemnt <- [2..n-1], mod n elemnt == 0]


--primeDivisorsAux x y
--	| x == y = []
--	| mod x y == 0 && isPrime y = [y]++primeDivisorsAux x (y+1)
--	| otherwise = primeDivisorsAux x (y+1)

primeDivisors :: Int -> [Int]
primeDivisors 0 = []
primeDivisors x =
	[elem | elem <- [1..(x)], mod x elem == 0 && isPrime elem]

--primeDivisors :: Int -> [Int]
--primeDivisors 0 = []
--primeDivisors x =
--	primeDivisorsAux x 1
