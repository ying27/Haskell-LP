eql :: [Int] -> [Int] -> Bool
eql [] [] = True
eql (a:b) [] = False;
eql [] (c:d) = False;
eql (a:b) (c:d) = (length (a:b) == length (c:d)) && all (\x -> x) (zipWith (\x y -> x == y) (a:b) (c:d))

prod :: [Int] -> Int
prod [] = 1
prod (a:b) = foldr (\x y -> x*y) 1 (a:b)

prodOfEvens :: [Int] -> Int
prodOfEvens [] = 1
prodOfEvens (a:b) = prod (filter (\x -> even x) (a:b))

powersOf2 :: [Int]
powersOf2 = iterate (\x -> x*2) 1

scalarProduct :: [Float] -> [Float] -> Float
scalarProduct (a:b) (c:d) = foldr (\x y -> x + y) 0 (zipWith (\x y -> x*y) (a:b) (c:d))
