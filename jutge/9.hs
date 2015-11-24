ones :: [Integer]
ones = 1:ones

nats :: [Integer]
nats = iterate (+1) 0

aux f ac = (f ac) ++ aux f (ac+1)
ints :: [Integer]
ints = 0 : aux (\x -> [x,-x]) 1;

triangulars :: [Integer]
triangulars = aux (\x -> [div (x*(x+1)) 2]) 0


--auxFact ac = q:auxFact (ac+1)
--  where q = foldr (\l ac -> l*ac) 1 [1..ac]
factorials :: [Integer]
--factorials = 1:auxFact 1
factorials = scanl (*) 1 (iterate (+1) 1)


fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

isqrt 0 = 0
isqrt 1 = 1
isqrt n = head $ dropWhile (\x -> x*x > n) $ iterate (\x -> (x + n `div` x) `div` 2) (n `div` 2)

isPrimer :: Integer -> Bool
isPrimer x = foldr (\a b -> (rem x a /= 0) && b) True (2:[3,5..(isqrt x)])

primes :: [Integer]
primes = 2:filter isPrimer (iterate (+2) 3)

hammAux :: [Integer] -> [Integer] -> [Integer] -> [Integer]
hammAux f@(x:xs) s@(y:ys) t@(z:zs) = [q]++hammAux xx yy zz
  where q = min (min x y) z
        xx = dropWhile (\o -> o <= q) f
        yy = dropWhile (\o -> o <= q) s
        zz = dropWhile (\o -> o <= q) t

hammings :: [Integer]
hammings = 1:hammAux (scanl (*) 2 (iterate (+0) 2)) (scanl (*) 3 (iterate (+0) 3)) (scanl (*) 5 (iterate (+0) 5))
