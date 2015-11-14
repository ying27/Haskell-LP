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

--hammAux a b = ((q && a < 7) || not q) && b
--      where q = isPrimer a

hammings :: [Integer]
--hammings = 1:2:3:4:5:6:filter (\x -> foldr (\a b -> not ((rem x a == 0) && (isPrimer a)) && b) True (x:[7,9..(div x 2)])) (iterate (+1) 7)
--hammings = 1:2:3:4:5:6:8:9:filter (\x -> not (any isPrimer ([t | t <- x:[7,9..(div x 2)], rem x t == 0]))) (iterate (+1) 10)
hammings = 1:2:3:4:5:6:filter (\x -> foldr (\a b -> not ((mod x a == 0) && (isPrimer a)) && b) True (x:[7,9..(div x 2)])) (iterate (+1) 7)



--hammings = filter (\x -> all (\y -> y < 7 ) [z | z <- x:2:[3,5..(div x 2)], rem x z == 0 && isPrimer z] ) (iterate (+1) 1)
--hammings = 1:2:3:4:5:filter (\x -> null [z | z <- x:[7,9..(div x 2)], rem x z == 0 && isPrimer z])     (iterate (+1) 6)
--hammings = 1:2:3:4:5:6:filter (\x ->  not (isPrimer x) || hammAux x)  (iterate (+1) 7)
--foldr :: (a -> b -> b) -> b -> [a] -> b
--hammAux a b = ((q && a < 7) || not q) && b
--      where q = isPrime a

--hammings :: [Integer]
--hammings = filter (\x -> foldr hammAux True [z|z<-[1..x], mod x z == 0]) (iterate (+1) 1)
