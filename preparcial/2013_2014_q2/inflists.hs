--PROBLEMA 1
--1.1
inflists :: [[Integer]]
--Solucion 1 (Se valora mas el uso de funciones de orden superior):
inflaux a = a : map (+1) (inflaux a)
inflists = map (\x -> (inflaux x)) [1..]

--Solucion 2:
--inflists = map (\x -> [x..]) [1..]

--1.2
takeLESum :: Integer -> [Integer] -> [Integer]
takeLESum n list = take (length (takeWhile (<=n) (scanl1 (+) list))) list

--1.3
consecutSum :: Integer -> [Integer]
consecutSum n = head (filter (\x -> sum x == n) (map (takeLESum n) inflists))
--take 1


--PROBLEMA 2
--2.1
dc :: (a -> Bool) -> (a -> b) -> (a -> [a]) -> (a -> [b] -> b) -> a -> b
dc t r p c prob
    | t prob = r prob
    | otherwise = c prob sol
    where sol = map (dc t r p c) (p prob)

--2.2
trivial :: [a] -> Bool
trivial [a] = True
trivial [a,b] = True
trivial _ = False

resol :: Ord a => [a] -> [a]
resol [a] = [a]
resol [a,b] = [min a b, max a b]

parteix :: [a] -> [[a]]
parteix a = [h]++[t]
    where p = div (length a) 2
          (h,t) = splitAt p a

--No acabo de entender por que combina tiene tantos parametros...
combina :: Ord a => [a] -> [[a]] -> [a]
combina _ [x,[]] = x
combina _ [[],y] = y
combina p [f@(x:xs),s@(y:ys)]
    | x < y = x : combina p [xs,s]
    | otherwise = y : combina p [f,ys]

mergesort :: Ord a => [a] -> [a]
mergesort a = dc trivial resol parteix combina a


--PROBLEMA 3
--3.1
data Expressio a = Fulla a
                | Unari (a -> a) (Expressio a)
                | Binari (a -> a -> a) (Expressio a) (Expressio a)


aval :: Expressio a -> a
aval (Fulla a) = a
aval (Unari f a) = f $ aval a
aval (Binari f a b) = f (aval a) (aval b)


instance (Eq a) => Eq (Expressio a) where
    --(Fulla a) == (Fulla b) = a =:t (==)= b
    (Fulla a) == (Fulla b) = True
    (Unari f a) == (Unari g b) = a == b
    (Binari f a b) == (Binari g c d) = a == c && b == d
    (Fulla a) == _ = False
    (Unari f a) == _ = False
    (Binari f a b) == _ = False

--3.2
data NExpressio a = NFulla a | NUnari (a -> a) (NExpressio a) | Nari (a -> a -> a) [NExpressio a]

naval :: NExpressio a -> a
naval (NFulla a) = a
naval (NUnari f a) = f $ naval a
naval (Nari f (x:xs)) = foldl (\a b -> f (naval b) a) (naval x) xs

instance (Eq a) => Eq (NExpressio a) where
    (NFulla a) == (NFulla b) = True
    (NUnari f a) == (NUnari g b) = a == b
    (Nari f a) == (Nari g c) = a == c
    (NFulla a) == _ = False
    (NUnari f a) == _ = False
    (Nari f a) == _ = False
