--PROBLEMA 1
--(a -> b -> a) -> a -> [b] -> [a]
prefsufs :: [a] -> [[a]]
prefsufs a@(x:xs) = (scanl (\a b -> a ++ [b]) [x] xs) ++ (scanr (:) [last xs] (init xs))


--PROBLEMA 2
--2.1
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f a = until (\x -> (f x) == x) f a

--2.2
nsqrt :: (Fractional a, Eq a) =>  a -> a
nsqrt y = fixedPoint (\x -> ((y/x)+x)/2.0) 1.0

--PROBLEMA 3
--3.1
data Polynomial a = Pol [a] deriving (Show)

instance (Eq a, Num a) => Eq (Polynomial a) where
    (Pol a) == (Pol b) = (reverse $ dropWhile (==0) $ reverse a) == (reverse $ dropWhile (==0) $ reverse b)

--3.2
polF f x [] = x
polF f [] y = y
polF f (x:xs) (y:ys) = (f x y) : polF f xs ys


polProdAux aj bij = foldr (+) 0 (zipWith (*) aj bij)

polProd :: Num a => [a] -> [a] -> [a]
polProd a b = map (\x -> polProdAux (take (x+1) a) (reverse (take (x+1) b))) [0..(2*n)]
    where n = max (length a) (length b)

instance (Num a) => Num (Polynomial a) where
    (Pol a) + (Pol b) = Pol (polF (+) a b)
    (Pol a) * (Pol b) = Pol (polProd a b) --NOT WORKING AT ALL...
    abs (Pol a) = Pol (map abs a)
    signum (Pol a) = Pol [signum (head a)]
    fromInteger a = Pol [(fromInteger a)]

--PROBLEMA 4
--4.1
data Dand a = Nor [AndOr a] | OLeaf a
data AndOr a = Nand [Dand a] | ALeaf a

--4.2
evalAux :: (a -> Bool) -> (Dand a) -> Bool
evalAux f (OLeaf a) = f a
evalAux f (Nor a) = foldr (\a b -> (eval f a) || b) False a

eval :: (a -> Bool) -> (AndOr a) -> Bool
eval f (ALeaf a) = f a
eval f (Nand a) = foldr (\a b -> (evalAux f a) && b) True a
