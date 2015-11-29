--PROBLEMA 1
sum2 :: [[Int]] -> Int
sum2 l1 = foldr (\a b -> (sum a) + b) 0 l1

--PROBLEMA 2
fsmap :: a -> [(a -> a)] -> a
fsmap a f = foldl (\a b -> (b a)) a f

--PROBLEMA 3
isDiv :: [Int] -> Int -> Bool
isDiv l n = foldr (\a b -> (mod n a) == 0 || b) False l

knoDiv :: Int -> [Int] -> Int
knoDiv n l = head (foldr (\a b -> (dropWhile (isDiv l) (tail b))) [1..] [1..n])

--PROBLEMA 4
data Arbre a = Node a [Arbre a] deriving (Show)

simetric :: Arbre a -> Arbre a
simetric x@(Node a []) = x
simetric (Node a b) = Node a (foldl (\a b -> (simetric b):a) [] b)


--PROBLEMA 5
--5.a
data Cua a = Cua [a] [a] deriving (Show)

cBuida :: Cua a
cBuida = Cua [] []

afegir :: a -> Cua a -> Cua a
afegir a (Cua cap cua) = Cua cap (a:cua)

avancar :: Cua a -> (a, Cua a)
avancar (Cua [] cua) =  (last cua, Cua (reverse (init cua)) [])
avancar (Cua (x:xs) cua) = (x,(Cua xs cua))

esBuida :: Cua t -> Bool
esBuida (Cua [] []) = True
esBuida (Cua _ _) = False

instance (Eq a) => Eq (Cua a) where
    Cua a b == Cua c d = a++(reverse b) == c++(reverse d)
