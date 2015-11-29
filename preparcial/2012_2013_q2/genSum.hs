--PROBLEMA 1
--1.a
genSum :: [Integer]
genSum = scanl (+) 0 [1..]

--1.b
esSum :: Integer -> Bool
esSum n = elem n (takeWhile (n >= ) genSum)

--PROBLEMA 2
selPred :: (Ord a, Num a, Eq a) => a -> [(a->Bool)] -> ([(a->Bool)],[(a->Bool)])
selPred a [] = ([],[])
selPred a x = foldr (\c (lh,lt) -> if (c a) then (c:lh,lt) else (lh,c:lt)) ([],[]) x

check :: a -> [(a -> b)] -> [b]
check a b = map (\x -> x a) b

--PROBLEMA 3
data Arbre a = Node a (Arbre a) (Arbre a) | Abuit

arbreFibonacci :: Arbre a -> Int
arbreFibonacci Abuit = 0
arbreFibonacci (Node a Abuit Abuit) = 1
arbreFibonacci (Node a b c)
    | r >= 0 && l > r && l == (r+1) = l+1
    | otherwise = -1
    where l = arbreFibonacci b
          r = arbreFibonacci c

--PROBLEMA 4

data MOList a = Mol [[a]]

--4.a
esta :: (Eq a) => a -> MOList a -> Bool
esta a (Mol b) = foldr (\e f -> (elem a e) || f) False b

inclosa :: (Eq a) => [a] -> MOList a -> Bool
inclosa a x@(Mol b)
    | (length a) == (length b) = foldr (\c d -> (esta c x) && d) True a
    | otherwise = False


--4.b
instance (Eq a) => Eq (MOList a) where
    Mol a == Mol b = a == b
