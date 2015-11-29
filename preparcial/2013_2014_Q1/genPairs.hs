--EJERCICIO 1
genPairs :: (Eq a) => [a] -> [a] -> [a] -> [(a,a)]
genPairs a b c = [(x,y) | x <- a, y <- b, (elem x c && notElem y c) || (notElem x c && elem y c)]

--EJERCICIO 2
nodup :: (Eq a) => [a] -> [a]
nodup [] = []
nodup [a] = [a]
nodup a = foldr (\a b -> a : (filter (/= a) b)) [] a


--EJERCICIO 3
--3.1
data Arbre a = Node a (Arbre a) (Arbre a) | Abuit deriving (Show)

--3.2
ttake :: Int -> Arbre a -> Arbre a
ttake n Abuit = Abuit
ttake n (Node a b c)
    | n > 0 = Node a (ttake (n-1) b) (ttake (n-1) c)
    | otherwise = Abuit

--3.3
inftreeAux :: (Num a) => a -> Arbre a
inftreeAux n = Node n (inftreeAux (n+1)) (inftreeAux (n+1))

inftree ::  (Num a) => Arbre a
inftree = inftreeAux 1


--EJERCICIO 4
data ErrorList a = Elist [a] Int

getErrors [] [] = 0
getErrors x [] = (length x) *2
getErrors [] y = (length y) *2
getErrors (x:xs) (y:ys)
    | x /= y = 2 + getErrors xs ys
    | otherwise = getErrors xs ys

instance (Eq a) => Eq (ErrorList a) where
    (Elist a ne) == (Elist b me) = (getErrors a b) <= (ne + me)
