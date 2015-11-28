
--PROBLEMA 1
mconcat :: [[a]]->[a]
mconcat a = [ x | [x] <- (foldr (\a b -> (map (\x -> [x]) a) ++ b) [] a)]

concat3 :: [[[a]]]->[a]
concat3 a = foldr (\a b -> (mconcat a) ++ b) [] a


--PROBLEMA 2
fold2r :: (a -> b -> c -> c) -> c -> [a] -> [b] -> c
fold2r f ac [] _ = ac
fold2r f ac _ [] = ac
fold2r f ac (x:xs) (y:ys) = f x y (fold2r f ac xs ys)


--POBLEMA 3
mix :: [a] -> [a] -> [a]
mix x [] = x
mix [] y = y
mix (x:xs) (y:ys) = [x,y]++(mix xs ys)

lmix :: [Int] -> [a] -> [a]
lmix [] c = c
lmix (a:b) c = lmix b (mix cap cua)
  where (cap,cua) = splitAt a c


--PROBLEMA 4
paux l  = h ++ (zipWith (+) t (paux l))
  where (h,t) = splitAt 1 l

dPascal :: Int -> [Integer]
dPascal n = foldl (\a b -> paux a) (iterate id (1)) [1..n]


--PROBLEMA 5
data BTree a = Node a (BTree a) (BTree a) | Empty deriving (Show)

foldrAux :: [a] -> [BTree a] -> [BTree a]
foldrAux [] _ = []
foldrAux [x] [] = [Node x Empty Empty]
foldrAux [x] [y] = [Node x y Empty]
foldrAux (x:xs) [] = (Node x Empty Empty) : foldrAux xs []
foldrAux (x:xs) (y:yy:ys) = (Node x y yy) : foldrAux xs ys

buildTreeF :: [[a]] -> BTree a
buildTreeF x = ret
  where [ret] = foldr (\a b -> foldrAux a b) [] x


--PROBLEMA 6
ex1 :: Expr Int
ex1 = Unary (Binary (List [Val 3, Unary (Val 2)]) (Val 8))

--6.1
class Lit a where
  unary :: a -> a
  binary :: a -> a -> a
  list :: [a] -> a

--6.2
data Expr a = Val a | Unary (Expr a) | Binary (Expr a) (Expr a) | List [Expr a] deriving (Show)

--6.3
eval :: Lit a => Expr a -> a
eval (Unary a) = unary (eval a)
eval (Binary a b) = binary (eval a) (eval b)
eval (List a) = list (map (\x -> eval x) a)
eval (Val a) = a

--6.4
instance Lit Int where
  unary a = -a
  binary a b = a + b
  list a = sum a
