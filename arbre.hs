data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)


{-
:l arbre.hs
let t7 = Node 7 Empty Empty
let t6 = Node 6 Empty Empty
let t5 = Node 5 Empty Empty
let t4 = Node 4 Empty Empty
let t3 = Node 3 t6 t7
let t2 = Node 2 t4 t5
let t1 = Node 1 t2 t3
let t1' = Node 1 t3 t2



let t8 = Node 8 Empty Empty; t9 = Node 9 Empty Empty; t10 = Node 10 Empty Empty; t11 = Node 11 Empty Empty; t6 = Node 6 Empty Empty; t7 = Node 7 Empty Empty
let t4 = Node 4 t8 t9; t5 = Node 5 t10 t11
let t2 = Node 2 t4 t5
let t3 = Node 3 t6 t7
let t1 = Node 1 t2 t3



let t2 = Node 2 Empty Empty; t3 = Node 3 Empty Empty; t1 = Node 1 t2 t3
let t7 = Node 7 Empty Empty; t6 = Node 6 Empty Empty; t5 = Node 5 t6 t7

-}


size :: Tree a -> Int
size Empty = 0
size (Node a b c) = 1 + (size b + size c)

height :: Tree a -> Int
height Empty = 0
height (Node a b c) = q + 1
  where q = max (height b) (height c)

equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal Empty _ = False
equal _ Empty = False
equal (Node a s d) (Node z x c) = (a == z) && q && w
  where (q,w) = (equal s x, equal d c)

isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic Empty Empty = True
isomorphic Empty _ = False
isomorphic _ Empty = False
isomorphic (Node a s d) (Node z x c) = (isomorphic s x) && (isomorphic d c)


preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node a b c) = [a]++q++w
  where (q,w) = (preOrder b,preOrder c)

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node a b c) = q++w++[a]
  where (q,w) = (postOrder b,postOrder c)


inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node a b c) = q++[a]++w
  where (q,w) = (inOrder b,inOrder c)

getNodeArbre Empty = []
getNodeArbre (Node a _ _) = [a]

--breadthFirstAux Empty = []
--breadthFirstAux x@(Node a s d) = q++w++breadthFirstAux s++breadthFirstAux d
--    where (q,w) = (getNodeArbre s, getNodeArbre d)


--breadthFirst :: Tree a -> [a]
--breadthFirst Empty = []
--breadthFirst x@(Node a s d) = a:breadthFirstAux x


breadthAux :: [Tree a]->[a]
breadthAux [Empty] = []
breadthAux (Empty:xs) = breadthAux xs
breadthAux (x@(Node a s d):xs) = a:breadthAux (xs++[s,d])

breadthFirst :: Tree a -> [a]
breadthFirst x = breadthAux [x]




getLeft :: Eq a => [a]->a->[a]
getLeft (x:xs) pivot
            | x == pivot = [x]
            | otherwise = x:(getLeft xs pivot)

getRight :: Eq a => [a]->a->[a]
getRight (x:xs) pivot
            | x == pivot = xs
            | otherwise = getRight xs pivot

build :: Eq a => [a] -> [a] -> Tree a
build _ [a] = Node a Empty Empty
build (x:xs) y = Node x (build (getLeft xs p) q) (build (getRight xs p) (tail(w)))
  where (q,w) = span (/= x) y
        p = last q


overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
overlap f x Empty = x
overlap f Empty x = x
overlap f (Node a Empty Empty) (Node b c d) = Node (f a b) c d
overlap f (Node b c d) (Node a Empty Empty) = Node (f b a) c d
overlap f (Node a s d) (Node q w e) = Node (f a q) (overlap f s w) (overlap f d e)
