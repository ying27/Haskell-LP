--PROBLEMA 1
esDob:: Int -> Int -> Bool
esDob = (\a b -> b == (a*2))

--PROBLEMA 2

elimDivAux :: [Int] -> Int -> [Int]
elimDivAux l n
    | foldr (\a b -> ((mod n a) == 0) || b) False l = l
    | otherwise = l++[n]

elimDiv :: [Int] -> [Int]
elimDiv [] = []
elimDiv (x:xs) = foldl (\a b -> elimDivAux a b) [x] xs


--PROBLEMA 3
infGen :: (Int -> Int) -> [Int]
infGen f = iterate f 1

fn :: Int -> (Int -> Int) -> Int
fn a f = (infGen f) !! a


--PROBLEMA 4
--Testing purpose
gr1 = G [1..7] fgr1
    where fgr1 1 = [2,5]
          fgr1 2 = [3]
          fgr1 3 = [4,6]
          fgr1 4 = []
          fgr1 5 = [6,7]
          fgr1 6 = [7]
          fgr1 7 = []
          fgr1 _ = []

gr2 = G [1..7] fgr1
    where fgr1 1 = [2,5]
          fgr1 2 = [3]
          fgr1 3 = [4,6]
          fgr1 4 = []
          fgr1 5 = [6,7]
          fgr1 6 = []
          fgr1 7 = []
          fgr1 _ = []

pgr = PG [1..5] fgr1
  where fgr1 1 = [(2,1),(5,1)]
        fgr1 2 = [(3,1)]
        fgr1 3 = [(4,1),(6,1)]
        fgr1 4 = []
        fgr1 5 = [(6,1),(7,1)]
        fgr1 _ = []

--4.a
data Graf a = G [a] (a -> [a])

geq :: (Eq a) => Graf a -> Graf a -> Bool
geq (G act fact) (G l g) = foldr (\a b -> ((fact a) == (g a) && elem a l) && b) True act

instance (Eq a) => Eq (Graf a) where
    a == b = geq a b && geq b a

--4.b
isReach :: Eq a => Graf a -> a -> a -> Bool
isReach g@(G l f) st en
    | st == en = True
    | otherwise = foldr (\a b -> (isReach g a en) || b ) False (f st)

--4.c
data PGraf a = PG [a] (a -> [(a,Int)])

isPReach :: (Eq a) => PGraf a -> a -> a -> (Bool,Int)
isPReach g@(PG l f) st en
    | st == en = (True,0)
    | list == [] = (False,-1)
    | fret == [] = (False,-1)
    | otherwise = head fret
    where list = f st
          res = map (\x -> isPReach g x en) [x | (x,y) <- list]
          ret = zipWith (\(a,b) (c,d) -> (c,d+b)) list res
          fret = filter (\(a,b) -> a) ret
