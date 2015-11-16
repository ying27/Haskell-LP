exampleSet :: Kd2nTree Point3d
exampleSet =
  let a = Node (Point3d (1.8,1.1,-2.0)) [1,2] [Empty]
      b = Node (Point3d (1.5,8.0,1.5)) [1] [Empty]
      c = Node (Point3d (3.3,2.8,2.5)) [3] [Empty]
      d = Node (Point3d (3.1,3.8,4.8)) [1,3] [Empty]
      e = Node (Point3d (4.0,5.1,3.8)) [2] [Empty]
      f = Node (Point3d (3.5,2.8,3.1)) [1,2] [c,d, Empty, e]
      g = Node (Point3d (3.5,0.0,2.1)) [3] [Empty]
      h = Node (Point3d (3.0,-1.7,3.1)) [1,2,3] [Empty]
      i = Node (Point3d (3.0,5.1,0.0)) [2] [a,b]
    in Node (Point3d (3.0,-1.0,2.1)) [1,3] [i,h,g,f]

exampleSet1 :: Kd2nTree Point3d
exampleSet1 =
  let f = Node (Point3d (3.5,2.8,3.1)) [1,2] [Empty,Empty, Empty, Empty]
      g = Node (Point3d (3.5,0.0,2.1)) [3] [Empty, Empty]
      h = Node (Point3d (3.0,-1.7,3.1)) [1,2,3] [Empty,Empty, Empty, Empty,Empty,Empty, Empty, Empty]
      i = Node (Point3d (3.0,5.1,0.0)) [2] [Empty,Empty]
    in Node (Point3d (3.0,-1.0,2.1)) [1,3] [i,h,g,f]

testbuild :: [(Point3d,[Int])]
testbuild =
  let a = [(Point3d (3.0,-1.0,2.1),[1,3]), (Point3d (3.5,2.8,3.1),[1,2]), (Point3d (3.5,0.0,2.1),[3])]
      b = [(Point3d (3.0,-1.7,3.1),[1,2,3]), (Point3d (3.0,5.1,0.0),[2]), (Point3d (1.5,8.0,1.5),[1])]
      c = [(Point3d (3.3,2.8,2.5),[3]), (Point3d (4.0,5.1,3.8),[2]), (Point3d (3.1,3.8,4.8),[1,3]), (Point3d (1.8,1.1,-2.0),[1,2])]
    in a++b++c

testbuild1 :: [(Point3d,[Int])]
testbuild1 = [(Point3d (3.0,-1.0,2.1),[1,3]), (Point3d (3.5,2.8,3.1),[1,2]), (Point3d (3.5,0.0,2.1),[3])]

testbuildini :: [([Double],[Int])]
testbuildini =
  let a = [([3.0,-1.0,2.1],[1,3]), ([3.5,2.8,3.1],[1,2]), ([3.5,0.0,2.1],[3])]
      b = [([3.0,-1.7,3.1],[1,2,3]), ([3.0,5.1,0.0],[2]), ([1.5,8.0,1.5],[1])]
      c = [([3.3,2.8,2.5],[3]), ([4.0,5.1,3.8],[2]), ([3.1,3.8,4.8],[1,3]), ([1.8,1.1,-2.0],[1,2])]
    in a++b++c

testbuildini1 :: [([Double],[Int])]
testbuildini1 = [([3.0,-1.0,2.1],[1,3]), ([3.5,2.8,3.1],[1,2]), ([3.5,0.0,2.1],[3])]

-------------------------------------------------------------------------------------------

class Point p where
  sel :: Int -> p -> Double
  dim :: p -> Int
  child :: p -> p -> [Int] -> Int
  dist :: p -> p -> Double
  list2Point :: [Double] -> p

-------------------------------------------------------------------------------------------

data Point3d  = Point3d (Double,Double,Double) deriving (Eq)

instance Point Point3d where
  sel 1 (Point3d (a,_,_))  = a
  sel 2 (Point3d (_,b,_))  = b
  sel 3 (Point3d (_,_,c))  = c

  dim k = 3

  child _ _ [] = 0;
  child q@(Point3d (a,b,c)) w@(Point3d (d,e,f)) l@(x:xs)
    | (sel x q) > (sel x w) = 2^((length l)-1) + child q w xs
    | otherwise  = child q w xs

  dist (Point3d (a,b,c)) (Point3d (d,e,f)) = sqrt ((d-a)^2 + (e-b)^2 + (f-c)^2)

  list2Point [a,b,c] = Point3d (a,b,c)

instance Show Point3d where
  show (Point3d (a,b,c)) = "("++(show a)++","++(show b)++","++(show c)++")"

-------------------------------------------------------------------------------------------

data Kd2nTree p = Node p [Int] [Kd2nTree p] | Empty

instance Eq a => Eq (Kd2nTree a) where
  Empty == Empty = True
  Empty == _     = False
  _     == Empty = False
  (Node q w e) == (Node a s d) = q==a && w==s && e==d --TODO: Check if the comparision between list of custom data works


showFills :: (Show p) => String -> Int -> [Kd2nTree p] -> String
showFills _ pos [] = ""
showFills p pos (Empty:xs) = showFills p (pos+1) xs
showFills "*" pos (x:xs) = showNode "*" pos x ++ showFills "*" (pos+1) xs
showFills p pos (x:xs) = (showNode (p++"     ") pos x) ++ showFills p (pos+1) xs


showNode :: (Show p) => String -> Int -> Kd2nTree p -> String
showNode _ _ Empty = show ""
showNode "*" pos (Node a w list) = "\n" ++ " <" ++ (show pos) ++ ">" ++ " " ++ show a ++ " " ++ show w ++ showFills "" 0 list
showNode p pos (Node a w list) = "\n" ++ p ++ " <" ++ (show pos) ++ ">" ++ " " ++ show a ++ " " ++ show w ++ showFills p 0 list


instance (Show p) => Show (Kd2nTree p) where
  show Empty = show ""
  show (Node a w list) = show a ++ " " ++ show w ++ showFills "*" 0 list


-------------------------------------------------------------------------------------------

--insertIni :: (Point punt) => Kd2nTree punt -> ([Double],[Int]) -> Kd2nTree punt
--insertIni a (p,comp) = insert a (list2Point p) comp

insert :: (Point punt) => Kd2nTree punt -> punt -> [Int] -> Kd2nTree punt
insert Empty p comp = Node p comp (take (2^(length comp)) (iterate id Empty))
--insert (Node a w list ) p comp = insert (list !! (child p a w)) p comp
insert (Node a w list) p comp = Node a w (l++[nod]++(tail r))
  where pos = (child p a w)
        (l,r) = splitAt pos list
        nod = insert (head r) p comp

build :: (Point punt) => [(punt,[Int])] -> Kd2nTree punt
build list =  foldl (\a (p,comp) -> insert a p comp) Empty list

buildIni :: (Point punt) => [([Double],[Int])] -> Kd2nTree punt
buildIni list = foldl (\a (p,comp) -> insert a (list2Point p) comp) Empty list

-------------------------------------------------------------------------------------------

getDoubles :: (Point p) => p -> Int -> [Double]
getDoubles p 1 = [sel 1 p]
getDoubles p count = (getDoubles p (count-1))++[(sel count p)]

get_all :: (Point p) => Kd2nTree p -> [([Double],[Int])]
get_all Empty = []
get_all (Node a comp xs) = (getDoubles a (dim a),comp) : foldr (\q b->(get_all q) ++ b) [] xs

-------------------------------------------------------------------------------------------

remove :: (Eq p, Point p) => Kd2nTree p -> p -> Kd2nTree p
remove (Node a w list) p
  | a /= p = Node a w (l++[nod]++(tail r))
  | otherwise = buildIni (foldr (\q b->(get_all q) ++ b) [] list)
  where pos = (child p a w)
        (l,r) = splitAt pos list
        nod = remove (head r) p

-------------------------------------------------------------------------------------------

contains :: (Eq p, Point p) => Kd2nTree p -> p -> Bool
contains Empty _ = False
contains (Node a w list) p
  | a /= p = contains (list !! (child p a w)) p
  | otherwise = True
