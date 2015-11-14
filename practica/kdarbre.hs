class Point p where
  sel :: Integer -> p -> Double
  dim :: p -> Integer
  child :: p -> p -> [Integer] -> Integer
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
  child q@(Point3d (a,b,c)) w@(Point3d (d,e,f)) (x:xs)
    | (sel x q) > (sel x w) = 2^(x-1)+child q w xs
    | otherwise  = child q w xs

  dist (Point3d (a,b,c)) (Point3d (d,e,f)) = sqrt ((d-a)^2 + (e-b)^2 + (f-c)^2)

  list2Point [a,b,c] = Point3d (a,b,c)

instance Show Point3d where
  show (Point3d (a,b,c)) = "("++(show a)++","++(show b)++","++(show c)++")"

-------------------------------------------------------------------------------------------

data Kd2nTree p = Node p [Integer] [Kd2nTree p] | Empty

instance Eq a => Eq (Kd2nTree a) where
  Empty == Empty = True
  Empty == _     = False
  _     == Empty = False
  (Node q w e) == (Node a s d) = q==a && w==s && e==d --TODO: Check if the comparision between list of custom data works


showFills :: (Show p) => String -> Integer -> [Kd2nTree p] -> String
showFills _ pos [] = ""
showFills p pos (Empty:xs) = showFills p (pos+1) xs
showFills "*" pos (x:xs) = showNode "*" pos x ++ showFills "*" (pos+1) xs
showFills p pos (x:xs) = (showNode (p++"     ") pos x) ++ showFills p (pos+1) xs
--showFills pos (x:xs) = "<" ++ (show pos) ++ ">" ++ " " ++ "\n" ++ showFills (pos+1) xs

showNode :: (Show p) => String -> Integer -> Kd2nTree p -> String
showNode _ _ Empty = show ""
showNode "*" pos (Node a w list) = " <" ++ (show pos) ++ ">" ++ " " ++ show a ++ " " ++ show w ++ "\n" ++ showFills "" 0 list
showNode p pos (Node a w list) = p ++ " <" ++ (show pos) ++ ">" ++ " " ++ show a ++ " " ++ show w ++ "\n" ++ showFills p 0 list

instance (Show p) => Show (Kd2nTree p) where
  show Empty = show ""
  show (Node a w list) = show a ++ " " ++ show w ++ "\n" ++ showFills "*" 0 list


-------------------------------------------------------------------------------------------



{-


let a = Node (Point3d (1.8,1.1,-2.0)) [1,2] [Empty]
let b = Node (Point3d (1.5,8.0,1.5)) [1] [Empty]
let c = Node (Point3d (3.3,2.8,2.5)) [3] [Empty]
let d = Node (Point3d (3.1,3.8,4.8)) [1,3] [Empty]
let e = Node (Point3d (4.0,5.1,3.8)) [2] [Empty]
let f = Node (Point3d (3.5,2.8,3.1)) [1,2] [c,d, Empty, e]
let g = Node (Point3d (3.5,0.0,2.1)) [3] [Empty]
let h = Node (Point3d (3.0,-1.7,3.1)) [1,2,3] [Empty]
let i = Node (Point3d (3.0,5.1,0.0)) [2] [a,b]
let j = Node (Point3d (3.0,-1.0,2.1)) [1,3] [i,h,g,f]


-}
