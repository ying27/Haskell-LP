countIf :: (Int -> Bool) -> [Int] -> Int
countIf f [] = 0
countIf f (x:y)
  | f x = 1 + countIf f y
  | otherwise = countIf f y

pam :: [Int] -> [Int -> Int] -> [[Int]]
pam _ [] = []
pam a (q:w) = map q a:pam a w

pam2 :: [Int] -> [Int -> Int] -> [[Int]]
pam2 [] _ = []
pam2 (a:b) q = [f a | f <- q]:pam2 b q


filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterFoldl f q a e = foldl q a (filter f e)

insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int]
insert f [] c = [c]
insert f (a:b) c
  | f a c = a : insert f b c
  | otherwise = [c,a] ++ b

insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int]
insertionSort f [] = []
insertionSort f (a:b) =  insert f q a
  where q = insertionSort f b
