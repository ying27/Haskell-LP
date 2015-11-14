insert :: [Int] -> Int -> [Int]

insert [] n = [n]
insert (x:y) n 
	| n < x = [n]++(x:y)
	| otherwise = [x]++insert y n


isort :: [Int] -> [Int]
isort [] = []
isort [z] = [z]
isort (x:y) =
	insert (isort y) x

remove::[Int] -> Int -> [Int]
remove (x:y) n
	| x == n = y
	| otherwise = [x]++remove y n


ssort :: [Int] -> [Int]
ssort [] = []
ssort [x] = [x]
ssort (x:y) 
	| q < x = [q]++ssort ([x]++(remove y q))
	| otherwise = [x]++ssort y
	where q = minimum y


merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge [] (x:y) = (x:y)
merge (x:y) [] = (x:y)
merge (x:y) (a:b) 
	| x < a = [x]++merge y (a:b)
	| otherwise = [a]++merge (x:y) b


msort :: [Int] -> [Int] 
msort [] = []
msort [x] = [x]
msort (x:y) =
	merge (msort f) (msort s)
	where { pi = div (length (x:y)) 2;
		 (f,s) = splitAt pi (x:y)}


qsort :: [Int] -> [Int]
qsort [] = []
qsort [x] = [x]
qsort (x:y) =
	qsort f ++ [x] ++ qsort s
	where {f = [elem | elem <- y, elem < x];
		   s = [elem | elem <- y, elem >= x]}

genQsort :: Ord a => [a] -> [a] 
genQsort [] = []
genQsort [x] = [x]
genQsort (x:y) =
	genQsort f ++ [x] ++ genQsort s
	where { f = [elem | elem <- y, elem < x];
			s = [elem | elem <- y, elem >= x]}