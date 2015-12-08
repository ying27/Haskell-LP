

checkDiagonal :: Int -> (Double,Double) -> (Double,Double) -> Bool
checkDiagonal evenWidth1 (x,y) (q,w)
    | (not $ isInt y) && y == w && (abs(x-q)) == 1 && (max x q) == rel = True
    | otherwise = False
    where rel = y - 0.5

checkQUp :: Int -> (Double,Double) -> (Double,Double) -> Bool
checkQUp evenWidth1 (x,y) (q,w)
    | x < q && not (isInt x) = y < (rel - x)
    | x > q && not (isInt q) = w < (rel - q)
    | otherwise = False
    where rel = fromIntegral(evenWidth1) - 1

checkQDown :: Int -> (Double,Double) -> (Double,Double) -> Bool
checkQDown evenWidth1 (x,y) (q,w)
    | x < q && isInt x = x > (rel-y)
    | x > q && isInt q = q > (rel-w)
    | otherwise = False
    where rel = fromIntegral(evenWidth1) - 0.5

checkAdjacent :: Int -> (Double,Double) -> (Double,Double) -> Bool
checkAdjacent ew1 (x,y) (q,w)
    | (x < q && y < w ||  x > q && y > w) && abs(x-q) == 0.5 && abs(y-w) == 0.5 = (checkQUp ew1 (x,y) (q,w) || checkQDown ew1 (x,y) (q,w))
    | otherwise = False
