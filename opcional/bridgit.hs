import System.Environment
import Data.List.Split
import System.Random

x :: Bridgit
x = Board [[0,0,0],[1,0],[0,0,0],[1,0],[0,0,0]]

y :: Bridgit
y = Board [[0,0,0],[2,2],[2,1,2],[0,0],[0,0,0]]


----Define the data board and it's show definition----------------------------------------------------------
data Bridgit = Board [[Int]]

instance Show Bridgit where
    show (Board a) = showBlueRow a

blue :: String -> String
blue a = "\x1b[34m" ++ a ++ "\x1b[0m"

red :: String -> String
red a = "\x1b[31m" ++ a ++ "\x1b[0m"

yellow :: String -> String
yellow a = "\x1b[33m" ++ a ++ "\x1b[0m"

showBlueRow :: [[Int]] -> String
showBlueRow [] = ""
showBlueRow (x:xs) = (showDots 1 x) ++ (showRedRow xs)

showRedRow :: [[Int]] -> String
showRedRow [] = ""
showRedRow (x:xs) = (showDots 2 x) ++ (showBlueRow xs)

showDots :: Int -> [Int] -> String
showDots 1 [0] = blue "   \n"
showDots 1 [1] = blue "---\n"
showDots 1 [2] = red " | \n"
showDots 2 [0] = red " +     + \n"
showDots 2 [1] = (red " + ") ++ (blue " | ") ++ (red " + \n")
showDots 2 [2] = red " + --- + \n"
showDots 1 ((0):xs) = "   " ++ (blue " + ") ++ (showDots 1 xs)
showDots 1 ((1):xs) = (blue "--- + ") ++ (showDots 1 xs)
showDots 1 ((2):xs) = (red " | ") ++ (blue " + ") ++ (showDots 1 xs)
showDots 2 ((0):xs) = (red " + ") ++ "   " ++ (showDots 2 xs)
showDots 2 ((1):xs) = (red " + ") ++ (blue " | ") ++ (showDots 2 xs)
showDots 2 ((2):xs) = (red " + ---") ++ (showDots 2 xs)

--------------------------------------------------------------------------------------------------------





----IA 1------------------------------------------------------------------------------------------------
--           player   f      c   colum list
exploreCol :: Int -> Double -> Double -> [Int] -> ([(Double,Double)],[(Double,Double)])
exploreCol player _ _ [] = ([],[])
exploreCol player f c l@(x:xs)
    | x == 0 = ((f,c):emp,ocu)
    | x  == player = (emp,(f,c):ocu)
    | otherwise = (emp,ocu)
    where (emp,ocu) = exploreCol player f (c+1) xs

exploreRow :: Int -> Int -> [[Int]] -> ([(Double,Double)],[(Double,Double)])
exploreRow _ _ [] = ([],[])
exploreRow player f (x:xs)
    | even f = (e++r,c++t)
    | otherwise = (ee++r,cc++t)
    where (e,c) = (exploreCol player (fromIntegral f) 0 x)
          (ee,cc) = (exploreCol player (fromIntegral f) 0.5 x)
          (r,t) = exploreRow player (f+1) xs

dist :: (Double,Double) -> (Double,Double) -> Double
dist (x,y) (q,w) = ret*ret*ret
    where ret = sqrt ((x-q)*(x-q) + (y-w)*(y-w))
getMinDist :: [(Double,Double)] -> (Double,Double) -> [Double]
getMinDist x po@(i,j) = [(foldr (\a b -> dist po a + b) 0 x),i,j]

cpu0 :: Bridgit -> Int -> [Int]
cpu0 (Board a) player = do
    let (empty,filled) = exploreRow player 0 a
    let (x:xs) = map (getMinDist filled) empty
    let [i,j,k] = foldr (\a b -> min a b) x xs
    [truncate j, truncate k]

--------------------------------------------------------------------------------------------------------

---Random-----------------------------------------------------------------------------------------------
randRow :: Bridgit -> IO Int
randRow a =  randomRIO(0, getHeight a)

randOddCol :: Bridgit -> IO Int
randOddCol a =  randomRIO(0, getOddWidth a)

randEvenCol :: Bridgit -> IO Int
randEvenCol a =  randomRIO(0, getEvenWidth a)

rand :: Bridgit -> Int
rand a = do
    f <- randRow a :: IO Int
    let ret = (id f)
    ret


{-
    do f <-
    (id f)
    --  [(id f)]

   if k then
        [0,0]
        --g <- randomRIO (0, getEvenWidth a)
    else
        --g <- randomRIO (0, getOddWidth a)
        [0,0]
    --if testBoard [f,g] a then
--        [f,g]
--        else random a
-}





--------------------------------------------------------------------------------------------------------



----To create the board and it's attributes-------------------------------------------------------------
getPath :: Int -> [Int]
getPath col = col : (col-1) : getPath col

create :: Int -> Int -> Bridgit
create f c = Board $ map (\x -> take x  [0,0..0]) path
    where path = take (f+c) $ getPath f

getHeight :: Bridgit -> Int
getHeight (Board a) = length a

getEvenWidth :: Bridgit -> Int
getEvenWidth (Board a) = length (head a)

getOddWidth :: Bridgit -> Int
getOddWidth (Board (x:xs)) = length (head xs)
---------------------------------------------------------------------------------------------------------





----Set the movements-------------------------------------------------------------------------------------
setCol :: Int -> Int -> [Int] -> [Int]
setCol player col submap = a ++ [player] ++ (tail b)
  where (a,b) = splitAt col submap

setMovement :: Int -> [Int] -> Bridgit -> Bridgit
setMovement player [fil,col] (Board mapa) = Board (a ++ [setCol player col (head b)] ++ (tail b))
  where (a,b) = splitAt fil mapa

-----------------------------------------------------------------------------------------------------------




----Logic of the board--------------------------------------------------------------------------------------
getNextS :: Int -> String -> [String] -> [Int]
getNextS 1 dir [f,c] = getNextB dir [(read f :: Int),(read c :: Int)]
getNextS 2 dir [f,c] = getNextR dir [(read f :: Int),(read c :: Int)]

getNext :: Int -> String -> [Int] -> [Int]
getNext 1 dir [f,c] = getNextB dir [f,c]
getNext 2 dir [f,c] = getNextR dir [f,c]

getNextB :: String -> [Int] -> [Int]
getNextB "u" [f,c] = [(f-1)*2-1,c-1]
getNextB "d" [f,c] = [f*2-1,c-1]
getNextB "l" [f,c] = [((f-1)*2),c-1]
getNextB "r" [f,c] = [((f-1)*2),c]

getNextR :: String -> [Int] -> [Int]
getNextR "u" [f,c] = [(f-1)*2,c-1]
getNextR "d" [f,c] = [f*2,c-1]
getNextR "l" [f,c] = [(f*2)-1,c-2]
getNextR "r" [f,c] = [(f*2)-1,c-1]

testBoard :: [Int] -> Bridgit -> Int
testBoard [fil,col] (Board mapa)
    | b == [] = -1
    | d == [] = -1
    | otherwise = head d
    where (a,b) = splitAt fil mapa
          (c,d) = splitAt col $ head b

-----------------------------------------------------------------------------------------------------------

-----Blue Player game over check---------------------------------------------------------------------------
checkBOddRow :: [Int] -> Bridgit -> Bool
checkBOddRow m@[f,c] b@(Board mapa)
  | (getHeight b) <= (f+1) = True
  | (testBoard m b) == 1 = (checkBEvenRow [f+1,c] b) || (checkBEvenRow [f+1,c+1] b) || (checkBOddRow [f+2,c] b)
  | otherwise = False

checkBEvenRow :: [Int] -> Bridgit -> Bool
checkBEvenRow m@[f,c] b@(Board mapa)
    | (getHeight b) <= (f+1) = True
    | (testBoard m b) == 1 = (checkBOddRow [f+1,c-1] b) || (checkBOddRow [f+1,c] b) || (checkBEvenRow [f+2,c] b)
    | otherwise = False
-----------------------------------------------------------------------------------------------------------


-----Red Player game over check----------------------------------------------------------------------------
checkROddRow :: [Int] -> Bridgit -> Bool
checkROddRow m@[f,c] b@(Board mapa)
    | (getOddWidth b) <= (c) = True
    | (testBoard m b) == 2 = (checkREvenRow [f-1,c+1] b) || (checkROddRow [f,c+1] b) || (checkREvenRow [f+1,c+1] b)
    | otherwise = False

checkREvenRow :: [Int] -> Bridgit -> Bool
checkREvenRow m@[f,c] b@(Board mapa)
    | (getOddWidth b) <= (c) = True
    | (testBoard m b) == 2 = (checkROddRow [f-1,c] b) || (checkREvenRow [f,c+1] b) || (checkROddRow [f+1,c] b)
    | otherwise = False
-----------------------------------------------------------------------------------------------------------


readPlayerMove :: Bridgit -> Int -> IO [Int]
readPlayerMove x player = do
    putStrLn "From:"
    move <- getLine--
    putStrLn "Direction (u,d,l,r):"
    dir <- getLine
    --putStrLn (show $ getNextB dir (splitOn " " move))
    return (getNextS player dir (splitOn " " move))


playBlue :: Bridgit -> Int -> IO()
playBlue x mode = do
  putStrLn (blue "***Blue player turn***")
  nxtmove <- readPlayerMove x 1
  if (testBoard nxtmove x) == 0 then do
        let newx = setMovement 1 nxtmove x
        putStrLn (show newx)
        let go = foldr (\a b-> (checkBOddRow [1,a] newx) || b) False (take (getOddWidth x) [0,1..])
        if go == False then
            playRed newx mode
        else putStrLn (yellow "PLAYER BLUE WINS!!")
  else do
      putStrLn (yellow "ILLEGAL MOVE!!")
      playBlue x mode



playRed :: Bridgit -> Int -> IO()
playRed x mode = do
  putStrLn (red "***Red player turn***")
  --nxtmove <- readPlayerMove x 2
  let nxtmove = cpu0 x 1

  if (testBoard nxtmove x) == 0 then do
    let newx = setMovement 2 nxtmove x
    putStrLn (show newx)
    let go = foldr (\a b-> (checkROddRow [a,0] newx) || b) False (take (getOddWidth x) [1,3..])
    if go == False then
        playBlue newx mode
    else putStrLn (yellow "PLAYER RED WINS!!")
  else do
    putStrLn (yellow "ILLEGAL MOVE!!")
    playRed x mode

------------------------------------------------------------------------------------------------------------









main = do
  putStrLn "Welcome to Bridg-it! First of all define the board size:"
  putStrLn "Rows: "
  rows <- getLine
  putStrLn "Columns: "
  columns <- getLine

  putStrLn "Each movement is defined by the row followed by a space and the column. Rows and columns starts at 1."
  putStrLn "Starting game..."

  let [x,y] = [(read rows :: Int),(read columns :: Int)]
  let board = create (max x y) (min x y)

  playBlue board 0



  --putStrLn (show blue)
  --putStrLn (show red)
  putStrLn "GAME OVER"
