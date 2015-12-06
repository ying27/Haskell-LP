import System.Environment
import Data.List.Split
import System.Random

x :: Bridgit
x = Board [[0,0,0],[1,0],[0,0,0],[1,0],[0,0,0]]

y :: Bridgit
y = Board [[0,0,0],[2,2],[2,1,2],[0,0],[0,0,0]]

z :: Bridgit
z = Board [[0,0,0,0],[1,2,0],[0,0,0,0],[0,0,0],[0,2,0,0],[1,0,1],[0,0,0,0]]

a :: [[Int]]
a = [[-1,-1,-1,-1],[1,2,2],[-2,1,2,-2],[2,1,1],[-2,1,1,-2],[1,2,2],[-1,-1,-1,-1]]

c = [[-1,-1,-1,-1],[1,2,2],[-2,2,2,-2],[1,2,1],[-2,2,1,-2],[1,1,1],[-1,-1,-1,-1]]


b :: Bridgit
b = Board a

cc = Board c


testbgo x = testBOddRow [1,1] x

testBOddRow :: [Int] -> Bridgit -> IO()
testBOddRow m@[f,c] b@(Board mapa)
  | (getHeight b) <= f = putStrLn $ show m
  | (testBoard m b) == 1 = do putStrLn $ show m
                              (testBEvenRow [f+1,c] b)
                              (testBEvenRow [f+1,c+1] b)
                              (testBOddRow [f+2,c] b)
  | otherwise = putStrLn  $ " *"++show m++"* "

testBEvenRow :: [Int] -> Bridgit -> IO()
testBEvenRow m@[f,c] b@(Board mapa)
    | (getHeight b) <= f = putStrLn $ ">"++(show m)++"<"
    | (testBoard m b) == 1 = do putStrLn $ show m
                                (testBOddRow [f,c-1] b)
                                (testBOddRow [f,c+1] b)
                                (testBOddRow [f+1,c-1] b)
                                (testBOddRow [f+1,c] b)
    | otherwise = putStrLn  $ " *"++show m++"* "




{-
testNeigh :: String -> (Double,Double) -> [(Double,Double)] -> Bool
testNeigh "u" (x,y) list = elem (x-0.5,y-0.5) list || elem (x-0.5,y+0.5) list || elem (x-1,y) list
testNeigh "d" (x,y) list = elem (x+0.5,y-0.5) list || elem (x+0.5,y+0.5) list || elem (x+1,y) list
testNeigh "l" (x,y) list = elem (x,y-1) list || elem (x-0.5,y-0.5) list || elem (x+0.5,y-0.5) list
testNeigh "r" (x,y) list = elem (x-0.5,y+0.5) list || elem (x,y+1) list || elem (x+0.5,y+0.5) list

testGOAux :: Bridgit -> Int -> [(Double,Double)] -> (Double,Double) -> Bool
testGOAux b 1 list p@(x,y)
    | x == 0.5 = testNeigh "d" p list
    | (x+0.5) == fromIntegral(getOddWidth b) = testNeigh "u" p list
    | isInt x = testNeigh "l" p list && testNeigh "r" p list
    | otherwise = testNeigh "u" p list && testNeigh "d" p list

testGOAux b 2 list p@(x,y)
    | y == 0.5 = testNeigh "r" p list
    | (y+0.5) == fromIntegral(getOddWidth b) = testNeigh "l" p list
    | isInt x = testNeigh "u" p list && testNeigh "d" p list
    | otherwise = testNeigh "l" p list && testNeigh "r" p list

-}

getcomp :: Int -> (Double,Double) -> Double
getcomp 1 (x,y) = x
getcomp 2 (x,y) = y

{-
testGO :: Int -> Bridgit -> Bool
testGO player b@(Board a)
    | start && end = foldr (\a b -> (any (\p -> (getcomp player p) == a) connex) && b) True (take (getOddWidth b) [0.5,1.5..])
    | otherwise = False
    where (empty,filled) = exploreRow player 0 a
          start = any (\p -> (getcomp player p) == 0.5) filled
          end = any (\p -> ((getcomp player p)+0.5) == fromIntegral(getOddWidth b)) filled
          connex = filter (\a -> testGOAux b player filled a) filled
-}


getNeigh :: String -> (Double,Double) -> [(Double,Double)]
getNeigh "u" (x,y) = [(x-0.5,y-0.5),(x-0.5,y+0.5),(x-1,y)]
getNeigh "d" (x,y) = [(x+0.5,y-0.5),(x+0.5,y+0.5),(x+1,y)]
getNeigh "l" (x,y) = [(x,y-1),(x-0.5,y-0.5),(x+0.5,y-0.5)]
getNeigh "r" (x,y) = [(x-0.5,y+0.5),(x,y+1),(x+0.5,y+0.5)]

getGOAux :: Int -> (Double,Double) -> [(Double,Double)]
getGOAux 1 p@(x,y)
    | x == 0.5 = getNeigh "d" p
    | isInt x = getNeigh "l" p ++ getNeigh "r" p
    | otherwise = getNeigh "u" p ++ getNeigh "d" p

getGOAux 2 p@(x,y)
    | y == 0.5 = getNeigh "r" p
    | isInt x = getNeigh "u" p ++ getNeigh "d" p
    | otherwise = getNeigh "l" p ++ getNeigh "r" p


extract :: (Eq a) => (a,a) -> [(a,a)] -> [(a,a)]
extract p (x:xs)
    | x == p = xs
    | otherwise = x : extract p xs

gorec :: Bridgit -> Double -> Int -> [(Double,Double)] -> (Double,Double) -> Bool
gorec b cua player [] p = False
gorec b cua player list p@(x,y)
    | elem p list && (getcomp player p) == cua = True
    | elem p list = foldr (\q w -> gorec b cua player (extract p list) q || w) False (getGOAux player p)
    | otherwise = False

testGO :: Int -> Bridgit -> Bool
testGO player b@(Board a) = any (gorec b cua player filled) heads
    where (empty,filled) = exploreRow player 0 a
          cua = fromIntegral (getOddWidth b) - 0.5
          heads = filter (\p -> (getcomp player p) == 0.5) filled










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
showDots 1 [-3] = blue "   \n"
showDots 1 [0] = blue "   \n"
showDots 1 [-1] = blue "   \n"
showDots 1 [-2] = red "   \n"
showDots 1 [2] = red " | \n"
showDots 2 [0] = red " +     + \n"
showDots 2 [1] = (red " + ") ++ (blue " | ") ++ (red " + \n")
showDots 2 [2] = red " + --- + \n"
showDots 1 ((-3):xs) = "   " ++ (blue " + ") ++ (showDots 1 xs)
showDots 1 ((-2):xs) = (red "   ") ++ (blue " + ") ++ (showDots 1 xs)
showDots 1 ((-1):xs) = "   " ++ (blue " + ") ++ (showDots 1 xs)
showDots 1 ((0):xs) = "   " ++ (blue " + ") ++ (showDots 1 xs)
showDots 1 ((1):xs) = (blue "--- + ") ++ (showDots 1 xs)
showDots 1 ((2):xs) = (red " | ") ++ (blue " + ") ++ (showDots 1 xs)
showDots 2 ((0):xs) = (red " + ") ++ "   " ++ (showDots 2 xs)
showDots 2 ((1):xs) = (red " + ") ++ (blue " | ") ++ (showDots 2 xs)
showDots 2 ((2):xs) = (red " + ---") ++ (showDots 2 xs)

--------------------------------------------------------------------------------------------------------





----IA 1------------------------------------------------------------------------------------------------
isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)
--           player    f         c    colum_list
exploreCol :: Int -> Double -> Double -> [Int] -> ([(Double,Double)],[(Double,Double)])
exploreCol player _ _ [] = ([],[])
exploreCol player f c l@(x:xs)
    | x == 0 = ((f,c):emp,ocu)
    | x == player = (emp,(f,c):ocu)
    | otherwise = (emp,ocu)
    where (emp,ocu) = exploreCol player f (c+1) xs

exploreRow :: Int -> Double -> [[Int]] -> ([(Double,Double)],[(Double,Double)])
exploreRow _ _ [] = ([],[])
exploreRow player f (x:xs)
    | isInt f = (e++r,c++t)
    | otherwise = (ee++r,cc++t)
    where (e,c) = (exploreCol player f 0 x)
          (ee,cc) = (exploreCol player f 0.5 x)
          (r,t) = exploreRow player (f+0.5) xs

distIA :: Int -> (Double,Double) -> (Double,Double) -> Double
distIA player (x,y) (q,w)
    | player == 2 && x == q && ret == 1 = ret
    | player == 1 && y == w && ret == 1 = ret
    | player == 2 && x == q && ret == 0.5 = 0.5
    | player == 1 && y == w && ret == 0.5 = 0.5
    | otherwise = 0
    where ret = (x-q)*(x-q) + (y-w)*(y-w)

getPDist :: Int -> [(Double,Double)] -> (Double,Double) -> [Double]
getPDist player x po@(i,j) = [(foldr (\a b -> distIA player po a + b) 0 x),i,j]

cpu0 :: Int -> Bridgit -> IO [Int]
cpu0 player (Board a) = do
    let (empty,filled) = exploreRow player 0 a
    putStrLn $ show empty
    let (x:xs) = map (getPDist player filled) empty
    let [i,j,k] = foldr (\a b -> max a b) x xs
    return [truncate (2*j), truncate k]

--------------------------------------------------------------------------------------------------------













---Random-----------------------------------------------------------------------------------------------
rand :: Bridgit -> IO [Int]
rand a = do
    f <- randomRIO(0, getHeight a)
    if even f then do
        g <- randomRIO (0, getEvenWidth a)
        if (testBoard [f,g] a) == 0 then
            return [f,g]
        else rand a
    else do
        g <- randomRIO (0, getOddWidth a)
        if (testBoard [f,g] a) == 0 then
            return [f,g]
        else rand a
--------------------------------------------------------------------------------------------------------






---ReadFromKeyboard-------------------------------------------------------------------------------------
readPlayerMove :: Int -> Bridgit -> IO [Int]
readPlayerMove player x = do
    putStrLn "From:"
    move <- getLine--
    putStrLn "Direction (u,d,l,r):"
    dir <- getLine
    --putStrLn (show $ getNextB dir (splitOn " " move))
    return (getNextS player dir (splitOn " " move))
--------------------------------------------------------------------------------------------------------


----To create the board and it's attributes-------------------------------------------------------------
getPath :: Int -> [Int]
getPath col = (col-1) : col : getPath col

create :: Int -> Int -> Bridgit
create f c = Board $ [cap] ++ (map (\x -> if even x then [-2]++(take (x-2) [0,0..0])++[-2] else take x [0,0..0]) path) ++ [cap]
    where path = take (f+c-2) $ getPath f
          cap = take f ([(-1),(-1)..(-1)] :: [Int])

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
    | (testBoard m b) == 2 = (checkROddRow [f-1,c] nb) || (checkROddRow [f+1,c] nb) || (checkREvenRow [f-2,c] nb) || (checkREvenRow [f+2,c] nb)
    | otherwise = False
    where nb = setMovement 0 m b
-----------------------------------------------------------------------------------------------------------


playBlue :: Bridgit -> Int -> (Bridgit -> IO [Int]) -> IO()
playBlue x mode cpu = do
  putStrLn (blue "***Blue player turn***")
  --nxtmove <- readPlayerMove 1 x
  --nxtmove <- cpu x
  nxtmove <- rand x
  let t = testBoard nxtmove x
  if t == 0 || t == (-1) then do
        let newx@(Board q) = setMovement 1 nxtmove x
        putStrLn (show newx)
        putStrLn (show q)
        let go = testGO 1 newx
        if go == False then
            playRed newx mode cpu
        else putStrLn (yellow "PLAYER BLUE WINS!!")
  else do
      putStrLn (yellow "ILLEGAL MOVE!!")
      playBlue x mode cpu



playRed :: Bridgit -> Int -> (Bridgit -> IO [Int]) -> IO()
playRed x@(Board a) mode cpu = do
  putStrLn (red "***Red player turn***")

  --nxtmove <- readPlayerMove 2 x
  nxtmove <- cpu x
  putStrLn $ show nxtmove
  nxtmove <- rand x
  let t = testBoard nxtmove x
  if t == 0 || t == (-2) then do
    let newx@(Board q) = setMovement 2 nxtmove x
    putStrLn (show newx)
    putStrLn (show q)
    let go = testGO 2 newx
    if go == False then
        playBlue newx mode cpu
    else putStrLn (yellow "PLAYER RED WINS!!")
  else do
    putStrLn (yellow "ILLEGAL MOVE!!")
    playRed x mode cpu

------------------------------------------------------------------------------------------------------------









main = do
  putStrLn "Welcome to Bridg-it! First of all define the board size:"
  --putStrLn "Define board: "
  --columns <- getLine
  let columns = "4"
  putStrLn "Each movement is defined by the row followed by a space and the column. Rows and columns starts at 1."
  putStrLn "Starting game..."

  let x = (read columns :: Int)
  let board = create x (x-1)

  --playBlue board 0 rand
  playBlue board 0 (cpu0 1)


  --putStrLn (show blue)
  --putStrLn (show red)
  putStrLn "GAME OVER"
