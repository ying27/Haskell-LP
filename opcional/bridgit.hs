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





----IA 0------------------------------------------------------------------------------------------------
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

distIA :: Double ->  Int -> (Double,Double) -> (Double,Double) -> Double
distIA final player (x,y) (q,w)
    | ret <= 1 && (getcomp player (x,y) == final || getcomp player (x,y) == 0.5) = ret*2
    | player == 2 && (not $ isInt x) && x == q && ret == 1 = ret
    | player == 1 && (not $ isInt y) && y == w && ret == 1 = ret
    | ret == 0.5 = 0.75
    | otherwise = 0
    where ret = (x-q)*(x-q) + (y-w)*(y-w)

getPDist :: Double -> Int -> [(Double,Double)] -> (Double,Double) -> [Double]
getPDist final player x po@(i,j) = [(foldr (\a b -> distIA final player po a + b) 0 x),i,j]

ia0 :: Int -> Bridgit -> IO [Int]
ia0 player b@(Board a) = do
    let (empty,filled) = exploreRow player 0 a
    let final = (fromIntegral $ getOddWidth b) - 0.5
    --putStrLn $ show empty
    let (x:xs) = map (getPDist final player filled) empty
    --putStrLn $ show (x:xs)
    let [i,j,k] = foldr (\a b -> max a b) x xs
    return [truncate (2*j), truncate k]

--------------------------------------------------------------------------------------------------------




----IA 1------------------------------------------------------------------------------------------------
getvs :: Int -> Int
getvs 1 = 2
getvs 2 = 1

ia1sim :: Int -> Bridgit -> (Double,Double) -> Bool
ia1sim player b (x,y) = testGO player newx
    where newx = setMovement player [truncate (2*x),truncate y] b

ia1 :: Int -> Bridgit -> IO [Int]
ia1 player b@(Board a) = do
    let (empty,filled) = exploreRow player 0 a
    let me = filter (ia1sim (getvs player) b) empty
    if me == [] then do

        let govers = filter (ia1sim player b) empty

        if govers == [] then do
            ia0 player b
        else do
            let (x,y) = head govers
            return [truncate (2*x),truncate y]

    else do
        let (x,y) = head me
        return [truncate (2*x),truncate y]
--------------------------------------------------------------------------------------------------------




----IA 2------------------------------------------------------------------------------------------------
checkDiagonal :: Int -> Int -> (Double,Double) -> (Double,Double) -> Bool
checkDiagonal player ew1(xx,yy) (qq,ww)
    | (not $ isInt x) && x == q && (abs(y-w)) == 1 && (max y w) == rel = True
    | otherwise = False
    where rel = fromIntegral(ew1) - x
          (x,y) = cha2b player ew1 (xx,yy)
          (q,w) = cha2b player ew1 (qq,ww)

cha2b :: Int -> Int -> (Double,Double) -> (Double,Double)
cha2b 1 oddWidth (x,y) = ((fromIntegral oddWidth)-y,x)
cha2b 2 oddWidth (x,y) = (x,y)

checkQU :: Int -> Int -> (Double,Double) -> (Double,Double) -> Bool
checkQU player evenWidth1 (xx,yy) (qq,ww)
    | xx < qq && not (isInt xx) = yy < (rel - xx)
    | xx > qq && not (isInt qq) = ww < (rel - qq)
    | otherwise = False
    where rel = fromIntegral(evenWidth1) - 1

checkQD :: Int -> Int -> (Double,Double) -> (Double,Double) -> Bool
checkQD player evenWidth1 (xx,yy) (qq,ww)
  | xx < qq && isInt xx = xx > (rel-yy)
  | xx > qq && isInt qq = qq > (rel-ww)
  | otherwise = False
  where rel = fromIntegral(evenWidth1) - 0.5

checkAdjacent :: Int -> Int -> (Double,Double) -> (Double,Double) -> Bool
checkAdjacent player ew1 (xx,yy) (qq,ww)
    | (x < q && y < w ||  x > q && y > w) && abs(x-q) == 0.5 && abs(y-w) == 0.5 = (checkQU player ew1 (x,y) (q,w) || checkQD player ew1 (x,y) (q,w))
    | otherwise = False
    where (x,y) = cha2b player ew1 (xx,yy)
          (q,w) = cha2b player ew1 (qq,ww)


ia2 :: Int -> Bridgit -> IO [Int]
ia2 player b@(Board a) = do
    let (empty,filled) = exploreRow player 0 a
    if filled == [] then do
        if player == 2 then do
            return [getHeight b-2,0]
        else do
            return [1,0]
    else do
        let evenWidth = (getEvenWidth b) - 1
        --filter (\a -> any (\b -> checkDiagonal evenWidth a b || checkAdjacent a b) filled) empty
        let dmoves = filter (\a -> any (\b -> checkDiagonal player evenWidth a b) filled) empty

        if dmoves == [] then do
            let admoves = filter (\a -> any (\b -> checkAdjacent player evenWidth a b) filled) empty
            if admoves == [] then do
                --putStrLn $ "No movements found"
                ia1 player b
            else do
                let (x,y) = head admoves
                --putStrLn $ "Adjacents :" ++ show admoves
                return [truncate (2*x),truncate y]
        else do
            let (x,y) = head dmoves
            --putStrLn $ "Diagonals: " ++ show dmoves
            return [truncate (2*x),truncate y]
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
create f c = Board $ [cap] ++ (map (\x -> if petit /= x then [-2]++(take (x-2) [0,0..0])++[-2] else take x [0,0..0]) path) ++ [cap]
    where path = take (f+c-2) $ getPath f
          cap = take f ([(-1),(-1)..(-1)] :: [Int])
          petit = min f c

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

t1 [fil,col] [] = -5
t1 [fil,col] b = t2 d
    where (c,d) = splitAt col $ head b

t2 [] = -5
t2 a = head a

testBoard :: [Int] -> Bridgit -> Int
testBoard [fil,col] (Board mapa) = t1 [fil,col] b
    where (a,b) = splitAt fil mapa
          --(c,d) = splitAt col $ head b

-----------------------------------------------------------------------------------------------------------

-----Game Over check---------------------------------------------------------------------------
getcomp :: Int -> (Double,Double) -> Double
getcomp 1 (x,y) = x
getcomp 2 (x,y) = y

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
-----------------------------------------------------------------------------------------------------------


playBlue :: Bridgit -> (Bridgit -> IO [Int]) -> (Bridgit -> IO [Int]) -> IO()
playBlue x strb strr = do
  putStrLn (blue "***Blue player turn***")

  nxtmove <- strb x

  let t = testBoard nxtmove x
  if t == 0 || t == (-1) then do
        let newx@(Board q) = setMovement 1 nxtmove x
        putStrLn (show newx)
        --putStrLn (show q)
        let go = testGO 1 newx
        if go == False then
            playRed newx strb strr
        else putStrLn (yellow "PLAYER BLUE WINS!!")
  else do
      putStrLn (yellow "ILLEGAL MOVE!!")
      playBlue x strb strr



playRed :: Bridgit -> (Bridgit -> IO [Int]) -> (Bridgit -> IO [Int]) -> IO()
playRed x strb strr = do
  putStrLn (red "***Red player turn***")

  --nxtmove <- readPlayerMove 2 x
  nxtmove <- strr x

  let t = testBoard nxtmove x
  if t == 0 || t == (-2) then do
    let newx@(Board q) = setMovement 2 nxtmove x
    putStrLn (show newx)
    --putStrLn (show q)
    let go = testGO 2 newx
    if go == False then
        playBlue newx strb strr
    else putStrLn (yellow "PLAYER RED WINS!!")
  else do
    putStrLn (yellow "ILLEGAL MOVE!!")
    playRed x strb strr

------------------------------------------------------------------------------------------------------------


getIA :: String -> Int -> (Bridgit -> IO [Int])
getIA "1" _ = rand
getIA "2" 1 = ia0 2
getIA "2" 2 = ia0 1
getIA "3" 1 = ia1 2
getIA "3" 2 = ia1 1
getIA "4" 1 = ia2 2
getIA "4" 2 = ia2 1



gameMode board = do
    putStrLn "SELECT GAME MODE:"
    putStrLn "[1] Player VS CPU"
    putStrLn "[2] CPU VS CPU"
    option <- getLine
    if (option /= "1" && option /= "2") then do
        putStrLn "Error. Game Mode not found. Please choose again:"
        gameMode board
    else do
        putStrLn "Please choose the CPU1 Strategy"
        putStrLn "[1] RANDOM"
        putStrLn "[2] IA0"
        putStrLn "[3] IA1"
        putStrLn "[4] IA2"
        str1 <- getLine
        if option == "2" then do
            putStrLn "Please choose the CPU2 Strategy"
            putStrLn "[1] RANDOM"
            putStrLn "[2] IA0"
            putStrLn "[3] IA1"
            putStrLn "[4] IA2"
            str2 <- getLine
            playBlue board (getIA str1 1) (getIA str2 2)
        else do
            putStrLn "Please choose who starts the game"
            putStrLn $ blue "[1] Player"
            putStrLn $ red "[2] CPU"
            start <- getLine
            if start == "1" then
                playBlue board (readPlayerMove 1) (getIA str1 2)
            else
                playRed board (readPlayerMove 1) (getIA str1 2)


main = do
  putStrLn "Welcome to Bridg-it! First of all define the board size:"
  putStrLn "Define board size. Enter a number: "
  columns <- getLine
  let x = read columns :: Int
  let board@(Board q) = create x (x-1)
  --putStrLn (show q)
  putStrLn $ show board

  putStrLn "Each movement is defined by the row followed by a space and the column. Rows and columns starts at 1."
  putStrLn "Starting game..."

  gameMode board


  --playBlue board 0 rand
  --playBlue board 0 (ia0 1)


  --putStrLn (show blue)
  --putStrLn (show red)
  putStrLn "GAME OVER"
