import System.Environment
import Data.List.Split

x :: Bridgit
x = Board [[0,0,0],[1,0],[0,0,0],[1,0],[0,0,0]]


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






----To create the board and it's attributes--------------------------------------------------------------
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
checkGameOverOddRow :: [Int] -> Bridgit -> Bool
checkGameOverOddRow m@[f,c] b@(Board mapa)
  | (getHeight b) <= (f+1) = True
  | (testBoard m b) == 1 = (checkGameOverEvenRow [f+1,c] b) || (checkGameOverEvenRow [f+1,c+1] b) || (checkGameOverOddRow [f+2,c] b)
  | otherwise = False

checkGameOverEvenRow :: [Int] -> Bridgit -> Bool
checkGameOverEvenRow m@[f,c] b@(Board mapa)
    | (getHeight b) <= (f+1) = True
    | (testBoard m b) == 1 = (checkGameOverOddRow [f+1,c-1] b) || (checkGameOverOddRow [f+1,c] b) || (checkGameOverEvenRow [f+2,c] b)
    | otherwise = False


-----------------------------------------------------------------------------------------------------------




playBlue :: Bridgit -> IO()
playBlue x = do
  putStrLn (blue "***Blue player turn***")
  putStrLn "From:"
  move <- getLine--
  putStrLn "Direction (u,d,l,r):"
  dir <- getLine
  --putStrLn (show $ getNextB dir (splitOn " " move))
  let nxtmove = getNextS 1 dir (splitOn " " move)

  if (testBoard nxtmove x) == 0 then do
        let newx = setMovement 1 nxtmove x
        putStrLn (show newx)
        let go = foldr (\a b-> (checkGameOverOddRow [1,a] newx) || b) False (take (getOddWidth x) [0,1..])
        if go == False then
            playRed newx
        else putStrLn (yellow "PLAYER BLUE WINS!!")
  else do
      putStrLn (yellow "ILLEGAL MOVE!!")
      playBlue x



playRed :: Bridgit -> IO()
playRed x = do
  putStrLn (red "***Red player turn***")
  putStrLn "From:"
  move <- getLine
  putStrLn "Direction (u,d,l,r):"
  dir <- getLine

  let nxtmove = getNextS 2 dir (splitOn " " move)

  if (testBoard nxtmove x) == 0 then do
    let newx = setMovement 2 nxtmove x
    putStrLn (show newx)
    playBlue newx
  else do
    putStrLn (yellow "ILLEGAL MOVE!!")
    playRed x

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

  playBlue board



  --putStrLn (show blue)
  --putStrLn (show red)
  putStrLn "GAME OVER"
