import System.Environment
import Data.List.Split

x :: [[Int]]
x = [[0,0,0],[2,1],[2,2,2],[0,0],[0,0,0]]


----Define the data board and it's show definition----------------------------------------------------------
data Bridgit = Board [[Int]]

instance Show Bridgit where
    show (Board a) = showBlueRow a

blue :: String -> String
blue a = "\x1b[34m" ++ a ++ "\x1b[0m"

red :: String -> String
red a = "\x1b[31m" ++ a ++ "\x1b[0m"

showBlueRow :: [[Int]] -> String
showBlueRow [] = "\n"
showBlueRow (x:xs) = (showDots 1 x) ++ (showRedRow xs)

showRedRow :: [[Int]] -> String
showRedRow [] = "\n"
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






----To create the board---------------------------------------------------------------------------------
getPath :: Int -> [Int]
getPath col = col : (col-1) : getPath col

--let (blue,red) = create 4 3
--files -> columnes del jugador blau
create :: Int -> Int -> Bridgit
create f c = Board $ map (\x -> take x  [0,0..0]) path
    where path = take (f+c) $ getPath f
---------------------------------------------------------------------------------------------------------







----Set the movements-------------------------------------------------------------------------------------
setCol :: Int -> [Bool] -> [Bool]
setCol col submap = a ++ [True] ++ (tail b)
  where (a,b) = splitAt (col-1) submap

setMovement :: [Int] -> [[Bool]] -> [[Bool]]
setMovement [fil,col] mapa = a ++ [setCol col (head b)] ++ (tail b)
  where (a,b) = splitAt (fil-1) mapa

-----------------------------------------------------------------------------------------------------------




----Logic of the game--------------------------------------------------------------------------------------
getNext :: String -> [String] -> [Int]
getNext "u" [f,c] = [((read f :: Int)-1)*2,(read c :: Int)]
getNext "d" [f,c] = [((read f :: Int)*2),(read c :: Int)]
getNext "l" [f,c] = [((read f :: Int)*2)-1,(read c :: Int)-1]
getNext "r" [f,c] = [((read f :: Int)*2)-1,(read c :: Int)]

------------------------------------------------------------------------------------------------------------


test = putStrLn ("\x1b[34m" ++ "·-·" ++ "\x1b[0m")
test1 = putStrLn ("\x1b[34m" ++ "· ·" ++ "\x1b[0m")




{-
playBlue :: [[Bool]] -> [[Bool]] -> IO()
playBlue x y = do
  putStrLn "***Blue player turn***"
  putStrLn "From:"
  move <- getLine
  putStrLn "Direction (u,d,l,r):"
  dir <- getLine
  putStrLn (show $ getNext dir (splitOn " " move))

  --TODO: check that the movements are legal
  let newx = setMovement (getNext dir (splitOn " " move)) x
  putStrLn (show newx)


  putStrLn "The winner is the blue player"

-}



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

  --playBlue board



  --putStrLn (show blue)
  --putStrLn (show red)
  putStrLn "GAME OVER"
