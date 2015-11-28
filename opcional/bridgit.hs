import System.Environment
import Data.List.Split

getPath :: Int -> [Int]
getPath col = (col-1) : col : getPath col

--let (blue,red) = create 4 3
--files -> columnes del jugador blau
create :: Int -> Int -> ([[Bool]],[[Bool]])
create f c = (blue, red)
  where blue = (map (\x -> take x [(False),(False)..(False)]) (take (f + (f-1)) (getPath c)))
        red =  (map (\x -> take x [(False),(False)..(False)]) (take (c + (c-1)) (getPath f)))


setCol :: Int -> [Bool] -> [Bool]
setCol col submap = a ++ [True] ++ (tail b)
  where (a,b) = splitAt (col-1) submap

setMovement :: [Int] -> [[Bool]] -> [[Bool]]
setMovement [fil,col] mapa = a ++ [setCol col (head b)] ++ (tail b)
  where (a,b) = splitAt (fil-1) mapa


getNext :: String -> [String] -> [Int]
getNext "u" [f,c] = [(read f :: Int),(read c :: Int)]
getNext "d" [f,c] = [((read f :: Int)*2)-1,(read c :: Int)]
getNext "l" [f,c] = [(read f :: Int)+1,(read c :: Int)-1]
getNext "r" [f,c] = [(read f :: Int)+1,(read c :: Int)]

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


main = do
  putStrLn "Welcome to Bridg-it! First of all define the board size:"
  putStrLn "Rows: "
  rows <- getLine
  putStrLn "Columns: "
  columns <- getLine

  putStrLn "Each movementkeepmin is defined by the row followed by a space and the column. Rows and columns starts at 1."
  putStrLn "Starting game..."

  let (blue,red) = create (read rows :: Int) (read columns :: Int)

  playBlue blue red



  --putStrLn (show blue)
  --putStrLn (show red)
  putStrLn "GAME OVER"
