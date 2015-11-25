getPath :: Int -> [Int]
getPath col = (col-1) : col : getPath col

--let (blue,red) = create 4 3
--files -> columnes del jugador blau
create :: Int -> Int -> ([[Bool]],[[Bool]])
create f c = (blue, red)
  where blue = (map (\x -> take x [(False),(False)..(False)]) (take (f + (f-1)) (getPath c)))
        red =  (map (\x -> take x [(False),(False)..(False)]) (take (c + (c-1)) (getPath f)))


main = do
  putStrLn "Welcome to Bridg-it! First of all define the board size:"
  putStrLn "Rows: "
  rows <- getLine
  putStrLn "Columns: "
  columns <- getLine

  let (blue,red) = create (read rows :: Int) (read columns :: Int)
  putStrLn (show blue)
  putStrLn (show red)
  putStrLn "GAME OVER"
