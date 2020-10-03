type Table = [(Bool, Int)]

update :: Int -> Bool -> Int -> Table -> Table
update s [] = [s]
update s a b (x:xs)
      | s == x = update[(a,b)]:xs
      | otherwise = update xs