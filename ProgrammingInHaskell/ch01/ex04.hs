{-
 How should the definition of the function qsort be modified so that it produces a reverse sorted version of a list?
-}

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
  where
    larger = [y | y <- xs, y >= x]
    smaller = [y | y <- xs, y < x]
