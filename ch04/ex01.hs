halve :: [a] -> ([a], [a])
halve xs = let n = length xs `div` 2 in
             (take n xs, drop n xs)
