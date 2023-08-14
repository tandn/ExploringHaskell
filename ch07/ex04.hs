dec2int :: [Int] -> Int
dec2int = fst . foldr (\x (acc,i) -> (acc + x * 10^i, i + 1)) (0, 0)

dec2int' :: [Int] -> Int
dec2int' xs = fst (foldl (\(acc,i) x -> (acc + x * 10^i, i - 1)) (0, length xs) xs)
