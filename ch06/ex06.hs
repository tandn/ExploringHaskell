and' :: [Bool] -> Bool
and' [] = True
and' (b:bs) = b && and' bs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n a = a : replicate' (n - 1) a

(!!!) :: [a] -> Int -> a
(x:_) !!! 0 = x
(x:xs) !!! n = xs !!! (n - 1)

elem' :: Eq a => a -> [a] -> Bool
elem' v (x:xs) | v == x = True
               | otherwise = elem' v xs
