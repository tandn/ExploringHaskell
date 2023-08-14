sum' :: Num a => [a] -> a -- type definition
sum' [] = 0 -- simple case
sum' (x:xs) = x + sum xs -- other case


take' :: Num a => Int -> [a] -> [a]
take' 0 xs = xs
take' n (x:xs) | n > 0 = x : take' (n - 1) xs


last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs
