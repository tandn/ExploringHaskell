find :: Eq a => a -> [(a, b)] -> [b]
find k xs = [b | (a, b) <- xs, k == a]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])
