luhnDouble :: Int -> Int
luhnDouble x = let n = x * 2 in if n > 9 then n - 9 else n

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z t = (t + luhnDouble z + y + luhnDouble x) `mod` 10 == 0
