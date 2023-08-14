{-
 Define a function product that produces the product of a list of numbers, and show using your definition that product [2,3,4] = 24.
-}

product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

-- product [2,3,4]
-- { applying product' }
-- 2 * product [3,4]
-- { applying product' }
-- 2 * 3 * product' [4]
-- { applying product' }
-- 2 * 3 * 4 * product' []
-- { applying product' }
-- 2 * 3 * 4 * 1
-- { applying *}
-- 24
