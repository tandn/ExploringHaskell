sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

{- show that sum' [x] = x for any number x -}

-- sum' [x]
-- { applying the second clause }
-- x + sum' []
-- { applying the first clause }
-- x + 0
-- { applying + }
-- x
