isDigit' :: Char -> Bool
isDigit' c = c >= '0' && c <= '9'

even' :: Integral a => a -> Bool
even' n = mod n 2 == 0

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs = (take n xs, drop n xs)

recip' :: Factorial a => a -> a
recip' n = 1 / n

abs' :: Int -> Int
abs' n | n < 0 = -n
      | otherwise n

signum' :: Int -> Int
signum' n | n < 0 = -1
         | n == 0 = 0
         | otherwise 1

not' :: Bool -> Bool
not' True = False
not' False = True
