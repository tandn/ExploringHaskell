squareSum :: Num a => a
squareSum = sum [x^2 | x <- [1..100]]
