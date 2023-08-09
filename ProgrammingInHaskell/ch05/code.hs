_ = [x^2 | x <- [1..5]]

_ = [(x,y) | x <- [1..3], y <- [4..5]]

_ = [(x,y) | x <- [4..5], x <- [1..3]]

_ = [(x,y) | x <- [1..3], y <- [x..3]]

concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

firsts :: [(a,b)] -> [a]
firsts ts = [x | (x,_) <- ts]

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime n]

find :: Eq a => a -> [(a,b)] -> b
find k t = [v | (k'v) <- t, k == k']

_ = zip ['a' 'b','c'] [1,2,3,4]

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

_ = "abcde" !! 2

_ = take 3 "abcde"

_ = length "abcde"

_ = zip "abc" [1..4]

lowers :: String -> Int
lowers xs = length [x | x <- xs, 'a' <= x <= 'z']

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

_ lowers "Haskell"

_ count 's' "Mississippi"
