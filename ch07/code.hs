add' :: Int -> Int -> Int
add' = \x -> \y -> x + y

twice :: (a -> a) -> a -> a
twice f x = f (f x)

id' :: a -> a
id' x = x

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

_ = map' (+1) [1,3,5,7]

_ = map' even [1,2,3,4]

_ = map' reverse ["abc", "def", "ghi"]


filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs) | p x = x : filter' p xs
                 | otherwise = filter' p xs

sumsquareeven :: [Int] -> Int
sumsquareeven xs = sum (map (^2) (filter even xs))

_ = all even [2,4,6,8]
_ = any odd [2,4,6,8]
_ = takeWhile even [2,4,6,7,8]
_ = dropWhile odd [1,3,5,6,7]

sum' :: Num a => [a] -> a
sum' = foldr (+) 0

product' :: Num a => [a] -> a
product' = foldr (*) 1

or' :: [Bool] -> Bool
or' = foldr (||) False

and' :: [Bool] -> Bool
and' = foldr (&&) True

length' :: [a] -> Int
length' = foldr (\x v -> v + 1) 0

reverse' :: [a] -> [a]
reverse' = foldr (\x v -> v ++ [x]) []


{-

[1,2,3]

1:2:3:[]
--f 1 (f 2 (f 3 v))
f 1 (f 2 (f 3 v))

f (f (f v 1) 2) 3

f (f (f v 1) 2) 3
-}
