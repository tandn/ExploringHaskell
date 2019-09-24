{- chap 1. 4,5 -}

{- qsort reverse -}

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort bigger ++ [x] ++ qsort smaller
               where bigger = [b | b <- xs, b >= x]
                     smaller = [s | s <- xs, s < x]

{- replacing <= by < excludes first elements of the parameter lists --} 

{- chap 2. 5 -}

{- define init in two ways -}
init1 :: [a] -> [a]
init1 [x] = []
init1 (x:xs) = x : init1 xs

init2 :: [a] -> [a]
init2 xs = take ((length xs) - 1) xs 


{- chap 3. 3,4,5 -}
second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b -> (a,b)
pair x y = (x,y)

double :: Num a => a -> a
double x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)


{- https://stackoverflow.com/questions/1132051/is-finding-the-equivalence-of-two-functions-undecidable -}


{- chap 4 -}
halve :: [a] -> ([a],[a])
halve xs = let n = length xs `div` 2 in (take n xs, drop n xs)


and' :: Bool -> Bool -> Bool
and' a b | a == b = if a == True then True else False
       | otherwise = False

and'' :: Bool -> Bool -> Bool
and'' a b | a == True = b
          | a == False = False

mul :: Int -> Int -> Int -> Int
mul = \x y z -> x * y * z



luhnDouble :: Int -> Int
luhnDouble n | n*2 > 9 = n*2 - 9
             | otherwise = n*2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z t = (luhnDouble x + y + luhnDouble z + t) `mod` 10 == 0 
