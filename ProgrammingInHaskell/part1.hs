import Data.Char -- for chap 5


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


{- chap 5 -}
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfect :: Int -> Bool
perfect m = m == sum (factors m) - m

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], perfect x]

twocomp :: [a] -> [b] -> [(a,b)]
twocomp xs ys = concat [[(x,y) | y <- ys] | x <- xs]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

position :: Eq a => a -> [a] -> [Int]
position x xs = find x (zip xs [0..])

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]


-- Caesar cipher
int2letu :: Int -> Char
int2letu n = chr (ord 'A' + n)

int2letl :: Int -> Char
int2letl n = chr (ord 'a' + n)

let2int :: Char -> Int
let2int c | isUpper c = ord c - ord 'A'
          | isLower c = ord c - ord 'a'

letter :: Char -> Bool
letter c | isLower c || isUpper c = True
         | otherwise = False

shift :: Int -> Char -> Char
shift n c | isLower c = int2letl ((let2int c + n) `mod` 26)
          | isLower c = int2letu ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

percent :: Int -> Int -> Float
percent m n = (fromIntegral m) / (fromIntegral n) * 100

count :: Eq a => a -> [a] -> Int
count _ [] = 0
count v (x:xs) | v == x = 1 + count v xs
               | otherwise = count v xs

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['A'..'Z'] ++ ['a'..'z']] where n = length [x | x <- xs, letter x]

chisqrt :: [Float] -> [Float] -> Float
chisqrt os es = sum [(oi-ei)^2 / ei | (oi,ei) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

table :: [Float]
table = [8.167,1.492,2.782,4.253,12.702,2.228,2.015,6.094,6.966,0.153, 0.772, 4.025,2.406,6.749,7.507,1.929, 0.095,5.987,6.327,9.056,2.758,0.978,2.360,0.150,1.974,0.074,8.167,1.492,2.782,4.253,12.702,2.228,2.015,6.094,6.966,0.153, 0.772, 4.025,2.406,6.749,7.507,1.929, 0.095,5.987,6.327,9.056,2.758,0.978,2.360,0.150,1.974,0.074]

decode :: String  -> String
decode xs = encode (-f) xs
  where f = head (position (minimum chitab) chitab)
        chitab = [chisqrt (rotate k observe) table | k <- [0..25]]
        observe = freqs xs


{- chap 6 -}

and1 :: [Bool] -> Bool
and1 [] = True
and1 (x:xs) = x && and1 xs

concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (x:xs) = x ++ concat1 xs

replicate1 :: Int -> a -> [a]
replicate1 0 _ = []
replicate1 n x = (x : replicate1 (n-1) x)

(!!!) :: [a] -> Int -> a
(!!!) (x:_) 0 = x
(!!!) (_:xs) n = (!!!) xs (n-1)


elem1 :: Eq a => a -> [a] -> Bool
elem1 x (y:ys) | x == y = True
               | otherwise = elem1 x ys
elem1 _ [] = False

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge ys [] = ys
merge (x:xs) (y:ys) | x <= y  = (x : merge xs (y:ys))
                    | otherwise = (y : merge (x:xs) ys)

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = let (fst,snd) = halve xs in merge (msort fst) (msort snd)

all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' p (x:xs) = (p x) && (all' p xs)


any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' p (x:xs) = (p x) || (any' p xs)

remdup :: Eq a => [a] -> [a]
remdup [] = []
remdup (x:xs) = x : filter (/= x) (remdup xs)
