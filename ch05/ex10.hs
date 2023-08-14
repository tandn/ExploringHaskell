import Data.Char

u2i :: Char -> Int
u2i c = ord c - ord 'A'

i2u :: Int -> Char
i2u i = chr (i + ord 'A')

i2l :: Int -> Char
i2l i = chr (i + ord 'a')

l2i :: Char -> Int
l2i c = ord c - ord 'a'

shift :: Int -> Char -> Char
shift n c | isLower c = i2l ((l2i c + n) `mod` 26)
          | isUpper c = i2u ((u2i c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n c | c <- xs]

percent :: Int -> Int -> Float
percent m n = (fromIntegral m / fromIntegral n) * 100

alphabet :: [Char] -> Int
alphabet xs = sum [1 | x <- xs, isLower x || isUpper x]

count :: Char -> [Char] -> Int
count x xs = sum [1 | x' <- xs, x == toLower x']

freqs :: [Char] -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
  where
    n = alphabet xs

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

chisquare :: [Float] -> [Float] -> [Float]
chisquare os es = [((o-e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [p | (x',p) <- zip xs [0..], x' == x]

crack :: [Char] -> [Char]
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisquare (rotate n table') table | n <- [0..25]]
    table' = freqs xs
