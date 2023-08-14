all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' p (x:xs) = (p x) && all' p xs

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = True
any' p (x:xs) = (p x) && any' p xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x == True = x : takeWhile' p xs
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) | p x == True = dropWhile' p xs
                    | otherwise = (x:xs)
