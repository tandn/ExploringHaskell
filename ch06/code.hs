fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

mul :: Int -> Int -> Int
mul m 0 = 0
mul m n = m + mul m (n - 1)

prod :: [Int] -> Int
prod [] = 1
prod (n:ns) = n * prod ns

leng :: [a] -> Int
leng [] = 0
leng (_:xs) = 1 + leng xs

revers :: [a] -> [a]
revers [] = []
revers (x:xs) = revers xs ++ [x]

conca :: [a] -> [a] -> [a]
conca [] ys = ys
conca (x:xs) ys = x:(conca xs ys)

insert :: Ord a => a -> [a] -> [a]
insert a [] = [a]
insert a (x:xs) | a <= x = a:x:xs
                | otherwise = x:(insert a xs)

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip xs ys

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' n (x:xs) = drop (n - 1) xs
init' :: [a] -> [a]
init' [_] = []
init' (x:xs) = x : init' xs
