-- Code for chapter 01

double x = x + x

my_sum = sum [1..5]

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum'(xs)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [y | y <- xs, y < x]
    larger = [y | y <- xs, y >= x]

seqn :: Monad m => [m a] -> m [a]
seqn [] = return []
seqn (act:acts) = do x <- act
                     xs <- seqn acts
                     return (x:xs)
