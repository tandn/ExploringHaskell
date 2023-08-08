{- code for chapter 2 -}

double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

a = b + c
  where
    b = 1
    c = 2

d = a * 2

a' = b + c
  where
    {b = 1;
     c = 2}
d' = a' * 2

a'' = b + c where {b = 1; c = 2}; d'' = a'' * 2

main :: IO ()
main =
  print (14 == 2+3*4) >>
  print (20 == (2+3)*4) >>
  print (5.0 == sqrt (3^2 + 4^2)) >>
  print (1 == head [1,2,3,4,5]) >>
  print ([2,3,4,5] == tail [1,2,3,4,5]) >>
  print (3 == [1,2,3,4,5] !! 2) >>
  print ([1,2,3] == take 3 [1,2,3,4,5]) >>
  print ([4,5] == drop 3 [1,2,3,4,5]) >>
  print (5 == length [1,2,3,4,5]) >>
  print (15 == sum [1,2,3,4,5]) >>
  print (120 == product [1,2,3,4,5]) >>
  print ([1,2,3,4,5] == [1,2,3] ++ [4,5]) >>
  print ([5,4,3,2,1] == reverse [1,2,3,4,5]) >>
  print (40 == quadruple 10) >>
  print ([1,2,3,4] == take (double 2) [1,2,3,4,5]) >>
  print (3628800 == factorial 10) >>
  print (3 == average [1,2,3,4,5]) >>
  print (a == a' && a == a'' && d == d' && d == d'')
