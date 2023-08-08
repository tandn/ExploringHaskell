or1 :: Bool -> Bool -> Bool

or1 False x = x
or1 x False = x

or2 True True = True
or2 True False = True
or2 False True = True
or2 False False = False

or3 False False = False
or3 _ _ = True

or4 b c | b == c = b
          | otherwise = True
