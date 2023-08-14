sumdown :: Int -> Int
sumdown 0 = 0
sumdown m = m + sumdown (m - 1)
