last' xs = head (reverse xs)
last'' xs = xs !! (length xs - 1)
last''' xs = head (drop (length xs - 1) xs)
