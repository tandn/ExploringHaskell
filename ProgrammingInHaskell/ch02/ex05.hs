init' xs = reverse (drop 1 (reverse xs))
init'' xs = take (length xs - 1) xs
