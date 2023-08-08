third :: [a] -> [a]
third = head . (head . tail)

third' xs = xs !! 2

third'' (x:(y:(z:_))) = z
