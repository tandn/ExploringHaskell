map' f = foldr (\x acc -> f x : acc) []

filter' p = foldr (\x acc -> if p x then x : acc else acc) []
