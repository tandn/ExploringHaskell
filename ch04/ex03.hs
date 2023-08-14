safetail :: [a] -> [a]
safetail [] = []
safetail = tail

safetail' xs = if length xs == 0 then xs else tail xs

safetail'' xs | xs == [] = xs
              | tail xs
