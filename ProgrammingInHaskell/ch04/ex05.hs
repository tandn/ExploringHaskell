and' :: Bool -> Bool -> Bool
and' b c = if b then
            if c then True else False
          else False
