{- tautology checker -}

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

type Env = [(Char,Bool)]

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = [x:xs | x <- [False,True], xs <- bools (n-1)] 

find c e = head [v | (x,v) <- e, c == x] 

subst :: Prop -> [Env]
subst p = let vs = (remdup . vars) p in
              map (zip vs) (bools (length vs))  

eval :: Env -> Prop -> Bool
eval _ (Const a) = a
eval e (Var c) = find c e 
eval e (Not p) = not (eval e p)
eval e (And p1 p2) = (eval e p1) && (eval e p2)
eval e (Imply p1 p2) = not (eval e p1) || (eval e p2)

vars :: Prop -> [Char]
vars (Const a) = []
vars (Var c) = [c]
vars (Not p) = vars p
vars (And p1 p2) = vars p1 ++ vars p2
vars (Imply p1 p2) = vars p1 ++ vars p2

remdup :: Eq a => [a] -> [a]
remdup [] = []
remdup (x:xs) = x : remdup (filter (/=x) xs) 

tautology :: Prop -> Bool
tautology p = and [eval e p | e <- subst p]

p1 :: Prop
p1 = And (Var 'A') (Var 'B')

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')


{- abstract machine -}

