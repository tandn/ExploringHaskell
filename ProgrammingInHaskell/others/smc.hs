{- The SMC machine http://homepages.inf.ed.ac.uk/gdp/publications/sos_jlap.pdf pg. 15 -}
type V = Char
type T = Bool 
type N = Int
 
data Expr = Int N | Var V | Add Expr Expr | Sub Expr Expr | Mul Expr Expr

data BExp = Bool T | Eq Expr Expr | Or BExp BExp | Not BExp

data Com = Nil | Assign Expr Expr | Seq Com Com | If BExp Com Com | While BExp Com

data Op = ADD | MUL | SUB | EQUAL | OR | NOT | ASSIGN | IF | WHILE

data CE = CC Com | CB BExp | CE Expr | CO Op 

data SE = ST T | SN N | SV V | SB BExp | SC Com   

type Mem = [(V,N)]

type Conf = ([SE], Mem, [CE]) 

upd :: Mem -> V -> N -> Mem
upd [] v m = [(v,m)]
upd ((x,y):ps) v m | x == v = (x,m):ps 
                   | otherwise = (x,y) : upd ps v m

mem :: Mem -> V -> N
mem m v = head [y | (x,y) <- m, x == v]

trans :: Conf -> Conf
-- expr
trans (s, m, CE (Int n) : cs) = (SN n:s, m, cs)
trans (s, m, CE (Var v) : cs) = (SN (mem m v) : s, m, cs)
trans (s, m, CE (Add e e') : cs) = (s,m, CE e : CE e' : CO ADD : cs)
trans (s, m, CE (Sub e e') : cs) = (s,m, CE e : CE e' : CO SUB : cs)
trans (s, m, CE (Mul e e') : cs) = (s,m, CE e : CE e' : CO MUL : cs) 
trans (SN n : SN m' : s, m, CO ADD : cs) = (SN (n+m') : s, m, cs)
trans (SN n : SN m' : s, m, CO SUB : cs) = (SN (m'-n) : s, m, cs)
trans (SN n : SN m' : s, m, CO MUL : cs) = (SN (n*m') : s, m, cs)
-- boolean expr
trans (s, m, CB (Bool t) : cs) = (ST t : s, m, cs)
trans (s, m, CB (Eq e e') : cs) = (s, m, CE e : CE e' : CO EQUAL : cs)
trans (SN n : SN m' : s, m, CO EQUAL : cs) = (ST t : s, m, cs) where t = n == m'
trans (s, m, CB (Or b b') : cs) = (s, m, CB b : CB b' : CO OR : cs)
trans (ST t : ST t' : s, m, CO OR : cs) = (ST t'' : s, m, cs) where t'' = t == t'
trans (s, m, CB (Not b) : cs) = (s,m, CB b : CO NOT : cs)
trans (ST t : s, m, CO NOT : cs) = (ST t' : s, m, cs) where t' = not t
-- command
trans (s,m, CC Nil : c) = (s,m,c)
trans (s,m, CC (Assign (Var v) e) : cs) = (SV v : s, m, CE e : CO ASSIGN : cs)
trans (SN n : SV v : s, m, CO ASSIGN : cs) = (s, upd m v n, cs)
trans (s, m, CC (Seq c c') : cs) = (s,m, CC c : CC c' : cs)
trans (s, m, CC (If b c c') : cs) = (SC c : SC c' : s, m, CB b : CO IF : cs)
trans (ST t : SC c : SC c' : s, m, CO IF : cs) = (s, m, CC c'' : cs) where c'' = if t then c else c'
trans (s, m, CC (While b c) : cs) = (SB b : SC c : s, m, CB b : CO WHILE : cs)
trans (ST True : SB b : SC c : s, m, CO WHILE : cs) = (s, m, CC c : CC (While b c) : cs)
trans (ST False : SB b : SC c : s, m, CO WHILE : cs) = (s, m, cs)
trans ([],m,[]) = ([],m,[])

command :: Com
command = Seq (Assign (Var 'Y') (Int 1)) (While (Not (Eq (Var 'X') (Int 0))) (Seq (Assign (Var 'Y') (Mul (Var 'Y') (Var 'X'))) (Assign (Var 'X') (Sub (Var 'X') (Int 1)))))

memory :: Mem
memory = [('X',3),('Y',5)]

input :: Com -> Mem -> Conf
input cmd m = ([],m,[CC cmd])

eval :: Conf -> Mem
eval ([],m,[]) = m
eval cf = eval (trans cf)
