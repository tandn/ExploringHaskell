data Expr = Const Bool
  | Var Char
  | Not Expr
  | And Expr Expr
  | Imply Expr Expr

eval _ (Const c) = c
eval s (Var x) = find x s
eval s (Not e) = not (eval s e)
eval s (And e1 e1) = eval s e1 && eval s e2
eval s (Imply e1 e2) = eval s e1 <= eval s e2
