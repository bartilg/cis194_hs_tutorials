import ExprT

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add n n2) = (eval n) + (eval n2)
eval (Mul n n2) = (eval n) * (eval n2)

main = do
  print (eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)))
