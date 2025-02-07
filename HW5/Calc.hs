import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add n n2) = (eval n) + (eval n2)
eval (Mul n n2) = (eval n) * (eval n2)

evalStr :: String -> Maybe Integer
evalStr s = maybe Nothing (Just . eval) (parseExp Lit Add Mul s)

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id

main = do
  print (eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)))
  print (evalStr "(2+3)*4")
