-- multiplies all even values - 2 together
fun1old :: [Integer] -> Integer
fun1old [] = 1
fun1old (x:xs)
  | even x = (x - 2) * fun1old xs
  | otherwise = fun1old xs

fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter (even)

-- if its even call it divided by 2, else recurse 3n+1 
fun2old :: Integer -> Integer
fun2old 1 = 0
fun2old n 
  | even n = n + fun2old (n `div` 2)
  | otherwise = fun2old (3 * n + 1)

fun2 :: Integer -> Integer
fun2 = sum . filter (even) .takeWhile (>1) . (iterate (\x -> if even x then div x 2 else 3 * x + 1))

main = do
  print (fun1old [5,6,2,6,8,3,7,8,9])
  print (fun1 [5,6,2,6,8,3,7,8,9])
  print (fun1old [55,56,52,36,78,83,57,85,99])
  print (fun1 [55,56,52,36,78,83,57,85,99])
  print (fun2old 9)
  print (fun2 9)
  print (fun2old 89)
  print (fun2 89)
