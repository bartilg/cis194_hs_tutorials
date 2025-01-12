xorHelper :: Bool -> Bool -> Bool
xorHelper False x = x
xorHelper True x
  | x == True = False
  | x == False = True

xor :: [Bool] -> Bool
xor = foldr xorHelper False

main = do 
  print (xor [False, True, False])
  print (xor [False, True, False, False, True])
