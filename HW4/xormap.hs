xorHelper :: Bool -> Bool -> Bool
xorHelper False x = x
xorHelper True x
  | x == True = False
  | x == False = True

xor :: [Bool] -> Bool
xor = foldr xorHelper False

mapHelper :: (a -> b) -> a -> [b] -> [b]
mapHelper fun x acc = (fun x) : acc

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (mapHelper f) []

main = do 
  print (xor [False, True, False])
  print (xor [False, True, False, False, True])
  print (map' (*2) [1, 2, 3])
