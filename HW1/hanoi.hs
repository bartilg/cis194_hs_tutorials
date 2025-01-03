type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 x y z = [(x,z)]
hanoi n x y z = hanoi (n-1) x z y ++ [(x,z)] ++ hanoi (n-1) y x z

main = do 
    print(hanoi 3 "x" "y" "z")