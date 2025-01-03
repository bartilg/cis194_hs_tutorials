toDigitsRev :: Integer -> [Integer]
toDigitsRev x
 | x <= 0 = []
 | x < 10 = [x]
 | otherwise = (x `mod` 10) : toDigitsRev (x `div` 10)

toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:(y:zs)) = x : (y * 2) : doubleEveryOther zs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:y)
 | x > 10 = (x `mod` 10) + 1 + sumDigits y
 | otherwise = x + sumDigits y

validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther (toDigitsRev x)) `mod` 10 == 0

main = do
 let x = 4012888888881881
 let x2 = 4012888888881882
 print (toDigits x)
 let y =toDigitsRev x
 print y
 let doubled = doubleEveryOther y
 print doubled
 print (sumDigits doubled)
 print (validate x)
 print (validate x2)
