module Golf where
import Data.List (delete)
  
range :: [a] -> [Int]
range n = [1 .. (length n)]

skipper :: [a] -> Int -> [a]
skipper [] _ = []
skipper a n = case drop (n-1) a of
  (x:xs) -> x : skipper xs n
  [] -> []

skips :: [a] -> [[a]]
skips a = map (\n -> skipper a n) (range a)

midMax :: Integer -> Integer -> Integer -> Bool
midMax x y z = x < y && y > z

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:rest) 
  | midMax x y z = y : localMaxima(y:z:rest)
  | otherwise = localMaxima(y:z:rest)
localMaxima _ = []

buildStars :: [Integer] -> String
buildStars lis =  map (`checkStar` lis) [0..9]

checkStar :: Integer -> [Integer] -> Char
checkStar x lis 
  | x `elem` lis = '*'
  | otherwise = ' '

cutFirst :: [Integer] -> [Integer]
cutFirst x = foldr delete x [0..9]

histogram :: [Integer] -> String
histogram [] = "==========\n0123456789\n"
histogram n = histogram (cutFirst n) ++ ['\n'] ++ buildStars n

main :: IO ()
main = do 
  print (skips "ABCDEFG")
  print (skips "ABCD")
  print(localMaxima [2,9,5,6,1])
  print(localMaxima [2,3,4,1,5])
  print(localMaxima [1,2,3,4,5])
  print(localMaxima [1,2])
  putStrLn (histogram [1,4,5,4,6,6,3,4,2,4,9])