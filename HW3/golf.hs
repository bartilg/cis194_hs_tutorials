module Golf where

safeTail :: [a] -> Maybe a
safeTail [] = Nothing
safeTail _:xs = Just xs 

skips :: [a] -> [[a]]
skips [] = []
skips a:xs = [a:xs] ++ skips (safeTail xs)

main = do 
  skips "testing"
