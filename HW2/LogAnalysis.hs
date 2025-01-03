{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage x = case words x of
    "I":n:msg -> LogMessage Info (read n) (unwords msg)
    "W":n:msg -> LogMessage Warning (read n) (unwords msg)
    "E":n:n2:msg -> LogMessage (Error (read n)) (read n2) (unwords msg)
    _ -> Unknown x
    
parse:: String -> [LogMessage]
parse s = map parseMessage (lines s)

main :: IO [LogMessage]
main = do 
    testParse parse 10 "error.log"