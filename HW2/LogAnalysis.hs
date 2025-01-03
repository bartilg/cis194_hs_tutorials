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

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert mess Leaf = Node Leaf mess Leaf
insert mess@(LogMessage _ messTime _) (Node lnode nodemess@(LogMessage _ nodeTime _) rnode)
    | messTime < nodeTime = Node (insert mess lnode) nodemess rnode
    | otherwise = Node lnode nodemess (insert mess rnode)
insert _ tree = tree

build:: [LogMessage] -> MessageTree
build [] = Leaf
build messages = foldr insert Leaf messages

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lnode mess rnode) = inOrder lnode ++ [mess] ++ inOrder rnode

main :: IO ()
main = do 
    messages <- testParse parse 10 "error.log"
    print (inOrder (build messages))