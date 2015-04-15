{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Data.List.Split
import Log
        
parseMessage :: String -> LogMessage
parseMessage = f . words
               where f ("I":p:msg) = LogMessage Info (parseInt p) (unwords msg)
                     f ("E":l:p:msg) = LogMessage (Error (parseInt l)) (parseInt p) (unwords msg)
                     f xs = Unknown (unwords xs)

parseInt :: String -> Int
parseInt = read

parse :: String -> [LogMessage]
parse = map parseMessage . splitLn

splitLn :: String -> [String]
splitLn = Data.List.Split.splitOn "\n"


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logM@(LogMessage _ ts _) tree =
    case tree of
      Leaf -> Node Leaf logM Leaf
      (Node _ (Unknown _) _) -> tree
      (Node left nodeL@(LogMessage _ nodeTs _) right) -> if ts <= nodeTs then
                                                 Node (insert logM left) nodeL right else
                                                 Node left nodeL (insert logM right)
                                             
                          
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf


inOrder :: MessageTree -> [LogMessage]
inOrder tree =
    case tree of
      Leaf -> []
      (Node left (Unknown _) right) -> f left [] right
      (Node left l@(LogMessage _ _ _) right) -> f left [l] right
      where f left msg right = inOrder left ++ msg ++ inOrder right



whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMsg . inOrder . build . filter isSevereError
                where getMsg (LogMessage _ _ msg) = msg
                      getMsg (Unknown msg) = msg  -- unreachable

isSevereError :: LogMessage -> Bool
isSevereError (LogMessage (Error level) _ _) = (level > 50)
isSevereError _ = False
