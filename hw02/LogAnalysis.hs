{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log


parseMessage :: String -> LogMessage
parseMessage = parseWords . words
  where
    parseWords ("I":t:ws) = LogMessage Info (read t) (unwords ws)
    parseWords ("W":t:ws) = LogMessage Warning (read t) (unwords ws)
    parseWords ("E":s:t:ws) = LogMessage (Error (read s)) (read t) (unwords ws)
    parseWords ws = Unknown (unwords ws)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Inserts into a binary search tree of LogMessages
-- ordered by the integer timestamp
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert lm Leaf = Node Leaf lm Leaf
insert lm (Node l cur r)
  | lmTime < curTime = Node (insert lm l) cur r
  | lmTime >= curTime = Node l cur (insert lm r)
  where (LogMessage _ lmTime _) = lm
        (LogMessage _ curTime _) = cur

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l lm r) = (inOrder l) ++ [lm] ++ (inOrder r)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMessage . inOrder . build . filter (isSevere 50)

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ msg) = msg
getMessage (Unknown _) = "We can't use record syntax yet"

isSevere :: Int -> LogMessage -> Bool
isSevere severity (LogMessage (Error n) _ _) = n >= severity
isSevere _ _ = False


