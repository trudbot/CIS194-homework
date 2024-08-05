module LogAnalysis where
import Log

genMessageFromWords :: [String] -> LogMessage
genMessageFromWords ("I" : ts : xs) = LogMessage Info (read ts) (unwords xs)
genMessageFromWords ("W" : ts : xs) = LogMessage Warning (read ts) (unwords xs)
genMessageFromWords ("E" : level : ts : xs) = LogMessage (Error (read level)) (read ts) (unwords xs)
genMessageFromWords xs = Unknown (unwords xs)

-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
-- parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
-- parseMessage "This is not in the right format" == Unknown "This is not in the right format"
parseMessage :: String -> LogMessage
parseMessage s = genMessageFromWords (words s)

-- testParse parse 10 "error.log"
parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)


data CompareResult = LESS | EQUAL | GREATER
  deriving (Show, Eq)
compareTimeStamp :: LogMessage -> LogMessage -> CompareResult
compareTimeStamp (LogMessage _ ts1 _) (LogMessage _ ts2 _)
  | ts1 == ts2 = EQUAL
  | ts1 < ts2 = LESS
  | ts1 > ts2 = GREATER

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert mes Leaf = Node Leaf mes Leaf
insert mes (Node left mesRoot right) = case compareTimeStamp mes mesRoot of
  EQUAL -> Node left mes right
  LESS -> Node (insert mes left) mesRoot right
  GREATER -> Node left mesRoot (insert mes right)

-- foldl类似于javascript的reduce
-- flip用于调换参数顺序
build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

-- 中序遍历
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left mes right) = inOrder left ++ [mes] ++ inOrder right

-- 重要性不小于50的error
isRelevant :: LogMessage -> Bool
isRelevant (LogMessage (Error level) _ _) = level >= 50
isRelevant _ = False

getLogContent :: LogMessage -> String
getLogContent (LogMessage _ _ content) = content

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs = map getLogContent (inOrder (build (filter isRelevant logs)))

main = do
  print (parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help")
  print (parseMessage "I 29 la la la" == LogMessage Info 29 "la la la")
  print (parseMessage "This is not in the right format" == Unknown "This is not in the right format")
  
  print "test parse"
  result <- testParse parse 10 "error.log"
  print (map getLogContent result)

  print "test message tree"
  print (inOrder (build result))

  print "test whatWentWrong"
  result1 <- testWhatWentWrong parse whatWentWrong "sample.log"
  print result1