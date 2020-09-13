{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
-- import Debug.Trace

--此处有编译原理中产生式的思想
data IntStr = IntStr Int IntStr | Str String
    deriving Show

getNum :: Char -> Int
getNum c = case c of
    '0' -> 0
    '1' -> 1
    '2' -> 2
    '3' -> 3
    '4' -> 4
    '5' -> 5
    '6' -> 6
    '7' -> 7
    '8' -> 8
    '9' -> 9
    _ -> 10

parseIntStr :: IntStr -> IntStr
parseIntStr (IntStr n (Str (x:y:str))) = case (getNum x, getNum y) of
    (10, 10) -> IntStr n (Str (y:str))
    (10, _) -> IntStr n (parseIntStr (IntStr 0 (Str (y:str))))
    _ -> parseIntStr (IntStr (n*10 + (getNum x)) (Str (y:str)))
parseIntStr s = s


parseIntFromStr :: String -> IntStr
parseIntFromStr str = parseIntStr (IntStr 0 (Str str))

parseMessage :: String -> LogMessage
parseMessage str@(c : _ : list) = case (c, (parseIntFromStr list))of
    ('I', (IntStr i (Str s))) -> LogMessage Info i s
    ('W', (IntStr i (Str s))) -> LogMessage Warning i s
    ('E', (IntStr i (IntStr i2 (Str s)))) -> LogMessage (Error i) i2 s
    _ -> Unknown str 
parseMessage s = Unknown s

-- res = parseMessage "E 2 562 help help"

parse :: String -> [LogMessage]
parse s = parseArray . lines $ s

parseArray :: [String] -> [LogMessage]
parseArray [] = []
parseArray (s:list) = parseMessage s : parseArray list

-- res = testParse parse 10 "error.log"

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMessage@(LogMessage _ time _) (Node ltree node@(LogMessage _ nodeTime _) rtree)
    | time < nodeTime = (Node (insert logMessage ltree) node rtree)
    | time > nodeTime = (Node ltree node (insert logMessage rtree))
insert logMessage Leaf = (Node Leaf logMessage Leaf) 
insert _ _ = Leaf

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (h:list) = insert h (build list)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node ltree node rtree) = inOrder ltree ++ [node] ++ inOrder rtree

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong list = logToStr . inOrder . build . importantLog $ list

importantLog :: [LogMessage] -> [LogMessage]
importantLog [] = []
importantLog (h@(LogMessage (Error n) _ _):list)
    | n >= 50 = h: importantLog list
importantLog (_:list) = importantLog list     

logToStr :: [LogMessage] -> [String]
logToStr [] = []
logToStr ((LogMessage (Error _) _ s):list) = s : logToStr list
logToStr _ = []

res = testWhatWentWrong parse whatWentWrong "sample.log"