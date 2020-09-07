{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Debug.Trace

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
parseIntStr (IntStr n (Str (x:str))) = case getNum x of
    10 -> IntStr n (Str str)
    _ -> parseIntStr (IntStr (n*10 + (getNum x)) (Str str))


parseIntFromStr :: String -> IntStr
parseIntFromStr str = parseIntStr (IntStr 0 (Str str))

parseMessage :: String -> LogMessage
parseMessage (c : _ : list) = case (c, (parseIntFromStr list))of
    ('I', (IntStr i (Str s))) -> LogMessage Info i s
    ('W', (IntStr i (Str s))) -> LogMessage Warning i s
    ('E', (IntStr i (IntStr i2 (Str s)))) -> LogMessage (Error i) i2 s
    (_, (IntStr n (Str s))) -> Unknown s

main = parseMessage "E 2 562 help help" (trace main)