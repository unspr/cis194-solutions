tmpFun :: Integer->[Integer]->[Integer]
tmpFun n list
    | n <= 9 = n:list
tmpFun n list = tmpFun (div n 10) ((mod n 10):list)

toDigits  :: Integer -> [Integer]
toDigits n
    |n <= 0 = []
toDigits n = tmpFun n []

toDigitsRev  :: Integer -> [Integer]
toDigitsRev  n
    | n <= 0 = []
    | (n <= 9) = [n]
toDigitsRev n = (mod n 10) : toDigitsRev (div n 10)
 

genList :: [Integer] -> String
genList (x:[]) = show x
genList (x:list) = show x ++ "," ++ genList list

printList :: String -> String
printList n = "[" ++ n ++ "]"


revList :: [Integer] -> [Integer] -> [Integer]
revList (x:list) y
    | list == [] = x:y
    | True = revList list (x:y)



doubleEveryOther2 :: [Integer] -> [Integer]
doubleEveryOther2 (x:[]) = [x]
doubleEveryOther2 (x:y:[]) = x:[y*2]
doubleEveryOther2 (x:y:list) = x:y*2:(doubleEveryOther2 list)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list = revList (doubleEveryOther2 (revList list [])) []

sumDigit :: Integer -> Integer
sumDigit 0 = 0
sumDigit n = (mod n 10) + sumDigit (div n 10)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (head:list) = (sumDigit head) + (sumDigits list) 

validate :: Integer -> Bool
validate x
    | mod (sumDigits (doubleEveryOther (toDigits x))) 10 == 0 = True
    | True = False

printBool :: Bool -> String
printBool True = "True"
printBool False = "False"
-- main = putStrLn (printList (genList (doubleEveryOther [8,7,6,5])))
-- main = putStrLn (printList (genList  [sumDigits [16,7,12,5] ] ))
main = putStrLn (printBool (validate 4012888888881882))

