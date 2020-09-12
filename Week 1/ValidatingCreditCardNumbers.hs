toDigits  :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | (n <= 9) = [n]
toDigits n = toDigits (div n 10) ++ [mod n 10]

toDigitsRev  :: Integer -> [Integer]
toDigitsRev  n = revList . toDigits $ n

revList :: [Integer] -> [Integer]
revList [] = []
revList (x:list)  = revList list ++ [x]

doubleEveryOther2 :: [Integer] -> [Integer]
doubleEveryOther2 (x:[]) = [x]
doubleEveryOther2 (x:y:[]) = x:[y*2]
doubleEveryOther2 (x:y:list) = x:y*2:(doubleEveryOther2 list)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list = revList . doubleEveryOther2 . revList $ list

sumDigit :: Integer -> Integer
sumDigit 0 = 0
sumDigit n = (sumDigit $ div n 10) + mod n 10 

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (head:list) = sumDigit head + sumDigits list 

validate :: Integer -> Bool
validate x
    | (sumDigits . doubleEveryOther . toDigits $ x) `mod` 10 == 0 = True
    | True = False

res = validate 4012888888881881