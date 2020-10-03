fun1 :: [Integer] -> Integer
fun1 = product. map (\x -> x-2) .filter even

-- res = fun1 [1,4,3,4,5]

fun2 :: Integer -> Integer
fun2 =  sum .map (sum . getEvenArr) . (\input -> [input] ++ (takeWhile (/= 4) .tail $ iterate (\x -> getOld x * 3 + 1) input))

getEvenArr :: Integer -> [Integer]
getEvenArr = takeWhile even . iterate (\x -> x `div` 2)

getOld :: Integer -> Integer
getOld = head . dropWhile even . iterate (\x -> x `div` 2)
.
res = fun2 3 --should be 40