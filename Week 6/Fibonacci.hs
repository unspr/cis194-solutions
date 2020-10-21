fib :: Integer -> Integer
fib n = if n < 2 then n else fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibTail :: [Integer] -> [Integer]
fibTail (x:y:_) = x+y : fibTail [y, x+y]

fibs2 :: [Integer]
fibs2 = [0, 1] ++ fibTail [0, 1]

-- fibs3 这种方式确实很有趣，或许体现了 Haskell 惰性求值的优势，这种实现会更函数式些
-- 但最初的fib无法重复利用已计算的值而这里可以应该与具体 Haskell 的实现有关
fibs3 :: [Integer]
fibs3 = 0:1:zipWith (+) fibs3 (tail fibs3)


data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream x s) = x : streamToList s

instance Show a => Show (Stream a) where
    show = unwords . map show . take 20 .streamToList

-- res = streamMap (+ 1).streamRepeat $ 1

streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x s) =  Stream (f x) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

ruler :: Stream Integer
