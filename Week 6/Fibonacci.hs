{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

fib :: Integer -> Integer
fib n = if n < 2 then n else fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibTail :: [Integer] -> [Integer]
fibTail (x:y:_) = x+y : fibTail [y, x+y]

fibs2 :: [Integer]
fibs2 = [0, 1] ++ fibTail [0, 1]

-- fibs 这种方式确实很有趣，或许体现了 Haskell 惰性求值的优势，这种实现会更函数式些
-- 但最初的fib无法重复利用已计算的值而这里可以应该与具体 Haskell 的实现有关
fibs :: [Integer]
fibs = 0:1:zipWith (+) fibs (tail fibs)


data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream x s) = x : streamToList s

instance Show a => Show (Stream a) where
    show = unwords . map show . take 20 .streamToList

streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x s) =  Stream (f x) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (+ 1) ruler)

-- 写到这个函数的时候有点开心的
interleaveStreams :: Stream Integer -> Stream Integer -> Stream Integer
-- interleaveStreams (Stream x nx) (Stream y ny) = Stream x (Stream y (interleaveStreams nx ny))
interleaveStreams (Stream x next) s = Stream x (interleaveStreams s next)

-- res = ruler

x :: Stream Integer
x = (Stream 0 (Stream 1 (streamRepeat 0)))

instance Num (Stream Integer) where
    fromInteger n = Stream n (streamRepeat 0)
    negate = streamMap (* (-1))
    (Stream a an) + (Stream b bn) = Stream (a+b) (an + bn)
    (Stream a0 a') * b@(Stream b0 b') = Stream (a0*b0) (fromInteger a0*b' + a'*b)

instance Fractional (Stream Integer) where
    a@(Stream a0 a') / b@(Stream b0 b') =  Stream (a0 `div` b0) (fromInteger (1 `div` b0))*(a' - a/b *b')

fibs3 :: Stream Integer
