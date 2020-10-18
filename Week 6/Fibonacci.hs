fib :: Integer -> Integer
fib n = if n < 2 then n else fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]


