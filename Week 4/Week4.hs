fun1 :: [Integer] -> Integer
fun1 = product. map (\x -> x-2) .filter even

-- res = fun1 [1,4,3,4,5]

fun2 :: Integer -> Integer
fun2 =  sum .map (sum . getEvenArr) . (\input -> [input] ++ (takeWhile (/= 4) .tail $ iterate (\x -> getOld x * 3 + 1) input))

getEvenArr :: Integer -> [Integer]
getEvenArr = takeWhile even . iterate (\x -> x `div` 2)

getOld :: Integer -> Integer
getOld = head . dropWhile even . iterate (\x -> x `div` 2)

-- res = fun2 3 --should be 40

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr addNode Leaf

addNode :: a -> Tree a -> Tree a
addNode c Leaf = Node 0 Leaf c Leaf
addNode c (Node _ left oldC right) = case height left > height right of
    True -> let newTree = addNode c right in
        Node (max (height left) (height newTree) + 1) left oldC newTree
    False -> let newTree = addNode c left in
        Node (max (height newTree) (height right) + 1) newTree oldC right

height :: Tree a -> Integer
height Leaf = 0
height (Node h left _ right) = h

res = foldTree "ABCDEFGHIJ"

isBalance :: Tree a -> Bool
isBalance Leaf = True
isBalance (Node _ left _ right) = isBalance left && isBalance right