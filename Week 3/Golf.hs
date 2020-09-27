module Golf where
import Data.List

skips :: [a] -> [[a]]
skips s = (map $ skipAs s) [1..length s]

skipAs :: [a] -> Int -> [a]
skipAs list n = let l = drop (n-1) list in
    case l of
        [] -> []
        (x:remain) -> [x] ++ skipAs remain n

-- res = skips "ABCD"

localMaxima :: [Integer] -> [Integer]
localMaxima full@(x:y:z:list)
    | x < y && z < y = [y] ++ localMaxima (drop 1 full)
    | True = localMaxima (drop 1 full)
localMaxima _ = []

-- res = localMaxima [2,9,5,6,1] 

histogram :: [Integer] -> String
histogram list = unlines $ reverse (["0123456789", "=========="] ++ dot list)

dot :: [Integer] -> [String]
dot [] = []
dot list =  [listToXin (nub list)] ++ dot (list \\ (nub list))

listToXin :: [Integer] -> String
listToXin list = foldr func "          " list

func :: Integer -> String -> String
func n list = take (fromIntegral n) list ++ "*" ++ drop (fromIntegral (n + 1)) list

res = putStr (histogram [1,4,5,4,6,6,3,4,2,4,9])
