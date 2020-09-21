module Golf where
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

res = localMaxima [2,9,5,6,1] 