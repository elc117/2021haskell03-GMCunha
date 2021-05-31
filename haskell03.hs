add10toall :: [Int] -> [Int]
add10toall l = [x + 10 | x <- l]

multN :: Int -> [Int] -> [Int]
multN n l = [x * n | x <- l]

multN' :: Int -> [Int] -> [Int]
multN' n l = map (n *) l

applyExpr :: [Int] -> [Int]
applyExpr l = [3*x+2 | x <- l]

applyExpr' :: [Int] -> [Int]
applyExpr' l = map (\y -> 3*y+2) l

addSuffix :: String -> [String] -> [String]
addSuffix str l = [s ++ str | s <- l]

selectgt5 :: [Int] -> [Int]
selectgt5 l = [x | x <- l, x > 5]

sumOdds :: [Int] -> Int
sumOdds l = sum [x | x <- l, odd x]

sumOdds' :: [Int] -> Int
sumOdds' l = sum (filter odd l)

selectExpr :: [Int] -> [Int]
selectExpr l = [x | x <- l, x >= 20, x <= 50]