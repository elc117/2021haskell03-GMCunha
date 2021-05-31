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