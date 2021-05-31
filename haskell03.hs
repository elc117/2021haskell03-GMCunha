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
sumOdds' l = sum (filter (\ y -> y `mod` 2 == 1) l)

selectExpr :: [Int] -> [Int]
selectExpr l = [x | x <- l, even x, x >= 20, x <= 50]

countShorts :: [String] -> Int
countShorts l = length [x | x <- l, length x < 5]

calcExpr :: [Float] -> [Float]
calcExpr l = filter (\ y -> y > 10) [x^2/2 | x <- l]

trSpaces :: String -> String
trSpaces l = [if x==' ' then '-' else x | x <- l]

selectSnd :: [(Int,Int)] -> [Int]
selectSnd t = [x | (_, x) <- t]

dotProd :: [Int] -> [Int] -> Int
dotProd l1 l2 = sum ([x*y | (x, y) <- zip l1 l2])

{-alternativa sem list comprehension
mulTuple :: (Int, Int) -> Int
mulTuple (x, y) = x*y

dotProd :: [Int] -> [Int] -> Int
dotProd l1 l2 = sum (map mulTuple (zip l1 l2))-}