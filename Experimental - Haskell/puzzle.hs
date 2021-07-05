countToN :: Int -> [(Int, Int, Int, Int, Int, Int)] -> [(Int, Int, Int, Int, Int, Int)]
countToN x ys
    | x /= 999999 = countToN (succ x) (ys ++ [listToTuple . stringToList . padOut . succ $ x])
    | otherwise = ys

padOut :: Int -> String
padOut x
    = replicate (6 - length (show x)) '0' ++ show x

stringToList :: String -> [Int]
stringToList x
    = map (read . (:"")) x :: [Int]

listToTuple :: [Int] -> (Int, Int, Int, Int, Int, Int)
listToTuple [x1, x2, x3, x4, x5, x6]
    = (x1, x2, x3, x4, x5, x6)

rule1 :: (Int,Int,Int,Int,Int,Int) -> Bool
rule1 (t1,t2,t3,t4,t5,t6) 
    = t1 /= t2 && t1 /= t3 && t1 /= t4 && t1 /= t5 && t1 /= t6
    && t2 /= t3 && t2 /= t4 && t2 /= t5 && t2 /= t6
    && t3 /= t4 && t3 /= t5 && t3 /= t6
    && t4 /= t5 && t4 /= t6
    && t5 /= t6


rule2 :: (Int,Int,Int,Int,Int,Int) -> Bool
rule2 (t1,t2,t3,t4,t5,t6)
    = isAlternate (t1, t2) && isAlternate (t2, t3) && isAlternate (t3, t4) && isAlternate (t4, t5) && isAlternate (t5, t6)

isAlternate :: (Int, Int) -> Bool
isAlternate (n, m)
    | isEven n && isOdd m = True
    | isOdd n && isEven m = True
    | otherwise = False

isEven :: Int -> Bool
isEven n
    = n `mod` 2 == 0

isOdd :: Int -> Bool
isOdd m
    = m `mod` 2 == 1


rule3 :: (Int,Int,Int,Int,Int,Int) -> Bool
rule3 (t1,t2,t3,t4,t5,t6)
    = isDiffTwo (t1, t2) && isDiffTwo (t2, t3) && isDiffTwo (t3, t4) && isDiffTwo (t4, t5) && isDiffTwo (t5, t6)

isDiffTwo :: (Int, Int) -> Bool
isDiffTwo (n, m)
    | n - m < -2 || n - m > 2 = True
    | otherwise = False


rule4 :: (Int,Int,Int,Int,Int,Int) -> Bool
rule4 (t1,t2,t3,t4,t5,t6)
    = convertInt (t1, t2, t5, t6) && convertInt (t3, t4, t5, t6)

convertInt :: (Int, Int, Int, Int) -> Bool
convertInt (n, m, j, k)
    = isDividend(n * 10 + m,  j * 10 + k)

isDividend :: (Int, Int) -> Bool
isDividend (x, y)
    | x `rem` y == 0 = True
    | otherwise = False


isSolution :: (Int, Int, Int, Int, Int, Int) -> Bool
isSolution (t1,t2,t3,t4,t5,t6)
    | rule1 (t1,t2,t3,t4,t5,t6) && rule2 (t1,t2,t3,t4,t5,t6)  && rule3 (t1,t2,t3,t4,t5,t6) && rule4 (t1,t2,t3,t4,t5,t6) = True
    | otherwise = False


findSolutions :: [(Int, Int, Int, Int, Int, Int)] -> [(Int, Int, Int, Int, Int, Int)]
findSolutions xs
    = filter isSolution xs


main :: IO()
main
    = putStrLn (show (findSolutions (countToN (-1) [])))
    -- = putStrLn (show $ findSolutions $ possibles)
