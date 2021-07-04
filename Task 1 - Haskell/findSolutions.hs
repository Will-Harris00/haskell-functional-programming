rule1 :: (Int,Int,Int,Int,Int,Int) -> Bool
rule1 (t1,t2,t3,t4,t5,t6) 
    = t1 /= t2 && t1 /= t3 && t1 /= t4 && t1 /= t5 && t1 /= t6
    && t2 /= t3 && t2 /= t4 && t2 /= t5 && t2 /= t6
    && t3 /= t4 && t3 /= t5 && t3 /= t6
    && t4 /= t5 && t4 /= t6
    && t5 /= t6


rule2 :: (Int,Int,Int,Int,Int,Int) -> Bool
rule2 (t1,t2,t3,t4,t5,t6)
    = isAlternate (t1, t2) && isAlternate (t2, t3) && isAlternate (t3, t4) 
    && isAlternate (t4, t5) && isAlternate (t5, t6)

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
    = isDiffTwo (t1, t2) && isDiffTwo (t2, t3) && isDiffTwo (t3, t4) 
    && isDiffTwo (t4, t5) && isDiffTwo (t5, t6)

isDiffTwo :: (Int, Int) -> Bool
isDiffTwo (n, m)
    | n - m < -2 || n - m > 2 = True
    | otherwise = False


rule4 :: (Int,Int,Int,Int,Int,Int) -> Bool
rule4 (t1,t2,t3,t4,t5,t6)
    = convertInt (t1, t2, t5, t6) && convertInt (t3, t4, t5, t6)

convertInt :: (Int, Int, Int, Int) -> Bool
convertInt (n, m, j, k)
    = isDividend((n * 10) + m,  (j * 10) + k)

isDividend :: (Int, Int) -> Bool
isDividend (x, y)
    | x `rem` y == 0 = True
    | otherwise = False


possibles :: [(Int, Int, Int, Int, Int, Int)]
possibles 
    = do
    u <- [0..9]
    v <- [0..9]
    w <- [0..9]
    x <- [0..9]
    y <- [0..9]
    z <- [0..9]
    return (u, v, w, x, y, z)


isSolution :: (Int, Int, Int, Int, Int, Int) -> Bool
isSolution (t1,t2,t3,t4,t5,t6)
    | rule1 (t1,t2,t3,t4,t5,t6) && rule2 (t1,t2,t3,t4,t5,t6)  
    && rule3 (t1,t2,t3,t4,t5,t6) && rule4 (t1,t2,t3,t4,t5,t6) = True
    | otherwise = False


findSolutions :: [(Int, Int, Int, Int, Int, Int)] -> [(Int, Int, Int, Int, Int, Int)]
findSolutions xs
    = filter isSolution xs


main :: IO()
main
    = putStrLn (show (findSolutions (possibles)))
