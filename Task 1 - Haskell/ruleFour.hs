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


main :: IO()
main
    = putStrLn (show (rule4 (4,9,6,3,0,7)))
    -- = putStrLn (show (rule4 (6,9,2,7,0,3)))
