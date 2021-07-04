rule3 :: (Int,Int,Int,Int,Int,Int) -> Bool
rule3 (t1,t2,t3,t4,t5,t6)
    = isDiffTwo (t1, t2) && isDiffTwo (t2, t3) && isDiffTwo (t3, t4) 
    && isDiffTwo (t4, t5) && isDiffTwo (t5, t6)

isDiffTwo :: (Int, Int) -> Bool
isDiffTwo (n, m)
    | abs (n - m) > 2 = True
    | otherwise = False

main :: IO()
main
    = putStrLn (show (rule3 (4,9,6,3,0,7)))
    -- = putStrLn (show (rule3 (6,9,2,7,0,3)))
