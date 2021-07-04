rule1 :: (Int,Int,Int,Int,Int,Int) -> Bool

rule1 (t1,t2,t3,t4,t5,t6) 
    = t1 /= t2 && t1 /= t3 && t1 /= t4 && t1 /= t5 && t1 /= t6
    && t2 /= t3 && t2 /= t4 && t2 /= t5 && t2 /= t6
    && t3 /= t4 && t3 /= t5 && t3 /= t6
    && t4 /= t5 && t4 /= t6
    && t5 /= t6

main :: IO()
main
    = putStrLn (show (rule1 (1,2,3,4,5,6)))
    -- = putStrLn (show (rule1 (4,9,6,3,0,7)))
    -- = putStrLn (show (rule1 (6,9,2,7,0,3)))
