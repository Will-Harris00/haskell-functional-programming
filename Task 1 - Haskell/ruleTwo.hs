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

main :: IO()
main
    = putStrLn (show (rule2 (1,2,3,4,5,6)))
    -- = putStrLn (show (rule2 (4,9,6,3,0,7)))
    -- = putStrLn (show (rule2 (6,9,2,7,0,3)))
