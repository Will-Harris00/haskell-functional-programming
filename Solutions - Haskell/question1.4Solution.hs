rule4 :: (Int, Int, Int, Int, Int, Int) -> Bool
rule4 ( x1, x2, x3, x4, 0, 0 )
    = False
rule4 ( x1, x2, x3, x4, x5, x6 )
    = x1x2 `mod` x5x6 == 0 && x3x4 `mod` x5x6 == 0
    where
    x1x2 = 10 * x1 + x2
    x3x4 = 10 * x3 + x4
    x5x6 = 10 * x5 + x6


main :: IO()
main
    = putStrLn (show (rule4 (4,9,6,3,0,7)))
