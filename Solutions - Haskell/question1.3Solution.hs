rule3 :: (Int, Int, Int, Int, Int, Int) -> Bool
rule3 (x1, x2, x3, x4, x5, x6)
    = abs( x1 - x2 ) > 2
    && abs( x2 - x3 ) > 2
    && abs( x3 - x4 ) > 2
    && abs( x4 - x5 ) > 2
    && abs( x5 - x6 ) > 2


main :: IO()
main
    = putStrLn (show (rule3 (4,9,6,3,0,7)))
