import Data.List ( nub )

rule1 :: (Int, Int, Int, Int, Int, Int) -> Bool
rule1 (x1, x2, x3, x4, x5, x6)
    = xs == nub xs
    where
        xs = [ x1, x2, x3, x4, x5, x6 ]


main :: IO()
main
    = putStrLn (show (rule1 (1,2,3,4,5,6)))
