rule2 :: (Int, Int, Int, Int, Int, Int) -> Bool
rule2 ( x1, x2, x3, x4, x5, x6 )
    = ( evens [ x1, x3, x5 ] && odds [ x2, x4, x6 ] )
    || ( odds [ x1, x3, x5 ] && evens [ x2, x4, x6 ] )

evens :: [ Int ] -> Bool
evens
    = all even
odds :: [ Int ] -> Bool
odds
    = all odd


main :: IO()
main
    = putStrLn (show (rule2 (1,2,3,4,5,6)))
