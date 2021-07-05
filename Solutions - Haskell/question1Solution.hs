import Data.List ( nub )

rule1 :: (Int, Int, Int, Int, Int, Int) -> Bool
rule1 (x1, x2, x3, x4, x5, x6)
    = xs == nub xs
    where
        xs = [ x1, x2, x3, x4, x5, x6 ]


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


rule3 :: (Int, Int, Int, Int, Int, Int) -> Bool
rule3 (x1, x2, x3, x4, x5, x6)
    = abs( x1 - x2 ) > 2
    && abs( x2 - x3 ) > 2
    && abs( x3 - x4 ) > 2
    && abs( x4 - x5 ) > 2
    && abs( x5 - x6 ) > 2


rule4 :: (Int, Int, Int, Int, Int, Int) -> Bool
rule4 ( x1, x2, x3, x4, 0, 0 )
    = False
rule4 ( x1, x2, x3, x4, x5, x6 )
    = x1x2 `mod` x5x6 == 0 && x3x4 `mod` x5x6 == 0
    where
    x1x2 = 10 * x1 + x2
    x3x4 = 10 * x3 + x4
    x5x6 = 10 * x5 + x6


possibles :: [ (Int, Int, Int, Int, Int, Int) ]
possibles
    = concatMap (\a ->
        concatMap (\b ->
            concatMap (\c ->
                concatMap (\d ->
                    concatMap (\e ->
                        concatMap (\f -> [ (a, b, c, d, e, f) ])
                            xs)
                        xs)
                    xs)
                xs)
            xs)
        xs
        where xs = [0..9]


isSolution :: (Int, Int, Int, Int, Int, Int) -> Bool
isSolution t
    = rule1 t && rule2 t && rule3 t && rule4 t


findSolutions :: [(Int, Int, Int, Int, Int, Int)] -> [(Int, Int, Int, Int, Int, Int)]
findSolutions xs
    = filter isSolution xs


main :: IO()
main
    = putStrLn (show (findSolutions (possibles)))
