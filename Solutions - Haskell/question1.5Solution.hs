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


main :: IO()
main
    = putStrLn (show (possibles))
