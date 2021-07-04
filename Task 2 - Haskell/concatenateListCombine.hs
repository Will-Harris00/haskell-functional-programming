concatlist :: [[a]] -> [a]
concatlist []
    = []
concatlist (xs:xss)
    = combine xs (concatlist xss)

combine :: [a] -> [a] -> [a]
combine [] ys
    = ys
combine (x:xs) ys
    = x : combine xs ys

main :: IO()
main
    = putStrLn (show (concatlist [ [ 1, 2 ], [ 3 ], [ 4 ] ]))
    -- remove outer level structure and concatenates
