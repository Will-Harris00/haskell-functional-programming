concatlist :: [[a]] -> [a]
concatlist xss
    = foldr (++) [] xss

main :: IO()
main
    -- = putStrLn (show (concatlist ["AA", "BB", "CC"]))
    = putStrLn (show (concatlist [ [ 1, 2 ], [ 3 ], [ 4 ] ]))
    -- remove outer level structure and concatenates
