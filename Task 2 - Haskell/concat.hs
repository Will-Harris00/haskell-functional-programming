concatlist :: [[a]] -> [a]
concatlist []
    = []

concatlist (xs:xss)
    = xs ++ concatlist xss

main :: IO()
main
    -- = putStrLn (show (concatlist ([ [ [ [3,4], [5,6] ], [ [1,2], [7,8] ] ] ])))
    -- = putStrLn (show (concatlist ([ [ [3,4], [5,6] ] ])))
    -- = putStrLn (show (concatlist ([ [3,4], [5,6] ])))
    -- = putStrLn (show (concatlist ["AA", "BB", "CC"]))
    = putStrLn (show (concatlist [["AA", "BB", "CC"], ["DD", "EE", "FF"]]))
    -- remove outer level structure and concatenates
