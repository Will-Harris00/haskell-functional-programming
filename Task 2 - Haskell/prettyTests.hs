import Data.List ( intercalate )

pretty :: Show a => [[[a]]] -> String
pretty xs
    = (intercalate "\n" . map show $ concat xs)

midstep :: Show a => [[[a]]] -> [String]
midstep xs
    = map show $ concat xs

firststep :: [[[a]]] -> [[a]]
firststep xs
    = concat xs

firststepalt :: [[a]] -> [a]
firststepalt xs
    = foldr (++) [] xs

prettyalt :: Show a => [[[a]]] -> String
prettyalt xs
    -- = strip . (++ "\n") . intercalate "\n" . map show $ concat xs
    = (++ "\n") . strip . intercalate "\n" . map show $ concat xs
    

strip :: String -> String
strip xs
    = filter (not . (`elem` "\"")) xs

main :: IO()
main
    {-= putStrLn (show (prettyalt ([ [ 'a','b' ], [ 'c','d' ] ],
                                   [ [ 'e','f' ], [ 'g','h' ] ],
                                   [ [ 'i','j' ], [ 'k','l' ] ])))
    -}
    {-= putStrLn (show (prettyalt ([ [ [ 'a','b' ], [ 'c','d' ] ],
                                     [ [ 'e','f' ], [ 'g','h' ] ],
                                     [ [ 'i','j' ], [ 'k','l' ] ] ])))
    -}
    {-= putStrLn (show (prettyalt ([ [ [ 'a','b','c' ], [ 'c','d' ] ],
                                     [ [ 'e','f' ], [ 'h' ] ],
                                     [ [ 'i','j' ], [ 'k','l' ] ] ])))
    -}
    {-
    = putStrLn (show (prettyalt ([ [ [ 'a','b' ], [ 'c','d' ] ],
                                   [ [ 'e','f' ], [ 'g','h' ] ],
                                   [ [ 'i','j' ], [ 'k','l' ] ],
                                   [ [ 'm','n' ], [ 'o','p' ] ],
                                   [ [ 'q','r' ], [ 's','t' ] ],
                                   [ [ 'u','v' ], [ 'w','x' ] ],
                                   [ [ 'y','z' ], [ 'A','B' ] ] ])))
    -}
    = putStrLn (show (prettyalt ([ [ [ 'a','b','Z','X','Y' ], [ 'c','d' ], [ 'z','a', 'W', 'V', 'U' ] ],
                                   [ [ 'e','f' ], [ 'g','h' ] ],
                                   [ [ 'i','j' ], [ 'k','l' ] ],
                                   [ [ 'm','n' ], [ 'o','p' ] ],
                                   [ [ 'q','r' ], [ 's','t' ] ],
                                   [ [ 'u','v' ], [ 'w','x' ] ],
                                   [ [ 'y','z' ], [ 'A','B' ] ] ])))
