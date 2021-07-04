import Data.List ( intersperse, intercalate )

prettyalt :: Show a => [[[a]]] -> String
prettyalt xs
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

main :: IO()
main
    = putStrLn (show (firststepalt ([ [ [ 'a','b' ], [ 'c','d' ] ],
                                      [ [ 'e','f' ], [ 'g','h' ] ],
                                      [ [ 'i','j' ], [ 'k','l' ] ] ])))
    {-= putStrLn (show (firststep ([ [ [ 'a','b' ], [ 'c','d' ] ],
                                     [ [ 'e','f' ], [ 'g','h' ] ],
                                     [ [ 'i','j' ], [ 'k','l' ] ] ])))
    -}
    {-= putStrLn (show (midstep ([ [ [ 'a','b' ], [ 'c','d' ] ],
                                   [ [ 'e','f' ], [ 'g','h' ] ],
                                   [ [ 'i','j' ], [ 'k','l' ] ] ])))
    -}
    {-= putStrLn (show (pretty ([ [ [ 'a','b' ], [ 'c','d' ] ],
                                  [ [ 'e','f' ], [ 'g','h' ] ],
                                  [ [ 'i','j' ], [ 'k','l' ] ] ])))
    -}
