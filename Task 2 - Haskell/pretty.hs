import Data.List ( intersperse, intercalate )

prettyalt :: Show a => [[[a]]] -> String
prettyalt xs
    = (intercalate "\n" . map show $ concat xs)

midstep :: Show a => [[[a]]] -> [String]
midstep xs
    = map show $ concat xs

main :: IO()
main
    = putStrLn (show (prettyalt ([ [ [ 'a','b' ], [ 'c','d' ] ],
                                   [ [ 'e','f' ], [ 'g','h' ] ],
                                   [ [ 'i','j' ], [ 'k','l' ] ] ])))
    {-= putStrLn (show (midstep ([ [ [ 'a','b' ], [ 'c','d' ] ],
                                   [ [ 'e','f' ], [ 'g','h' ] ],
                                   [ [ 'i','j' ], [ 'k','l' ] ] ])))
    -}
