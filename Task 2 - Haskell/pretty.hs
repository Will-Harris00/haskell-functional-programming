import Data.List ( intersperse, intercalate )

prettyalt :: Show a => [[[a]]] -> String
prettyalt xs
    = (concat . intersperse "\n" . map show $ concat xs)

main :: IO()
main
    = putStrLn (show (prettyalt ([ [ [ 'a','b' ], [ 'c','d' ] ],
                                   [ [ 'e','f' ], [ 'g','h' ] ],
                                   [ [ 'i','j' ], [ 'k','l' ] ] ])))
