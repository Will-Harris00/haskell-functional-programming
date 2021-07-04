import Data.List ( intercalate )

pretty :: Show a => [[[a]]] -> String
pretty xs
    -- = strip . (++ "\n") . intercalate "\n" . map show $ concat xs
    = (++ "\n") . strip . intercalate "\n" . map show $ concat xs

strip :: String -> String
strip xs
    = filter (not . (`elem` "\"")) xs

main :: IO()
main
    = putStrLn (show (pretty ([ [ [ 'a','b' ], [ 'c','d' ] ],
                                [ [ 'e','f' ], [ 'g','h' ] ],
                                [ [ 'i','j' ], [ 'k','l' ] ] ])))
