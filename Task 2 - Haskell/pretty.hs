import Data.List ( intercalate, intersperse )

pretty :: Show a => [[[a]]] -> [ String ]
pretty xs
    = intersperse "\n" . map show $ xs

prettyalt :: Show a => [[[a]]] -> String
prettyalt xs
    -- = intercalate "\n" . map show $ xs
    -- = intercalate "\n" . map show $ concat xs
    -- = (++ "\n") . strip . intercalate "\n" . map show $ xs
    -- = (++ "\n") . strip . intercalate "\n" . map show $ concat xs
    = intercalate "\n" . map show $ (intercalate "\n" . map show $ xs)

strip :: String -> String
strip xs
    = filter (not . (`elem` "\"")) xs

main :: IO()
main
    = putStrLn (show (prettyalt ([ [".....","..#..","#.#..",".##..","....."],
                                   [".....",".#...","..##.",".##..","....."],
                                   [".....","..#..","...#.",".###.","....."],
                                   [".....",".....",".#.#.","..##.","..#.."],
                                   [".....",".....","...#.",".#.#.","..##."],
                                   [".....",".....","..#..","...##","..##."],
                                   [".....",".....","...#.","....#","..###"] ])))
    {-= putStrLn (show (pretty ([ [ [ 'a','b' ], [ 'c','d' ] ],
                                  [ [ 'e','f' ], [ 'g','h' ] ],
                                  [ [ 'i','j' ], [ 'k','l' ] ] ])))
    -}
