import Data.List ( intercalate, intersperse )


pretty :: [ [ [ Char ] ] ] -> [ [ Char ] ]
pretty []  = []
pretty ([] :xss) 
    = pretty xss
pretty ((x:xs):xss)
    -- = intersperse "\n" . map show $ xs
    = [increments xs]

increments :: [[Char]] -> [Char]
increments xs
    = intercalate "\n" xs

alt :: Show a => [a] -> String
alt s
    = strip . intercalate "\n" . map show $ s

prettyalt :: Show a => [[[a]]] -> String
prettyalt xs
    -- = intercalate "\n" . map show $ xs
    -- = intercalate "\n" . map show $ concat xs
    -- = (++ "\n") . strip . intercalate "\n" . map show $ xs
    -- = (++ "\n") . strip . intercalate "\n" . map show $ concat xs
    = (intercalate "\n" . map show $ xs)

strip :: String -> String
strip xs
    = filter (not . (`elem` "\"")) xs

main :: IO()
main
    = putStrLn (show (pretty ([ [".....","..#..","#.#..",".##..","....."],
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
