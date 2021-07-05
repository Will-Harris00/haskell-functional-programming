pretty :: [ [ [ Char ] ] ] -> [ Char ]
pretty
    = concatMap unlines


main :: IO()
main
    = putStrLn (show (pretty [ [ [ 'a','b' ], [ 'c','d' ] ],
                               [ [ 'e','f' ], [ 'g','h' ] ],
                               [ [ 'i','j' ], [ 'k','l' ] ] ]))
