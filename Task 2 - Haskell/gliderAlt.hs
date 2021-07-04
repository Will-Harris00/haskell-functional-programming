generateGrid :: Int -> Int -> String -> [ String ]
generateGrid width height 
    = replicate height . concat . replicate width

main :: IO()
main
    = putStrLn (show (generateGrid 5 5 "."))
