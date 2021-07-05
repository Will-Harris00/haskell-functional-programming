genGrid :: Int -> Int -> [ [String] ]
genGrid rows colums
    = [(take rows (repeat "....."))]

main :: IO()
main
    = putStrLn (show (genGrid 5 5))
