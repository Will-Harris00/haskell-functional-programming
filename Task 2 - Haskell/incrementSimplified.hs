increment :: Int -> Int
increment n
    = n + 1

increments :: [ Int ] -> [ Int ]
increments []
    = []
increments (x:xs)
    = increment x : increments xs

main :: IO()
main
    = putStrLn (show (increments [1,2,3,4,5]))
