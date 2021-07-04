funclist :: [a -> b] -> a -> [b]
funclist [] e
    = []

funclist (f:fs) e
    = f e : funclist fs e

increment :: Int -> Int
increment n
    = n + 1

increments :: [ Int ] -> [ Int ]
increments []
    = []
increments (x:xs)
    = increment x : increments xs

square :: Int -> Int
square n
    = n * n

squares :: [Int] -> [Int]
squares []
    = []
squares (x:xs)
    = square x : squares xs

main :: IO()
main
    = putStrLn (show (funclist [increments, squares] [8,9,10]))
