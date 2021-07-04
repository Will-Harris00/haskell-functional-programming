type Point
    = (Int, Int)

glider :: [ Point ]
glider
    = [ (0, 2), (1, 3), (2, 1), (2, 2), (2, 3) ]

visualisation :: Int -> Int -> [ [ String ] ]
visualisation x y
    = [replicate y $ (replicate x '.')]

main :: IO()
main
    = putStrLn (show (visualisation 5 5))
