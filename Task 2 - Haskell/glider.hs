type Point
    = (Int, Int)

glider :: [ Point ]
glider
    = [ (0, 2), (1, 3), (2, 1), (2, 2), (2, 3) ]

grid :: Int -> Int -> [ [ String ] ]
grid x y
    = [replicate y $ (replicate x '.')]

visualisation :: Int -> Int -> [ [ Point ] ] -> [ String ]
visualisation x y g
    = plotpoints (grid x y) g

plotpoints :: [ [ String ] ] -> [ [ Point ] ] -> [ String ]
plotpoints g p
    = magic (concat g) (concat p)

magic :: [ String ] -> [ Point ] -> [ String ]
magic g p 
    = g

split :: Point -> Int
split (a,b)
    = a + b

{-
isAlive :: [ [ String ] ] -> Point -> Bool
isAlive gr (x, y)
    | (((gr !! 0) !! y) !! x) == '#' = True
    | otherwise = False

separateTuple :: [ [ Point ] ] -> Point
separateTuple (xs:xss)
    = split xs : increments xss

selectStr :: [ Point ] -> [ String ] -> String
selectStr p g
    = changeChar (g !! left t) (right t)

changeChar :: String -> Int -> String
changeChar s i
    = (take i s) ++ "#" ++ (drop i s)
-}

main :: IO()
main
    = putStrLn (show (visualisation 5 5 [ glider ]))
