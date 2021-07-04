type Point
    = (Int, Int)

glider :: [ Point ]
glider
    = [ (0, 2), (1, 3), (2, 1), (2, 2), (2, 3) ]

grid :: Int -> Int -> [ [ String ] ]
grid x y
    = [replicate y $ (replicate x '.')]

visualisation :: Int -> Int -> [ [ Point ] ] -> [ [ String ] ]
visualisation x y g
    = plotpoints (grid x y) g

plotpoints :: [ [ String ] ] -> [ [ Point ] ] -> [ [ String ] ]
plotpoints g p
    = selectStr g p

selectStr :: [ [ String ] ] -> [ [ Point ] ] -> String
selectStr g t
    = changeChar (g !! left p) (right p)

changeChar :: String -> Int -> String
changeChar s i
    = (take i s) ++ "#" ++ (drop i s)

left :: (a,b) -> a
left (x,_) = x

right :: (a,b) -> b
right (_,y) = y

main :: IO()
main
    = putStrLn (show (visualisation 5 5 [ glider ]))
