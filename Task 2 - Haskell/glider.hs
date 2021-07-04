{-
type Point
    = (Int, Int)

glider :: [ Point ]
glider
    = [ (0, 2), (1, 3), (2, 1), (2, 2), (2, 3) ]


grid :: Int -> Int -> [ [ String ] ]
grid x y
    = [replicate y $ (replicate x '.')]

visualisation :: Int -> Int -> [ Point ] -> [ [ String ] ]
visualisation x y g
    = plotpoints $ grid x y
    
plotpoints :: [ [ String ] ] -> [ Point ] -> [ [ String ] ]
plotpoints g (t:ts)
    = changeChar g t : plotpoints ts
-}

changeChar :: [[[a]]] -> (Int, Int) -> a
changeChar g t
    = (((g !! left t) !! 0) !! right t)

left :: (a,b) -> a
left (x,_) = x

right :: (a,b) -> b
right (_,y) = y

-- s = s[:index] + "#" + s[index + 1:]

main :: IO()
main
    = putStrLn (show (changeChar [["hello"]] (0,3)))
