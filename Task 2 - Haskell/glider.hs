type Point
    = (Int, Int)

glider :: [ Point ]
glider
    = [ (0, 2), (1, 3), (2, 1), (2, 2), (2, 3) ]

createGrid :: Int -> Int -> [ String ]
createGrid width height 
    = replicate width $ (replicate height '.')

positionHash :: Point -> [ String ] -> [ String ]
positionHash (x, y) zs
    = take y zs ++ [zs !! y] ++ drop (y + 1) zs

plotPoints :: [ String ] -> [ Point ] -> [ String ]
plotPoints a []
    = a
plotPoints gr (x:xs)
    = plotPoints replace xs
    where 
        replace = positionHash x gr

visualisation :: Int -> Int -> [ [ Point ] ] -> [ [ String ] ]
visualisation w h seq
    = map (plotPoints grid) seq
    where
        grid = createGrid w h

main :: IO()
main
    = putStrLn (show (visualisation 5 5 [glider]))

{-
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
