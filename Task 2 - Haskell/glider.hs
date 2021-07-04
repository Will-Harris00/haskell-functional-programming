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
    = (take y zs) ++ [switchSymbol] ++ (drop (y + 1) zs)
    where
        switchSymbol = changeChar (zs !! y) x

changeChar :: String -> Int -> String
changeChar s i
    = (take i s) ++ "#" ++ (drop (i + 1) s)

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
