type Point
    = (Int, Int)

glider :: [ Point ]
glider
    = [ (0, 2), (1, 3), (2, 1), (2, 2), (2, 3) ]

generateGrid :: Int -> Int -> String -> [ String ]
generateGrid width height 
    = replicate height . concat . replicate width

placeHash :: Point -> [String] -> [String]
placeHash (x, y) zs
    = changeItemAt zs y repl
    where
        repl = changeItemAt (zs!!y) x '#'

changeItemAt :: [a] -> Int -> a -> [a]
changeItemAt xs i s
    = h ++ [s] ++ (tail t)
    where
        (h, t) = splitAt i xs

forListOfPoints :: [String] -> [Point] -> [String]
forListOfPoints a []
    = a
forListOfPoints gr (x:xs)
    = forListOfPoints replaced xs
    where 
        replaced = placeHash x gr

visualisation :: Int -> Int -> [ [ Point ] ] -> [ [ String ] ]
visualisation w h gs
    = map (forListOfPoints grid) gs
    where
        grid = generateGrid w h "."

main :: IO()
main
    = putStrLn (show (visualisation 5 5 [glider]))
