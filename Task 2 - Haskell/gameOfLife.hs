import Data.List ( intercalate )

type Point
    = (Int, Int)

glider :: [ Point ]
glider
    = [ (0, 2), (1, 3), (2, 1), (2, 2), (2, 3) ]




pretty :: Show a => [[[a]]] -> String
pretty xs
    -- = strip . (++ "\n") . intercalate "\n" . map show $ concat xs
    = (++ "\n") . strip . intercalate "\n" . map show $ concat xs

strip :: String -> String
strip xs
    = filter (not . (`elem` "\"")) xs


visualisation :: Int -> Int -> [ [ Point ] ] -> [ [ String ] ]
visualisation w h seq
    = map (plotPoints grid) seq
    where
        grid = createGrid w h

plotPoints :: [ String ] -> [ Point ] -> [ String ]
plotPoints a []
    = a
plotPoints gr (x:xs)
    = plotPoints replace xs
    where 
        replace = positionHash x gr

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



evolution :: [ Point ] -> [ [ Point ] ]
evolution gl
    = iterate generations gl

generations :: [ Point ] -> [ Point ]
generations gl
    = persists gl ++ creation gl

neighbours :: Point -> [ Point ]
neighbours (x, y)
    = map wrap [
                -- north
                (x, y-1),
                -- north-east
                (x+1,y-1),
                -- east
                (x+1, y),
                -- south-east
                (x+1, y+1),
                -- south
                (x, y+1),
                -- south-west
                (x-1, y+1),
                -- west
                (x-1, y),
                -- north-west
                (x-1, y-1)]

wrap :: Point -> Point
wrap (x, y) = ((x `mod` 5), (y `mod` 5))

persists :: [ Point ] -> [ Point ]
persists gl
    = [w | w <- gl, elem (liveNeighbours gl w) [2,3]]

creation :: [ Point ] -> [ Point ]
creation gl
    = [(x,y) | x <- [0..4],
               y <- [0..4],
               isDead gl (x,y),
               liveNeighbours gl (x,y) == 3]

liveNeighbours :: [ Point ] -> Point -> Int
liveNeighbours gl p
    = length . filter (isAlive $ gl) $ neighbours p

isAlive :: [ Point ] -> Point -> Bool
isAlive gl p
    = elem p gl

isDead :: [ Point ] -> Point -> Bool
isDead gl p
    = not (isAlive gl p)


main :: IO()
main
    -- = putStrLn (show (neighbours (4,4)))
    -- = putStrLn (show (liveNeighbours glider $ (2,2)))
    -- = putStrLn (show (creation glider))
    -- = putStrLn (show (persists glider))
    -- = putStrLn (show (generations glider))
    -- = putStrLn (show (take 8 (evolution glider)))
    -- = putStrLn (show (take 8 (visualisation 5 5 (evolution glider))))
    = putStrLn (pretty (take 8 (visualisation 5 5 (evolution glider))))
