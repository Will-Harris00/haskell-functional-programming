type Point
    = (Int, Int)

glider :: [ Point ]
glider
    = [ (0, 2), (1, 3), (2, 1), (2, 2), (2, 3) ]

neighbours :: Point -> [ Point ]
neighbours (x, y)
    = map wrap [-- north
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

liveCell :: [ [ String ] ] -> Point -> Bool
liveCell gl (x, y)
    | (((gl !! 0) !! y) !! x) == '#' = True
    | otherwise = False

liveNeighbours :: [ Point ] -> Point -> Int
liveNeighbours gl p
    = length . filter (isAlive $ gl) $ neighbours p

isAlive :: [ Point ] -> Point -> Bool
isAlive gl p
    = elem p gl

main :: IO()
main
    -- = putStrLn (show (neighbours (4, 4)))
    -- = putStrLn (show (liveCell [[".....","..#..","#.#..",".##..","....."]] (0, 2)))
    = putStrLn (show (liveNeighbours glider $ (2,2)))
