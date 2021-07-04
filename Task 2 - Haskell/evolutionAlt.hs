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

neighbs :: Int -> Int -> [(Int, Int)]
neighbs x y =
    do
        x <- [x, x-1, x+1]
        y <- [y, y-1, y+1]
        return (x,y)

wrap :: Point -> Point
wrap (x, y) = ((x `mod` 5), (y `mod` 5))

persists :: [ Point ] -> [ Point ]
persists gl
    = [p | p <- gl, elem (liveNeighbours gl p) [2,3]]

creation :: [ Point ] -> [ Point ]
creation gl
    = [(x,y) | x <- [1..5],
               y <- [1..5],
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

generations :: [ Point ] -> [ Point ]
generations gl
    = persists gl ++ creation gl

evolution :: [ Point ] -> [ [ Point ] ]
evolution gl
    = iterate generations gl

main :: IO()
main
    -- = putStrLn (show (neighbours (4, 4)))
    -- = putStrLn (show (liveNeighbours glider $ (2,2)))
    -- = putStrLn (show (creation glider))
    -- = putStrLn (show (persists glider))
    -- = putStrLn (show (generations glider))
    -- = putStrLn (show (take 8 (evolution glider)))
    = putStrLn (show (neighbs 4 4))
