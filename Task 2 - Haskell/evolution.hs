type Point
    = (Int, Int)

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

main :: IO()
main
    = putStrLn (show (neighbours (4, 4)))
