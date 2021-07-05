type Point
    = ( Int, Int )

glider :: [ Point ]
glider
    = [ (0, 2), (1, 3), (2, 1), (2, 2), (2, 3) ]


visualisation :: Int -> Int -> [ [ Point ] ] -> [ [ [ Char ] ] ]
visualisation w h
    = map (visualise w h)

visualise :: Int -> Int -> [ Point ] -> [ [ Char ] ]
visualise w h gen
    = map (\y -> map (\x -> hashDot (isAlive gen (x,y))) [0..w]) [0..h]

isAlive :: [ Point ] -> Point -> Bool
isAlive pts pt
    = pt `elem` pts

hashDot :: Bool -> Char
hashDot True = '#'
hashDot False = '.'


main :: IO()
main
    = putStrLn (show (visualisation 5 5 [ glider ]))
