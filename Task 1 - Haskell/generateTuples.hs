possibles :: [(Int, Int, Int, Int, Int, Int)]
possibles 
    = do
    u <- [0..9]
    v <- [0..9]
    w <- [0..9]
    x <- [0..9]
    y <- [0..9]
    z <- [0..9]
    return (u, v, w, x, y, z)

main :: IO()
main
    = putStrLn (show (possibles))
