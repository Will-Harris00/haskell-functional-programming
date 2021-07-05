countToN :: [(Int, Int, Int, Int, Int, Int)]
countToN
    = ([listToTuple . stringToList . padOut $ prnt])

prnt :: [Int]
prnt
    = [1..999999]

padOut :: Int -> String
padOut x
    = replicate (6 - length (show x)) '0' ++ show x

stringToList :: String -> [Int]
stringToList x
    = map (read . (:"")) x :: [Int]

listToTuple :: [Int] -> (Int, Int, Int, Int, Int, Int)
listToTuple [x1, x2, x3, x4, x5, x6]
    = (x1, x2, x3, x4, x5, x6)

main :: IO()
main
    = putStrLn (show (countToN))
    -- = putStrLn (show $ findSolutions $ possibles)
