countToN :: [String]
countToN
    = listInt $ prnt

prnt :: [Int]
prnt
    = [1..999999]

padOut :: Int -> String
padOut x
    = replicate (6 - length (show x)) '0' ++ show x

listInt :: [Int] -> [String]
listInt (x:xs)
    = padOut x : listInt xs

stringToList :: String -> [Int]
stringToList x
    = map (read . (:"")) x :: [Int]

listToTuple :: [Int] -> (Int, Int, Int, Int, Int, Int)
listToTuple [x1, x2, x3, x4, x5, x6]
    = (x1, x2, x3, x4, x5, x6)

main :: IO()
main
    -- = putStrLn (show (prnt))
    = putStrLn (show (countToN))
    -- = putStrLn (show $ findSolutions $ possibles)
