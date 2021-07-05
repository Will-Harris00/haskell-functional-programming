countToN :: Int -> [(Int, Int, Int, Int, Int, Int)] -> [(Int, Int, Int, Int, Int, Int)]
countToN x ys
    | x /= 9999 = countToN (succ x) (ys ++ [listToTuple . stringToList . padOut . succ $ x])
    | otherwise = ys

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
    = putStrLn (show (countToN (-1) []))
