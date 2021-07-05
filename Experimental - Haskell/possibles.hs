countToN :: Int -> [Int] -> [(Int,Int,Int,Int,Int,Int)] -> [(Int,Int,Int,Int,Int,Int)]
countToN x xs ys
    | x /= 9999 = countToN (succ x) (xs ++ [x]) $ ys ++ [listToTuple . stringToList . padOut . succ $ x]
    | otherwise = ys

intToString :: Int -> String
intToString x 
    = show x

stringToInt :: String -> Int
stringToInt x
    = read x :: Int

padOut :: Int -> String
padOut x
    = replicate (6 - length (intToString x)) '0' ++ intToString x

stringToList :: String -> [Int]
stringToList x
    = map (read . (:"")) x :: [Int]

listToTuple :: [Int] -> (Int,Int,Int,Int,Int,Int)
listToTuple [t1,t2,t3,t4,t5,t6]
    = (t1,t2,t3,t4,t5,t6)

main :: IO()
main
    = putStrLn (show (countToN (-1) [] []))
