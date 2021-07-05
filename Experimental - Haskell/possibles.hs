countToN :: Int -> [Int] -> [Int]
countToN x xs
    | x /= 1000 = countToN (succ x) (xs ++ [x])
    | otherwise = xs

main :: IO()
main
    = putStrLn (show (countToN 0 []))
