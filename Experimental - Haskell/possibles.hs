countToN :: [Int]
countToN
    = listInt $ prnt
    -- = [listToTuple . listString . listInt $ prnt]

prnt :: [Int]
prnt
    = [0..999999]

listInt :: [Int] -> [Int]
listInt (x:xs)
    = magic x : listInt xs

magic :: Int -> [Int]
magic x
    = stringToList . padOut $ x

padOut :: Int -> String
padOut x
    = replicate (6 - length (show x)) '0' ++ show x

stringToList :: String -> [Int]
stringToList x
    = map (read . (:"")) x :: [Int]

listToTuple :: [Int] -> (Int, Int, Int, Int, Int, Int)
listToTuple [x1, x2, x3, x4, x5, x6]
    = (x1, x2, x3, x4, x5, x6)

{-
listString :: [String] -> [Int]
listString (x:xs)
    = testString x : listString xs

testString :: String -> [Int]
testString x
    = map (read . (:"")) x :: [Int]
-}

main :: IO()
main
    -- = putStrLn (show (prnt))
    = putStrLn (show (countToN))
    -- = putStrLn (show (testString ["999999"]))
    -- = putStrLn (show $ findSolutions $ possibles)
