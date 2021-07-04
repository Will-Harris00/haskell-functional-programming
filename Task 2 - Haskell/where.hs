average :: [Int] -> Int
average xs
    = p `div` q
        where
        p = sum xs
        q = length xs
          -- q = length xs 
          -- The above is not permitted as the local definition
          -- is out of alignment so given syntax error.

main :: IO()
main
    = putStrLn (show (average [2,4,6]))

{-
The keyword where allows one to name a value, 
and to use it in an expression.
It is though that in general 'where' 
corresponds to top-down programming.
Local definitions must be vertically aligned.
-}
