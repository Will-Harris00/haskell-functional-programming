average :: [Int] -> Int
average xs
    = let
        p = sum xs
        q = length xs
          -- q = length xs 
          -- The above is not permitted as the local definition
          -- is out of alignment so given syntax error.
    in
        p `div` q

main :: IO()
main
    = putStrLn (show (average [2,4,6]))

{-
The keyword let allows one to name a value, 
and to use it in an expression. 
It is though that in general 'let' 
corresponds to bottom-up programming.
Local definitions must be verticall aligned.
-}
