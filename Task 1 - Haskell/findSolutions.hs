{- rule one takes a six digit tuple and determines
whether all the digits it contains are unique -}
rule1 :: (Int,Int,Int,Int,Int,Int) -> Bool
rule1 (t1,t2,t3,t4,t5,t6) 
    = t1 /= t2 && t1 /= t3 && t1 /= t4 && t1 /= t5 && t1 /= t6
    && t2 /= t3 && t2 /= t4 && t2 /= t5 && t2 /= t6
    && t3 /= t4 && t3 /= t5 && t3 /= t6
    && t4 /= t5 && t4 /= t6
    && t5 /= t6


{- rule two determines if alternate digits
are odd and even or even and odd -}
rule2 :: (Int,Int,Int,Int,Int,Int) -> Bool
rule2 (t1,t2,t3,t4,t5,t6)
    = isAlternate (t1, t2) && isAlternate (t2, t3) && isAlternate (t3, t4) 
    && isAlternate (t4, t5) && isAlternate (t5, t6)

{- selects sets adjacent numbers within the
larger six digit tuple to check even/odd pairing -}
isAlternate :: (Int, Int) -> Bool
isAlternate (n, m)
    | even n && odd m = True
    | odd n && even m = True
    | otherwise = False


{- rule three determines if alternate
digits differ by more than two -}
rule3 :: (Int,Int,Int,Int,Int,Int) -> Bool
rule3 (t1,t2,t3,t4,t5,t6)
    = isDiffTwo (t1, t2) && isDiffTwo (t2, t3) && isDiffTwo (t3, t4) 
    && isDiffTwo (t4, t5) && isDiffTwo (t5, t6)

{- takes a pair of number and return true
if the difference is greater than two -}
isDiffTwo :: (Int, Int) -> Bool
isDiffTwo (n, m)
    | abs (n - m) > 2 = True
    | otherwise = False


{- rule four determines if the first and
middle pairs of digits form numbers that
are both multiples of the last -}
rule4 :: (Int,Int,Int,Int,Int,Int) -> Bool
rule4 (t1,t2,t3,t4,t5,t6)
    = convertInt (t1, t2, t5, t6) && convertInt (t3, t4, t5, t6)

{- takes two pairs of numbers and converts
each set into a number with tens and units -}
convertInt :: (Int, Int, Int, Int) -> Bool
convertInt (n, m, j, k)
    = isDividend((n * 10) + m,  (j * 10) + k)

{- checks if the final pair of numbers is a 
perfect divisor for the first and second pair
where the quotient leaves no remainder -}
isDividend :: (Int, Int) -> Bool
isDividend (x, y)
    | x `rem` y == 0 = True
    | otherwise = False


{- generates a list containing ever possible permutation
of six digit tuples used to find all the solutions -}
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


{- determines is any given six digit tuple is a valid
solution to the problem as it follows the four rules -}
isSolution :: (Int, Int, Int, Int, Int, Int) -> Bool
isSolution (t1,t2,t3,t4,t5,t6)
    | rule1 (t1,t2,t3,t4,t5,t6) && rule2 (t1,t2,t3,t4,t5,t6)  
    && rule3 (t1,t2,t3,t4,t5,t6) && rule4 (t1,t2,t3,t4,t5,t6) = True
    | otherwise = False


{- checks every six digit tuple previously generated and
filters these to find valid solutions to the problem -}
findSolutions :: [(Int, Int, Int, Int, Int, Int)] -> [(Int, Int, Int, Int, Int, Int)]
findSolutions xs
    = filter isSolution xs


main :: IO()
main
    = putStrLn (show (findSolutions (possibles)))
