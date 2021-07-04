import Data.List ( intercalate )

{- defines a data type point which 
consists of an integer tuple -}
type Point
    = (Int, Int)

{- defines the initial generation of cells 
for the glider pattern in terms of a 
list of points of alive cells -}
glider :: [ Point ]
glider
    = [ (0, 2), (1, 3), (2, 1), (2, 2), (2, 3) ]

{- defines a constant for 
the width of the cell grid -}
width :: Int
width = 5

{- defines a constant for 
the height of the cell grid -}
height :: Int
height = 5



{-*** Question 2.1 ***-}
{- takes a nested list of strings and 
separates the individual elements by
spliting each with newline characters 
and concatenating the structure -}
pretty :: Show a => [[[a]]] -> String
pretty xs
    -- = strip . (++ "\n") . intercalate "\n" . map show $ concat xs
    = (++ "\n") . strip . intercalate "\n" . map show $ concat xs

{- removes any escape characters 
and additional quotation marks -}
strip :: String -> String
strip xs
    = filter (not . (`elem` "\"")) xs


{-*** Question 2.2 ***-}
{- creates a visual representation 
of the game given the glider pattern 
and the width and height of the grid -}
visualisation :: Int -> Int -> [ [ Point ] ] -> [ [ String ] ]
visualisation w h seq
    = map (plotPoints grid) seq
    where
        grid = createGrid w h

{- plots each of the alive cell within the grid
represented by a hashtag according to the coordinates
given in the list of points within the glider pattern -}
plotPoints :: [ String ] -> [ Point ] -> [ String ]
plotPoints a []
    = a
plotPoints gr (x:xs)
    = plotPoints replace xs
    where 
        replace = positionHash x gr

{- generates the grid according
to the width and height where dead 
cells are represented by a period -}
createGrid :: Int -> Int -> [ String ]
createGrid width height 
    = replicate width $ (replicate height '.')

{- indexes the list of strings to select a
single row in the grid for which the string
replacement is due to occur then concatenates
the new string back together with the list -}
positionHash :: Point -> [ String ] -> [ String ]
positionHash (x, y) zs
    = (take y zs) ++ [switchSymbol] ++ (drop (y + 1) zs)
    where
        switchSymbol = changeChar (zs !! y) x

{- takes the indexed string and replaces 
a particular column with a hashtag which
represents an alive cell in the grid -}
changeChar :: String -> Int -> String
changeChar s i
    = (take i s) ++ "#" ++ (drop (i + 1) s)



{-*** Question 2.3 ***-}
{- takes the inital glider pattern and 
determines the future generations of cells 
according to the rules of survival and growth -}
evolution :: [ Point ] -> [ [ Point ] ]
evolution gl
    = iterate generations gl

{- find the next generation of alive cells
using the points from the previous one -}
generations :: [ Point ] -> [ Point ]
generations gl
    = persists gl ++ creation gl

{- determines for a given point the 
coordinate of the surrounding 
eight neighbouring cells -}
neighbours :: Point -> [ Point ]
neighbours (x, y)
    {- the edges of the grid are wrapped to
    increase the chances of cell survival -}
    = map wrap [
        -- north
        (x, y-1),
        -- north-east
        (x+1,y-1),
        -- east
        (x+1, y),
        -- south-east
        (x+1, y+1),
        -- south
        (x, y+1),
        -- south-west
        (x-1, y+1),
        -- west
        (x-1, y),
        -- north-west
        (x-1, y-1)]

{- ensures no cell can be places outside the
border of the grid by wrapping the edges -}
wrap :: Point -> Point
wrap (x, y) = ((x `mod` width), (y `mod` height))

{- of the living cells determines 
which survive in the next generation
for those cells that have exactly 
two or three living neighbours -}
persists :: [ Point ] -> [ Point ]
persists gl
    = [w | w <- gl, elem (liveNeighbours gl w) [2,3]]




{- This is an alternative method for determining which cells
are created, I have left it in as it is more readable thought
also less efficient as requires every point to be checked -}

{- determines for every point those
that have exactly three live neighbours
and so are created in the next generation -}

{-
creation :: [ Point ] -> [ Point ]
creation gl
    = [(x,y) | x <- [0..(width-1)],
               y <- [0..(height-1)],
               isDead gl (x,y),
               liveNeighbours gl (x,y) == 3]
-}




{- This improved method considers only those points that have
alive neighbouring cells, so would be more efficient for larger
grids over many generation, though it has slightly poorer readablity -}

{- filters points with live neighbours to determine
those that have exactly three live neighbours
and so are created in the next generation -}
creation :: [ Point ] -> [ Point ]
creation gl
    = [w | w <- rmduplicates(concat (map neighbours gl)),
            isDead gl w,
            liveNeighbours gl w == 3]

{- supplementary function to remove duplicate points
from the list containing points with live neighbours -}
rmduplicates :: Eq a => [a] -> [a]
rmduplicates [] 
    = []
rmduplicates (x:xs)
    = x : rmduplicates (filter (/= x) xs)

{- calculates for a given point the number
of neighbouring cells which are alive -}
liveNeighbours :: [ Point ] -> Point -> Int
liveNeighbours gl p
    = length . filter (isAlive $ gl) $ neighbours p

{- checks for a given point if this 
contains a live cell returning true -}
isAlive :: [ Point ] -> Point -> Bool
isAlive gl p
    = elem p gl

{- for a given point returns 
false if the cell is not alive -}
isDead :: [ Point ] -> Point -> Bool
isDead gl p
    = not (isAlive gl p)


main :: IO()
main
    = putStrLn (pretty (take 8 (visualisation width height (evolution glider))))

    {- Tests for the functions contained within the pretty method -}
    {- = putStrLn (pretty ([ [".....","..#..","#.#..",".##..","....."],
                                [".....",".#...","..##.",".##..","....."],
                                [".....","..#..","...#.",".###.","....."],
                                [".....",".....",".#.#.","..##.","..#.."],
                                [".....",".....","...#.",".#.#.","..##."],
                                [".....",".....","..#..","...##","..##."],
                                [".....",".....","...#.","....#","..###"] ]))
    -}
    {-= putStrLn (pretty ([ [ [ 'a','b' ], [ 'c','d' ] ],
                                  [ [ 'e','f' ], [ 'g','h' ] ],
                                  [ [ 'i','j' ], [ 'k','l' ] ] ]))
    -} 

    {- Tests for the functions contained within the glider method -}
    -- = putStrLn (show (visualisation 5 5 [glider]))

    {- Tests for the functions contained within the evolution method -}
    -- = putStrLn (show (neighbours (4,4)))
    -- = putStrLn (show (liveNeighbours glider $ (2,2)))
    -- = putStrLn (show (creation glider))
    -- = putStrLn (show (persists glider))
    -- = putStrLn (show (generations glider))
    -- = putStrLn (show (take 8 (evolution glider)))
    -- = putStrLn (show (take 8 (visualisation width height (evolution glider))))
