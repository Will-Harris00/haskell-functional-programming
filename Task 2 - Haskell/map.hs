mapping :: (a -> b) -> [ a ] -> [ b ]
mapping f []
    = []
mapping f (x:xs)
    = f x : mapping f xs

-- map is a generic higher-order function 
-- build in to the haskell standard prelude
-- takes a function and applies this mapping to a list of integers
-- from list 'a' and returns a list 'b' of updated figues
