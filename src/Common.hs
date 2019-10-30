module Common 
    ( module Common
    , module Debug.Trace
    )
where 

import Debug.Trace

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
-- x |> f = f x

(<|) :: (a -> b) -> a -> b
(<|) = ($)

none :: (Foldable t) => (a -> Bool) -> t a -> Bool
none p = all (not . p)

allFalse :: (Foldable t) => t Bool -> Bool
allFalse = none id

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f as bs = zip as bs |> map (uncurry f)

zip2 :: [a] -> [b] -> [(a, b)]
zip2 = map2 (\a b -> (a, b))