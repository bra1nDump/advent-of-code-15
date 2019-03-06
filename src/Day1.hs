module Day1 (day1) where

import Data.List

day1 :: String -> (String, String)
day1 input =
  (p1, p2)
  where
    count c = length . filter (c ==) $ input
    p1 = show $ (count '(') - (count ')')
    p2 =
      let folder (Nothing, a) (x, i) =
            let a' =
                  if x == '(' then a + 1
                  else a - 1
            in (if a' == -1 then Just i else Nothing, a')
          folder x@(Just i, a) _ = x
      in
        show $
        foldl folder
        (Nothing, 0)
        (zip input [1..])
