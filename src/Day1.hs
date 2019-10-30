module Day1 where

import Common
import qualified Data.List as List

p1 :: String -> String
p1 input = show $ (count '(') - (count ')')
  where count c = length . filter (c ==) $ input
      
p2 :: String -> String
p2 directions = show . List.elemIndex (-1) $ floorsVisited
  where 
    floorsVisited = 
      scanl 
        (\floor direction -> 
          if direction == '(' 
          then floor - 1 else floor + 1
        )
        0 directions
