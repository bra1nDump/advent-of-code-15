{-# LANGUAGE 
    OverloadedStrings
  , LambdaCase 
#-}

module Day3 where

import Common
import Control.Arrow
import Control.Monad as Monad
import Control.Monad.Trans.State

import Data.Set as Set

type House = (Int, Int)

move :: House -> Char -> House
move (x, y) =
  \case
    '^' -> (x, y + 1)
    'v' -> (x, y - 1)
    '>' -> (x + 1, y)
    '<' -> (x - 1, y)

walk :: [Char] -> House -> State (Set House) ()
walk [] house =
  modify (Set.insert house)
walk (direction:rest) house = do
  modify (Set.insert house)
  walk rest (move house direction)

visited :: [Char] -> Set House
visited directions = execState (walk directions (0, 0)) Set.empty

p1 :: String -> String 
p1 input = visited input |> size |> show

p2 :: String -> String
p2 input = 
  union 
    (visited (even input))
    (visited (odd input)) 
  |> size |> show
  where
    odd = tail >>> even
    even [] = []
    even [x] = [x]
    even (x:_:xs) = even xs
        