{-# LANGUAGE OverloadedStrings #-}

module Day3 (day3) where

import Control.Arrow
import Control.Monad

import Data.Map
import Data.List as List
import Control.Monad as Monad

next (x, y) m =
  case m of
  '^' -> (x, y + 1)
  'v' -> (x, y - 1)
  '>' -> (x + 1, y)
  '<' -> (x - 1, y)

day3 input =
      -- part 1
  let p1 :: String
      p1 = show . size $ walk empty input
      -- part 2
      santa = walk empty (mod2 0 input)
      roboSanta = walk santa (mod2 1 input)
  in (p1, show . size $ roboSanta)
  where mod2 p = List.map snd . List.filter ((==) p . (flip mod) 2 . fst) . zip [1..]
        walk init =
          List.foldl
          (\(xy, grid) m -> 
             let n = next xy m
                 g = insertWith (\_ -> (+) 1) n 1 grid
             in (n, g))
          ((0,0), init)
          >>> snd
