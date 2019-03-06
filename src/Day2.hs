{-# LANGUAGE OverloadedStrings #-}

module Day2 (day2, split) where

import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class

import Data.List as List
import Control.Monad as Monad

super =
  filterM (\_ -> [True, False]) 

split x =
  foldl (\(buf, acc) c ->
           if c == x then ([], (List.reverse buf):acc)
           else (c:buf, acc)) ([], [])
  >>> (\(x, xs) -> (List.reverse x):xs)
  >>> List.reverse

day2 input =
  let toI :: String -> [Integer]
      toI = map read . split 'x'
      i = map toI $ lines input
      p1 = sum . map (\sides -> area sides + smallArea sides) $ i
      p2 = sum . map (\sides -> minPerimiter sides + product sides) $ i
  in
    (show p1, show p2)
  where
    -- part 1
    area =
      (* 2) . sum . map product . filter ((== 2) . length)
      . super
    smallArea = product . take 2 . sort
    -- part 2
    minPerimiter = (* 2) . sum . take 2 . sort
