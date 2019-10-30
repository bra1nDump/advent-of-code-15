{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class

import Data.List as List
import Control.Monad as Monad
import Debug.Trace

super = filterM (\_ -> [True, False]) 

split :: (Eq a) => a -> [a] -> [[a]]
split x xs =
  case span (/= x) xs of
    (group, []) -> [ group ]
    (group, rest) -> group:split x rest

solve :: ([Integer] -> Integer) -> String -> String
solve rectangleScore = 
  lines >>> map parseRectangle >>> map rectangleScore >>> sum >>> show
  where 
    parseRectangle :: String -> [Integer]
    parseRectangle = 
      List.groupBy (\x y -> x /= 'x' && y /= 'x')
      >>> filter (/= "x") >>> map read

p1 :: String -> String
p1 = solve score
  where
    score sides = area sides + smallArea sides
    area = (* 2) . sum . map product . filter ((== 2) . length) . super
    smallArea = product . take 2 . sort

p2 :: String -> String 
p2 = solve score
  where
    score sides = minPerimeter sides + product sides
    minPerimeter = (* 2) . sum . take 2 . sort
