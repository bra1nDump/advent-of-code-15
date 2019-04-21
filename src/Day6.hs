{-# LANGUAGE ViewPatterns, LambdaCase #-}

module Day6 (day6, parseInstruction) where

import Data.Monoid -- Sum monoid
import Data.List as L
import Data.Array as A
import Control.Monad.Trans.State.Lazy (State, evalState, get, put)
import Data.Foldable

import Debug.Trace

day6 input = (p1 input, p2 input)

type Point = (Int, Int)

parseInstruction :: State String (Bool -> Bool, (Point, Point))
parseInstruction = do
  f <- parseF
  rect <- parseRect
  return (f, rect)

parseF = do
  (f, rest) <- fmap parse $ get
  put rest
  return f
  where
    parse (L.stripPrefix "turn on " -> Just rest) = (const True, rest)
    parse (L.stripPrefix "turn off " -> Just rest) = (const False, rest)
    parse (L.stripPrefix "toggle " -> Just rest) = (not, rest)
    parse _ = undefined

parseInstruction2 :: State String (Integer -> Integer, (Point, Point))
parseInstruction2 = do
  f <- parseF2
  rect <- parseRect
  return (f, rect)

parseF2 :: State String (Integer -> Integer)
parseF2 = do
  (f, rest) <- fmap parse $ get
  put rest
  return f
  where
    parse (L.stripPrefix "turn on " -> Just rest) = ((+) 1, rest)
    parse (L.stripPrefix "turn off " -> Just rest) = ((-) 1, rest)
    parse (L.stripPrefix "toggle " -> Just rest) = ((+) 2, rest)
    parse _ = undefined


parseRect :: State String (Point, Point)
parseRect = do
  words' <- fmap words $ get
  let [p1, _, p2] = words'
  return (point p1, point p2)
  where point (break ((==) ',') -> (x, y)) = (read x, read (drop 1 y))

initGrid initValue = A.array ((0,0), (999,999))
           [ ((x,y), initValue) | x <- [0..999], y <- [0..999] ]

instructionFolder :: a -> [((a -> a), (Point, Point))] -> Array (Int, Int) a
instructionFolder initValue =
  foldl (\grid (f, ((x1, y1), (x2, y2))) ->
           grid A.// [ (p, f (grid A.! p)) | x <- [x1..x2], y <- [y1..y2], let p = (x, y) ])
  (initGrid initValue)

p1 input =
  show . getSum
  . foldMap (\case
                True -> Sum 1
                False -> Sum 0)
  . instructionFolder False
  $ map (evalState parseInstruction) . lines $ input
  
p2 input =
  show . foldl (+) (0 :: Integer)
  . instructionFolder (0 :: Integer)
  $ map (evalState parseInstruction2) . lines $ input
  
