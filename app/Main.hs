module Main where

import System.Directory
import System.IO

import Lib

daysDir = "/Users/kirill/competitive/advent15/days/"

getDays :: Int -> IO [String]
getDays dayCount =
  sequence
  . fmap readFile
  . fmap (daysDir ++)
  . fmap show
  $ [1..dayCount]

solvers :: [String -> (String, String)]
solvers =
  [ day1
  , day2
  , day3
  ]

main :: IO ()
main = do
  days <- getDays 3
  print $
    Prelude.zipWith ($) solvers days

