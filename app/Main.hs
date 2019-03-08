module Main where

import System.Directory
import System.IO
import System.IO.Error

import Control.Exception

import DownloadDays (fetchDays)

import Day1
import Day2
import Day3
import Day4

daysDir = "/Users/kirill/competitive/advent15/days/"

readDays :: Int -> IO [String]
readDays dayCount =
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
  , day4
  ]

days :: IO [String]
days = catchJust
       (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing)
       (readDays . length $ solvers)
       (\_ -> fetchDays . length $ solvers)

main :: IO ()
main = do
  days <- days
  print $
    Prelude.zipWith ($) solvers days

