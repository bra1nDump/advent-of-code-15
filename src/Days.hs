{-# LANGUAGE
  BangPatterns
  , QuasiQuotes
  , NamedFieldPuns
#-}

module Days where

import Common
import Data.String.Interpolate

import System.Directory
import System.IO
import System.IO.Error
import System.Clock

import Control.Exception
import Control.Applicative

import Advent15Client
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8

daysDir = "C:/Users/kirill/Desktop/projects/advent-of-code-15/days/"

type Day = Int

-- try to use ErrorT monad, looks interesting
inputForDay :: Day -> IO String
inputForDay day = do
  let dayPath = daysDir ++ show day
  exists <- doesPathExist dayPath
  if exists then readFile dayPath
  else do
    dayInput <- getDay day 
    writeFile dayPath dayInput
    return dayInput

data Solver = Solver
  { part1 :: String -> String
  , part2 :: String -> String 
  }

solvers :: [(Day, Solver)]
solvers =
  [ (1, Solver Day1.p1 Day1.p2)
  , (2, Solver Day2.p1 Day2.p2)
  , (3, Solver Day3.p1 Day3.p2)
  , (4, Solver Day4.p1 Day4.p2)
  , (5, Solver Day5.p1 Day5.p2)
  , (6, Solver Day6.p1 Day6.p2)
  , (7, Solver Day7.p1 Day7.p2)
  , (8, Solver Day8.p1 Day8.p2)
  ]

solveDay :: Solver -> String -> IO ()
solveDay (Solver { part1, part2}) input = do
  time part1 "1"
  time part2 "2"
  where 
    time partSolver label = do
      start <- getTime Realtime
      let !solution = partSolver input
      end <- getTime Realtime
      putStrLn [i|#{label} #{solution} time: #{diffTimeSpec end start}|]

solveAll :: IO ()
solveAll =
  solvers
  |> map (\(day, solver) -> do
      print [i|day #{day} |]
      getDay day >>= solveDay solver
    )
  |> sequence_
  