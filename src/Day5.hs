{-# LANGUAGE 
    UnicodeSyntax
  , LambdaCase
  , BlockArguments
#-}

module Day5 where

import Data.List (intersect, groupBy, group, isInfixOf)

import Common
import Control.Arrow
import Control.Applicative

niceCount :: [String -> Bool] -> String -> String
niceCount nicePredicates = 
  lines >>> filter isNice >>> length >>> show
  where isNice string = all ($ string) nicePredicates

p1 :: String -> String
p1 = niceCount [ atLeast3Vowels, doubleLetter, noInfixes ]
  where
    atLeast3Vowels = intersect "aeiou" >>> length >>> (>= 3)
    doubleLetter = group >>> any (\strike -> length strike > 1)
    noInfixes str =
      allFalse [ isInfixOf disallowed str | disallowed <- ["ab", "cd", "pq", "xy"] ]

p2 :: String -> String
p2 = niceCount [ xyxyPattern, xyxPattern ]
  where 
    xyxyPattern str = 
      map2 (==) str (drop 2 str)  -- True when characters match
      |> groupBy (&&)             -- both are True
      |> any ((1 < ) . length)
    xyxPattern str =
      map2 (==) str (drop 2 str)
      |> any id
