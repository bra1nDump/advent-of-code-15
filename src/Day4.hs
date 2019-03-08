{- LANGUAGE OverloadedStrings -}

module Day4 (day4) where

import Control.Arrow

import Data.Digest.Pure.MD5 (md5)
import Data.List as L
import Data.ByteString.Lazy.Char8 as BS

day4 :: String -> (String, String)
day4 = L.filter ('\n' /=) >>> solve

solve input =
  (p1, p2)
  where
    notLucky1 = ("00000" /=) . L.take 5
    notLucky2 = ("000000" /=) . L.take 6
    mine notLucky =
      show . L.head
      . L.dropWhile
      (notLucky . show
       . md5
       . BS.pack . (input ++) . show
      )
      $ [1..]
    p1 = mine notLucky1
    p2 = mine notLucky2
