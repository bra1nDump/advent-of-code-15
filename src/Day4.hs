{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import Common
import Control.Arrow

import Data.Digest.Pure.MD5 (md5, md5DigestBytes)
import Data.List as List
import qualified Data.ByteString.Lazy.Char8 as BS

hash :: BS.ByteString -> BS.ByteString
hash = BS.fromStrict . md5DigestBytes . md5

mine :: String -> (String -> Bool) -> String
mine prefix notLucky =
  show . head
  . dropWhile
  (notLucky . show
    . md5
    . BS.pack . (prefix ++) . show
  )
  $ [1..]

p1 :: String -> String 
p1 input = mine input (not . List.isPrefixOf "00000")

p2 :: String -> String
p2 input = mine input (not . List.isPrefixOf "000000")
