{-# LANGUAGE OverloadedStrings #-}

module Advent15Client where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Default.Class
import Data.Maybe (fromJust)
import Data.Text
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B

getDay :: Int -> IO String
getDay day = runReq def $ do
  let cookieHeader =
        (header
         "cookie"
         "_ga=GA1.2.705727629.1567566706; _gid=GA1.2.507807161.1567566706; session=53616c7465645f5f33d58869d1e8431d6edb1b8b5e4670808f6554d02bb7a9bb2e44fb60f87557232fe48498c30d4f21")
  let (url, _) = fromJust . parseUrlHttps . B.pack $
                 ("https://adventofcode.com/2015/day/" ++ (show day) ++ "/input")
  res <- req GET
    url
    NoReqBody
    bsResponse
    cookieHeader
  return . B.unpack . B.init . responseBody $ res
