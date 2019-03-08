{-# LANGUAGE OverloadedStrings #-}

module DownloadDays (fetchDays) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Default.Class
import Data.Maybe (fromJust)
import Data.Text
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B

fetchDay :: Int -> IO String
fetchDay day = runReq def $ do
  let cookieHeader =
        (header
         "cookie"
         "_ga=GA1.2.625403450.1551846454; _gid=GA1.2.351519271.1551846454; session=53616c7465645f5f790947c4b3eb1df8e14ec71583db8fa93d5271d853b0bfd86f99bad89a81feb33b47b17b0d3a8d86")
  let (url, _) = fromJust . parseUrlHttps . B.pack $
                 ("https://adventofcode.com/2015/day/" ++ (show day) ++ "/input")
  res <- req GET
    url
    NoReqBody
    bsResponse
    cookieHeader
  return . B.unpack . responseBody $ res

days = "/Users/kirill/competitive/advent15/days"

fetchDays :: Int -> IO [String]
fetchDays dayCount =
  mapM
  (\day -> do
     input <- fetchDay day
     writeFile (days ++ "/" ++ (show day)) input
     return input
  )
  [1..dayCount]
