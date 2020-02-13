{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bilge
import Imports

main :: IO ()
main = do
  assert (get (host "google.com" . debug Head)) $ do
    const 301 === statusCode
    const "Moved Permanently" === statusMessage
  get (host "www.google.de" . debug Head) !!! do
    const 200 === statusCode
    const "OK" === statusMessage
    const "text/html" =~= getHeader' "Content-Type"
    assertTrue "Unexpected Transfer-Encoding" $ \r ->
      getHeader' "Transfer-Encoding" r == "chunked"
  let req =
        host "www.google.de"
          . path "/"
          . query "q" "hello world"
          . accept "text/plain"
  rs <- assertR (get (req . debug Head)) $ do
    const 200 === statusCode
    const (Just "hello world") =~= getBody
  print (statusMessage rs)
