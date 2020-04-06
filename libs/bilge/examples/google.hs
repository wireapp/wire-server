{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

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
