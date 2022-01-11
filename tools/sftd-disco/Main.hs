{-# LANGUAGE LambdaCase #-}
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

module Main (main) where

import Control.Monad (forM)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Word (Word16)
import Network.DNS hiding (Domain, header)
import Network.HTTP.Client hiding (port)
import Network.HTTP.Types.Status
import Network.Socket
import qualified Options.Applicative as P
import qualified System.Logger as Log
import System.Logger.Class
import URI.ByteString

newtype Opts = Opts
  { optSrvRecord :: ByteString
  }
  deriving (Show)

parseOpts :: P.ParserInfo Opts
parseOpts = P.info (P.helper <*> parser) desc
  where
    desc = P.header "SFT discovery service" <> P.fullDesc

    parser =
      Opts
        <$> P.option
          bs
          ( P.long "srvRecord"
              <> P.help "SRV record containing SFT services. Example: _sft._tcp.wire-server-sftd.wire.svc.cluster.local"
          )

    bs :: P.ReadM ByteString
    bs = ByteString.pack <$> P.str

initHttpManager :: IO Manager
initHttpManager =
  newManager
    defaultManagerSettings
      { managerResponseTimeout = responseTimeoutMicro 10000000
      }

main :: IO ()
main = withSocketsDo $ do
  opts <- P.execParser parseOpts
  lgr <- Log.new Log.defSettings
  manager <- initHttpManager
  rlv <- makeResolvSeed defaultResolvConf
  withResolver rlv $ \resolver ->
    lookupSRV resolver (optSrvRecord opts) >>= \case
      Left e -> undefined
      Right results -> do
        urls <- resolveUrls lgr manager results
        -- writeToFile
        undefined

  undefined

resolveUrls :: Logger -> Manager -> [(Word16, Word16, Word16, ByteString)] -> IO [Text]
resolveUrls l m input = forM input (resolveUrl l m)

resolveUrl :: Logger -> Manager -> (Word16, Word16, Word16, ByteString) -> IO Text
resolveUrl lgr mgr (_, _, port, dom) = do
  let url = "http://" <> cs dom <> ":" <> show port
  request <- parseRequest url
  response <- httpLbs request mgr

  Log.info lgr $
    msg (val "The status code was: ")
      ~~ field "code" (statusCode $ responseStatus response)
  let body = cs $ responseBody response

  case parseURI strictURIParserOptions body of
    Left e ->
      Log.err lgr $
        msg (val "Error parsing response")
          ~~ field "request" url
          ~~ field "error" (show e)
    Right uri -> do
      print "uri = "
      print uri
      undefined

  undefined
