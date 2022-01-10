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

import Data.Text (Text)
import qualified Data.Text as Text
import Network.DNS hiding (header)
import Network.Socket
import Options.Applicative
import qualified System.Logger as Log

newtype Opts = Opts
  { optSrvRecord :: Text
  }
  deriving (Show)

parseOpts :: ParserInfo Opts
parseOpts = info (helper <*> parser) desc
  where
    desc = header "SFT discovery service" <> fullDesc

    parser =
      Opts
        <$> option
          txt
          ( long "srvRecord"
              <> help "SRV record containing SFT services. Example: _sft._tcp.wire-server-sftd.wire.svc.cluster.local"
          )

    txt = Text.pack <$> str

main :: IO ()
main = withSocketsDo $ do
  opts <- execParser parseOpts
  lgr <- Log.new Log.defSettings
  rlv <- makeResolvSeed defaultResolvConf
  undefined
