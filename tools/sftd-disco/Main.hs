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

import Control.Concurrent.Async
import Control.Monad.Catch
import Control.Monad.Trans
import qualified Data.Attoparsec.ByteString.Char8 as Parser
import Data.Bifunctor
import Data.Bitraversable
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IP
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Data.Traversable
import Data.Word
import Network.DNS hiding (header)
import Network.HTTP.Types
import Network.Socket
import Network.Socket.ByteString
import Network.Wai
import Network.Wai.Handler.Warp
import Options.Applicative
import System.Clock
import qualified System.Logger as Log
import System.Logger.Message (msg, val)
-- this library sucks
import System.Metrics.Prometheus.Concurrent.RegistryT
import System.Metrics.Prometheus.Encode.Text
import qualified System.Metrics.Prometheus.Metric.Counter as Counter
import System.Metrics.Prometheus.Metric.Gauge (Gauge)
import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge
import qualified System.Metrics.Prometheus.Metric.Histogram as Histo
import System.Metrics.Prometheus.MetricId
import System.Metrics.Prometheus.Registry (RegistrySample)
import System.Timeout (timeout)

data Opts = Opts
  { optTier :: !Text
  }
  deriving (Show)

parseOpts :: ParserInfo Opts
parseOpts = info (helper <*> parser) desc
  where
    desc = header "restund Metrics Exporter" <> fullDesc

    parser =
      Opts
        <$> option
          txt
          ( long "tier"
              <> help "Deployment Tier"
          )

    txt = Text.pack <$> str
    bs = ByteString.pack <$> str

main :: IO ()
main = withSocketsDo $ do
  opts <- execParser parseOpts
  lgr <- Log.new Log.defSettings
  rlv <- makeResolvSeed defaultResolvConf
  undefined
