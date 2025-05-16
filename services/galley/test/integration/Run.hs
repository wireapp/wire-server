-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Run
  ( main,
  )
where

import API qualified
import API.SQS qualified as SQS
import Bilge hiding (body, header, host, port)
import Bilge qualified
import Cassandra.Util
import Control.Lens
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Conversion
import Data.Proxy
import Data.Tagged
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeFileEither)
import Federation
import Galley.Aws qualified as Aws
import Galley.Options hiding (endpoint)
import Galley.Options qualified as O
import Imports hiding (local)
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import OpenSSL (withOpenSSL)
import Options.Applicative
import System.Logger.Class qualified as Logger
import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Ingredients.Basic
import Test.Tasty.Options
import Test.Tasty.Runners.AntXML
import TestHelpers (test)
import TestSetup
import Util.Options
import Util.Options.Common
import Util.Test
import Util.Test.SQS qualified as SQS

newtype ServiceConfigFile = ServiceConfigFile String
  deriving (Eq, Ord, Typeable)

instance IsOption ServiceConfigFile where
  defaultValue = ServiceConfigFile "/etc/wire/galley/conf/galley.yaml"
  parseValue = fmap ServiceConfigFile . safeRead
  optionName = pure "service-config"
  optionHelp = pure "Service config file to read from"
  optionCLParser =
    fmap ServiceConfigFile $
      strOption $
        ( short (untag (pure 's' :: Tagged ServiceConfigFile Char))
            <> long (untag (optionName :: Tagged ServiceConfigFile String))
            <> help (untag (optionHelp :: Tagged ServiceConfigFile String))
        )

runTests :: (String -> String -> TestTree) -> IO ()
runTests run = defaultMainWithIngredients ings $
  askOption $
    \(ServiceConfigFile c) ->
      askOption $ \(IntegrationConfigFile i) -> run c i
  where
    ings =
      includingOptions
        [ Option (Proxy :: Proxy ServiceConfigFile),
          Option (Proxy :: Proxy IntegrationConfigFile)
        ]
        : listingTests
        : composeReporters antXMLRunner consoleTestReporter
        : defaultIngredients

main :: IO ()
main = withOpenSSL $ runTests go
  where
    go g i = withResource (getOpts g i) releaseOpts $ \setup ->
      testGroup
        "galley"
        [ API.tests setup,
          test setup "isConvMemberL" isConvMemberLTests
        ]
    getOpts gFile iFile = do
      m <- newManager tlsManagerSettings {managerResponseTimeout = responseTimeoutMicro 300000000}
      let local p = Endpoint {host = "127.0.0.1", port = p}
      gConf <- handleParseError =<< decodeFileEither gFile
      iConf <- handleParseError =<< decodeFileEither iFile
      -- FUTUREWORK: we don't support process env setup any more, so both gconf and iConf
      -- must be 'Just'.  the following code could be simplified a lot, but this should
      -- probably happen after (or at least while) unifying the integration test suites into
      -- a single library.
      galleyEndpoint <- optOrEnv (.galley) iConf (local . read) "GALLEY_WEB_PORT"
      let g = mkRequest galleyEndpoint
      b <- mkRequest <$> optOrEnv (.brig) iConf (local . read) "BRIG_WEB_PORT"
      c <- mkRequest <$> optOrEnv (.cannon) iConf (local . read) "CANNON_WEB_PORT"
      -- unset this env variable in galley's config to disable testing SQS team events
      q <- join <$> optOrEnvSafe queueName' gConf (Just . pack) "GALLEY_SQS_TEAM_EVENTS"
      e <- join <$> optOrEnvSafe endpoint' gConf (fromByteString . BS.pack) "GALLEY_SQS_ENDPOINT"
      convMaxSize <- optOrEnv maxSize gConf read "CONV_MAX_SIZE"
      awsEnv <- initAwsEnv e q
      -- Initialize cassandra
      lg <- Logger.new Logger.defSettings
      db <- defInitCassandra (fromJust gConf ^. cassandra) lg
      teamEventWatcher <- sequence $ (SQS.watchSQSQueue . (^. Aws.awsEnv) <$> awsEnv) <*> q
      pure $ TestSetup (fromJust gConf) (fromJust iConf) m g b c awsEnv convMaxSize db (FedClient m galleyEndpoint) teamEventWatcher
    queueName' = fmap (view queueName) . view journal
    endpoint' = fmap (view O.endpoint) . view journal
    maxSize = view (settings . maxConvSize)
    initAwsEnv (Just e) (Just q) = Just <$> SQS.mkAWSEnv (JournalOpts q e)
    initAwsEnv _ _ = pure Nothing
    releaseOpts _ = pure ()
    mkRequest (Endpoint h p) = Bilge.host (encodeUtf8 h) . Bilge.port p
