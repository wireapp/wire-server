{-# LANGUAGE TemplateHaskell #-}

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

module TestSetup
  ( test,
    tsManager,
    tsEndpoint,
    tsBrig,
    tsOpts,
    TestSetup (..),
    Cargohold,
    TestM,
    runTestM,
    viewUnversionedCargohold,
    viewCargohold,
    createTestSetup,
    runFederationClient,
    withFederationClient,
    withFederationError,
    apiVersion,
    unversioned,
  )
where

import Bilge hiding (body, responseBody)
import CargoHold.Options hiding (domain)
import Control.Exception (catch)
import Control.Lens
import Control.Monad.Codensity
import Control.Monad.Except
import Control.Monad.Morph
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Conversion
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Yaml
import Imports
import Network.HTTP.Client hiding (responseBody)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS
import qualified Network.Wai.Utilities.Error as Wai
import Servant.Client.Streaming
import Test.Tasty
import Test.Tasty.HUnit
import Util.Options (Endpoint (..))
import Util.Options.Common
import Util.Test
import Web.HttpApiData
import Wire.API.Federation.Domain
import Wire.API.Routes.Version

type Cargohold = Request -> Request

type TestM = ReaderT TestSetup Http

mkRequest :: Endpoint -> Request -> Request
mkRequest (Endpoint h p) = Bilge.host (encodeUtf8 h) . Bilge.port p

data TestSetup = TestSetup
  { _tsManager :: Manager,
    _tsEndpoint :: Endpoint,
    _tsBrig :: Endpoint,
    _tsOpts :: Opts
  }

makeLenses ''TestSetup

-- | Note: Apply this function last when composing (Request -> Request) functions
apiVersion :: ByteString -> Request -> Request
apiVersion newVersion r = r {HTTP.path = setVersion newVersion (HTTP.path r)}
  where
    setVersion :: ByteString -> ByteString -> ByteString
    setVersion v p =
      let p' = removeSlash' p
       in v <> "/" <> fromMaybe p' (removeVersionPrefix p')

removeSlash' :: ByteString -> ByteString
removeSlash' s = case B8.uncons s of
  Just ('/', s') -> s'
  _ -> s

removeVersionPrefix :: ByteString -> Maybe ByteString
removeVersionPrefix bs = do
  let (x, s) = B8.splitAt 1 bs
  guard (x == B8.pack "v")
  (_, s') <- B8.readInteger s
  pure (B8.tail s')

-- | Note: Apply this function last when composing (Request -> Request) functions
unversioned :: Request -> Request
unversioned r =
  r
    { HTTP.path =
        maybe
          (HTTP.path r)
          (B8.pack "/" <>)
          (removeVersionPrefix . removeSlash' $ HTTP.path r)
    }

viewCargohold :: TestM Cargohold
viewCargohold =
  fmap
    (apiVersion (toHeader latestVersion) .)
    viewUnversionedCargohold
  where
    latestVersion :: Version
    latestVersion = maxBound

viewUnversionedCargohold :: TestM Cargohold
viewUnversionedCargohold = mkRequest <$> view tsEndpoint

runTestM :: TestSetup -> TestM a -> IO a
runTestM ts action = runHttpT (view tsManager ts) (runReaderT action ts)

test :: IO TestSetup -> TestName -> TestM () -> TestTree
test s name action = testCase name $ do
  ts <- s
  runTestM ts action

data IntegrationConfig = IntegrationConfig
  -- internal endpoint
  { cargohold :: Endpoint,
    brig :: Endpoint
  }
  deriving (Show, Generic)

instance FromJSON IntegrationConfig

createTestSetup :: FilePath -> FilePath -> IO TestSetup
createTestSetup optsPath configPath = do
  -- FUTUREWORK: It would actually be useful to read some
  -- values from cargohold (max bytes, for instance)
  -- so that tests do not need to keep those values
  -- in sync and the user _knows_ what they are
  m <-
    newManager
      tlsManagerSettings
        { managerResponseTimeout = responseTimeoutMicro 300000000
        }
  let localEndpoint p = Endpoint {_host = "127.0.0.1", _port = p}
  iConf <- handleParseError =<< decodeFileEither configPath
  opts <- decodeFileThrow optsPath
  endpoint <- optOrEnv @IntegrationConfig (.cargohold) iConf (localEndpoint . read) "CARGOHOLD_WEB_PORT"
  brigEndpoint <- optOrEnv @IntegrationConfig (.brig) iConf (localEndpoint . read) "BRIG_WEB_PORT"
  pure $
    TestSetup
      { _tsManager = m,
        _tsEndpoint = endpoint,
        _tsBrig = brigEndpoint,
        _tsOpts = opts
      }

runFederationClient :: ClientM a -> ReaderT TestSetup (ExceptT ClientError (Codensity IO)) a
runFederationClient action = do
  man <- view tsManager
  Endpoint cHost cPort <- view tsEndpoint
  domain <- view (tsOpts . settings . federationDomain)
  let base = BaseUrl Http (T.unpack cHost) (fromIntegral cPort) "/federation"
  let env =
        (mkClientEnv man base)
          { makeClientRequest = \burl req -> do
              req' <- defaultMakeClientRequest burl req
              pure req' {requestHeaders = (originDomainHeaderName, toByteString' domain) : requestHeaders req'}
          }

  r <- lift
    . lift
    $ Codensity
    $ \k ->
      -- Servant's streaming client throws exceptions in IO for some reason
      catch (withClientM action env k) (k . Left)

  either throwError pure r

hoistFederation :: ReaderT TestSetup (ExceptT ClientError (Codensity IO)) a -> ExceptT ClientError TestM a
hoistFederation action = do
  env <- ask
  hoist (liftIO . lowerCodensity) $ runReaderT action env

withFederationClient :: ReaderT TestSetup (ExceptT ClientError (Codensity IO)) a -> TestM a
withFederationClient action =
  runExceptT (hoistFederation action) >>= \case
    Left err ->
      liftIO
        . assertFailure
        $ "Unexpected federation client error: "
          <> displayException err
    Right x -> pure x

withFederationError :: ReaderT TestSetup (ExceptT ClientError (Codensity IO)) a -> TestM Wai.Error
withFederationError action =
  runExceptT (hoistFederation action)
    >>= liftIO
      . \case
        Left (FailureResponse _ resp) -> case Aeson.eitherDecode (responseBody resp) of
          Left err -> assertFailure $ "Error while parsing error response: " <> err
          Right e -> (Wai.code e @?= responseStatusCode resp) $> e
        Left err -> assertFailure $ "Unexpected federation client error: " <> displayException err
        Right _ -> assertFailure "Unexpected success"
