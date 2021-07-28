{-# OPTIONS_GHC -Wno-orphans #-}

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

module Test.Federator.Validation where

import qualified Data.ByteString as BS
import Data.Domain (Domain (..), domainText)
import Data.Either.Combinators (mapLeft)
import Data.String.Conversions
import qualified Data.Text.Encoding as Text
import Federator.Discovery (DiscoverFederator (..))
import Federator.Options
import Federator.Remote (Remote)
import Federator.Validation
import Imports
import Polysemy (Sem, runM)
import qualified Polysemy
import Polysemy.Embed
import qualified Polysemy.Error as Polysemy
import qualified Polysemy.Reader as Polysemy
import Test.Federator.InternalServer ()
import Test.Federator.Options (noClientCertSettings)
import Test.Polysemy.Mock (evalMock)
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Federation.GRPC.Types
import Wire.Network.DNS.SRV (SrvTarget (..))

mockDiscoveryTrivial :: Sem (DiscoverFederator ': r) x -> Sem r x
mockDiscoveryTrivial = Polysemy.interpret $ \(DiscoverFederator dom) ->
  pure . Right $ SrvTarget (Text.encodeUtf8 (domainText dom)) 443

tests :: TestTree
tests =
  testGroup "Validation" $
    [ testGroup "federateWith" $
        [ federateWithAllowListSuccess,
          federateWithAllowListFail
        ],
      testGroup "validateDomain" $
        [ validateDomainAllowListFailSemantic,
          validateDomainAllowListFail,
          validateDomainAllowListSuccess
        ],
      testGroup "validatePath - Success" validatePathSuccess,
      testGroup "validatePath - Normalize" validatePathNormalize,
      testGroup "validatePath - Forbid" validatePathForbidden
    ]

federateWithAllowListSuccess :: TestTree
federateWithAllowListSuccess =
  testCase "should give True when target domain is in the list" $
    -- removing evalMock @Remote doesn't seem to work, but why?
    runM . evalMock @Remote @IO $ do
      let settings = settingsWithAllowList [Domain "hello.world"]
      res <- Polysemy.runReader settings $ federateWith (Domain "hello.world")
      embed $ assertBool "federating should be allowed" res

federateWithAllowListFail :: TestTree
federateWithAllowListFail =
  testCase "should give False when target domain is not in the list" $
    runM . evalMock @Remote @IO $ do
      let settings = settingsWithAllowList [Domain "only.other.domain"]
      res <- Polysemy.runReader settings $ federateWith (Domain "hello.world")
      embed $ assertBool "federating should not be allowed" (not res)

validateDomainAllowListFailSemantic :: TestTree
validateDomainAllowListFailSemantic =
  testCase "semantic validation" $
    runM . evalMock @Remote @IO $ do
      let settings = settingsWithAllowList [Domain "only.other.domain"]
      res :: Either InwardError Domain <-
        Polysemy.runError
          . mockDiscoveryTrivial
          . Polysemy.runReader settings
          $ validateDomain Nothing ("invalid//.><-semantic-&@-domain" :: Text)
      embed $ assertEqual "semantic parse failure" (Left IInvalidDomain) (mapLeft inwardErrorType res)

validateDomainAllowListFail :: TestTree
validateDomainAllowListFail =
  testCase "allow list validation" $
    runM . evalMock @Remote @IO $ do
      let settings = settingsWithAllowList [Domain "only.other.domain"]
      res :: Either InwardError Domain <-
        Polysemy.runError
          . mockDiscoveryTrivial
          . Polysemy.runReader settings
          $ validateDomain Nothing ("hello.world" :: Text)
      embed $ assertEqual "allow list:" (Left IFederationDeniedByRemote) (mapLeft inwardErrorType res)

validateDomainAllowListSuccess :: TestTree
validateDomainAllowListSuccess =
  testCase "should give parsed domain if in the allow list" $
    -- removing evalMock @Remote doesn't seem to work, but why?
    runM . evalMock @Remote @IO $ do
      let domain = Domain "hello.world"
      let settings = settingsWithAllowList [domain]
      res :: Either InwardError Domain <-
        Polysemy.runError
          . mockDiscoveryTrivial
          . Polysemy.runReader settings
          $ validateDomain Nothing ("hello.world" :: Text)
      embed $ assertEqual "validateDomain should give 'hello.world' as domain" (Right domain) res

validatePathSuccess :: [TestTree]
validatePathSuccess = do
  let paths =
        [ "federation/get-user-by-handle",
          "federation/get-conversations"
        ]
  expectOk <$> paths
  where
    expectOk :: ByteString -> TestTree
    expectOk path = testCase ("should allow " <> cs path) $ do
      res <- runSanitize path
      res @?= Right path

validatePathNormalize :: [TestTree]
validatePathNormalize = do
  let paths =
        [ ("federation//stuff", "federation/stuff"),
          ("/federation/get-user-by-handle", "federation/get-user-by-handle"),
          ("federation/../federation/stuff", "federation/stuff")
        ]
  expectNormalized <$> paths
  where
    expectNormalized :: (ByteString, ByteString) -> TestTree
    expectNormalized (input, output) = do
      testCase ("Should allow " <> cs input <> " and normalize to " <> cs output) $ do
        res <- runSanitize input
        res @?= Right output

validatePathForbidden :: [TestTree]
validatePathForbidden = do
  let paths =
        [ "",
          "/",
          "///",
          -- disallowed paths
          "federation",
          "/federation",
          "/federation/",
          "i/users",
          "/i/users",
          -- path traversals to avoid
          "../i/users",
          "federation/../i/users",
          "federation/%2e%2e/i/users", -- percent-encoded '../'
          "federation/%2E%2E/i/users",
          "federation/%252e%252e/i/users", -- double percent-encoded '../'
          "federation/%c0%ae%c0%ae/i/users", -- weird-encoded '../'
          -- syntax we don't wish to support
          "federation/é", -- not ASCII
          "federation/stuff?bar[]=baz", -- not parseable as URI
          "http://federation.wire.link/federation/stuff", -- contains scheme and domain
          "http://federation/stuff", -- contains scheme
          "federation.wire.link/federation/stuff", -- contains domain
          "federation/stuff?key=value", -- contains query parameter
          "federation/stuff%3fkey%3dvalue", -- contains query parameter
          "federation/stuff#fragment", -- contains fragment
          "federation/stuff%23fragment", -- contains fragment
          "federation/this-url-is-waaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaay-too-long"
        ]
  expectForbidden <$> paths
  where
    expectForbidden :: ByteString -> TestTree
    expectForbidden input = do
      testCase ("Should forbid '" <> cs (BS.take 40 input) <> "'") $ do
        res <- runSanitize input
        expectErr IForbiddenEndpoint res

runSanitize :: ByteString -> IO (Either InwardError ByteString)
runSanitize = runM . evalMock @Remote @IO . Polysemy.runError @InwardError . sanitizePath

expectErr :: InwardErrorType -> Either InwardError ByteString -> IO ()
expectErr expectedType (Right bdy) = do
  assertFailure $ "expected error '" <> show expectedType <> "' but got a valid body: " <> show bdy
expectErr expectedType (Left err) =
  unless (inwardErrorType err == expectedType)
    . liftIO
    $ assertFailure $ "expected type '" <> show expectedType <> "' but got " <> show err

settingsWithAllowList :: [Domain] -> RunSettings
settingsWithAllowList domains =
  noClientCertSettings {federationStrategy = AllowList (AllowedDomains domains)}
