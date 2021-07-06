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

import Data.Domain (Domain (..))
import Data.Either.Combinators (mapLeft)
import Data.String.Conversions
import Federator.Options
import Federator.Remote (Remote)
import Federator.Validation
import Imports
import Polysemy (runM)
import Polysemy.Embed
import qualified Polysemy.Error as Polysemy
import qualified Polysemy.Reader as Polysemy
import Test.Federator.InternalServer ()
import Test.Polysemy.Mock (evalMock)
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Federation.GRPC.Types

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
      testGroup "validatePath" $
        [ validatePathSuccess
        ],
      testGroup "validatePathNormalize" validatePathNormalize
    ]

federateWithAllowListSuccess :: TestTree
federateWithAllowListSuccess =
  testCase "should give True when target domain is in the list" $
    -- removing evalMock @Remote doesn't seem to work, but why?
    runM . evalMock @Remote @IO $ do
      let allowList = RunSettings (AllowList (AllowedDomains [Domain "hello.world"]))
      res <- Polysemy.runReader allowList $ federateWith (Domain "hello.world")
      embed $ assertBool "federating should be allowed" res

federateWithAllowListFail :: TestTree
federateWithAllowListFail =
  testCase "should give False when target domain is not in the list" $
    runM . evalMock @Remote @IO $ do
      let allowList = RunSettings (AllowList (AllowedDomains [Domain "only.other.domain"]))
      res <- Polysemy.runReader allowList $ federateWith (Domain "hello.world")
      embed $ assertBool "federating should not be allowed" (not res)

validateDomainAllowListFailSemantic :: TestTree
validateDomainAllowListFailSemantic =
  testCase "semantic validation" $
    runM . evalMock @Remote @IO $ do
      let allowList = RunSettings (AllowList (AllowedDomains [Domain "only.other.domain"]))
      res :: Either InwardError Domain <- Polysemy.runError . Polysemy.runReader allowList $ validateDomain ("invalid//.><-semantic-&@-domain" :: Text)
      embed $ assertEqual "semantic parse failure" (Left IInvalidDomain) (mapLeft inwardErrorType res)

validateDomainAllowListFail :: TestTree
validateDomainAllowListFail =
  testCase "allow list validation" $
    runM . evalMock @Remote @IO $ do
      let allowList = RunSettings (AllowList (AllowedDomains [Domain "only.other.domain"]))
      res :: Either InwardError Domain <- Polysemy.runError . Polysemy.runReader allowList $ validateDomain ("hello.world" :: Text)
      embed $ assertEqual "allow list:" (Left IFederationDeniedByRemote) (mapLeft inwardErrorType res)

validateDomainAllowListSuccess :: TestTree
validateDomainAllowListSuccess =
  testCase "should give parsed domain if in the allow list" $
    -- removing evalMock @Remote doesn't seem to work, but why?
    runM . evalMock @Remote @IO $ do
      let domain = Domain "hello.world"
      let allowList = RunSettings (AllowList (AllowedDomains [domain]))
      res :: Either InwardError Domain <- Polysemy.runError . Polysemy.runReader allowList $ validateDomain ("hello.world" :: Text)
      embed $ assertEqual "validateDomain should give 'hello.world' as domain" (Right domain) res

validatePathSuccess :: TestTree
validatePathSuccess = do
  let path = "federation/get-user-by-handle"
  testCase ("should allow " <> cs path) $ do
    res <- runSanitize path
    res @?= Right path

validatePathNormalize :: [TestTree]
validatePathNormalize = do
  let x =
        [ ("federation//stuff", "federation/stuff"),
          -- TODO add more
          ("federation/../federation/stuff", "/federation/stuff")
          -- ("federation%2Fstuff", "federation/stuff")
        ]
  expectNormalized <$> x
  where
    expectNormalized :: (ByteString, ByteString) -> TestTree
    expectNormalized (input, output) = do
      testCase ("Should allow " <> cs input <> " and normalize to " <> cs output) $ do
        res <- runSanitize input
        res @?= Right output

runSanitize :: ByteString -> IO (Either InwardError ByteString)
runSanitize = runM . evalMock @Remote @IO . Polysemy.runError @InwardError . sanitizePath
