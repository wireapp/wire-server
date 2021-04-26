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
import qualified Data.Text as Text
import Federator.Options
import Federator.Remote (Remote)
import Federator.Util
import Imports
import Polysemy (runM)
import Polysemy.Embed
import qualified Polysemy.Reader as Polysemy
import Test.Federator.InternalServer ()
import Test.Polysemy.Mock (evalMock)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup "Validation" $
    [ testGroup "federateWith" $
        [ federateWithAllowListSuccess,
          federateWithAllowListFail
        ],
      testGroup "federateWith'" $
        [ federateWith'AllowListFailSemantic,
          federateWith'AllowListFail,
          federateWith'AllowListSuccess
        ]
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

federateWith'AllowListFailSemantic :: TestTree
federateWith'AllowListFailSemantic =
  testCase "semantic validation" $
    runM . evalMock @Remote @IO $ do
      let allowList = RunSettings (AllowList (AllowedDomains [Domain "only.other.domain"]))
      res <- Polysemy.runReader allowList $ federateWith' ("invalid//.><-semantic-&@-domain" :: Text)
      embed $ assertBool "invalid semantic domains should produce errors" (isLeft res)
      embed $ assertEqual "semantic:" (Left ("Domain parse failure" :: Text)) (mapLeft (Text.take 20) res)

federateWith'AllowListFail :: TestTree
federateWith'AllowListFail =
  testCase "allow list validation" $
    runM . evalMock @Remote @IO $ do
      let allowList = RunSettings (AllowList (AllowedDomains [Domain "only.other.domain"]))
      res <- Polysemy.runReader allowList $ federateWith' ("hello.world" :: Text)
      embed $ assertBool "federating should not be allowed" (isLeft res)
      embed $ assertEqual "allow list:" (Left ("not in the federation allow list" :: Text)) (mapLeft (Text.takeEnd 32) res)

federateWith'AllowListSuccess :: TestTree
federateWith'AllowListSuccess =
  testCase "should give parsed domain if in the allow list" $
    -- removing evalMock @Remote doesn't seem to work, but why?
    runM . evalMock @Remote @IO $ do
      let domain = Domain "hello.world"
      let allowList = RunSettings (AllowList (AllowedDomains [domain]))
      res <- Polysemy.runReader allowList $ federateWith' ("hello.world" :: Text)
      embed $ assertEqual "federateWith' should give 'hello.world' as domain" (Right domain) res
