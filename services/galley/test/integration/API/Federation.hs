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

module API.Federation where

import API.Util
import Bilge
import Bilge.Assert
import Control.Lens
import Data.List1
import Data.Qualified (Qualified (..))
import Galley.Types
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import TestHelpers
import TestSetup
import Wire.API.Federation.API.Galley (GetConversationsRequest (..), GetConversationsResponse (..))
import qualified Wire.API.Federation.API.Galley as FedGalley

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "federation"
    [ test s "getConversations (/federation/conversations/get-by-ids) : All Found" getConversationsAllFound
    ]

getConversationsAllFound :: TestM ()
getConversationsAllFound = do
  -- FUTUREWORK: make alice / bob remote users
  [alice, bob] <- randomUsers 2
  connectUsers alice (singleton bob)
  -- create & get one2one conv
  cnv1 <- responseJsonUnsafeWithMsg "conversation" <$> postO2OConv alice bob (Just "gossip1")
  getConvs alice (Just $ Left [cnvId cnv1]) Nothing !!! do
    const 200 === statusCode
    const (Just [cnvId cnv1]) === fmap (map cnvId . convList) . responseJsonUnsafe
  -- create & get group conv
  carl <- randomUser
  connectUsers alice (singleton carl)
  cnv2 <- responseJsonUnsafeWithMsg "conversation" <$> postConv alice [bob, carl] (Just "gossip2") [] Nothing Nothing
  getConvs alice (Just $ Left [cnvId cnv2]) Nothing !!! do
    const 200 === statusCode
    const (Just [cnvId cnv2]) === fmap (map cnvId . convList) . responseJsonUnsafe
  -- get both

  fedGalleyClient <- view tsFedGalleyClient
  localDomain <- viewFederationDomain
  let aliceQualified = Qualified alice localDomain
  GetConversationsResponse cs <- FedGalley.getConversations fedGalleyClient (GetConversationsRequest aliceQualified [cnvId cnv1, cnvId cnv2])
  let c1 = find ((== cnvId cnv1) . cnvId) cs
  let c2 = find ((== cnvId cnv2) . cnvId) cs
  liftIO . forM_ [(cnv1, c1), (cnv2, c2)] $ \(expected, actual) -> do
    assertEqual
      "name mismatch"
      (Just $ cnvName expected)
      (cnvName <$> actual)
    assertEqual
      "self member mismatch"
      (Just . cmSelf $ cnvMembers expected)
      (cmSelf . cnvMembers <$> actual)
    assertEqual
      "other members mismatch"
      (Just [])
      ((\c -> cmOthers (cnvMembers c) \\ cmOthers (cnvMembers expected)) <$> actual)
