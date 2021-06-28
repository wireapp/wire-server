-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Galley.API.Message where

import Data.Coerce (coerce)
import Data.Domain (Domain)
import Data.Id (UserId)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Galley.API.Message
import Imports
import Test.Tasty
import Test.Tasty.QuickCheck
import Wire.API.Message
import Wire.API.User.Client (QualifiedUserClientMap (..), QualifiedUserClients (..), qualifiedUserClientMap)

tests :: TestTree
tests =
  testGroup
    "Galley.API.Message"
    [ testGroup
        "checkMessageClients"
        [ checkMessageClientSuccess,
          checkMessageClientEverythingReported,
          checkMessageClientRedundantSender
        ]
    ]

checkMessageClientSuccess :: TestTree
checkMessageClientSuccess = testProperty "success" $
  \(msg :: QualifiedNewOtrMessage) (senderDomain :: Domain) (senderUser :: UserId) ->
    let plainRecipients = qualifiedUserClientMap . qualifiedOtrRecipientsMap . qualifiedNewOtrRecipients $ msg
        expectedRecipients = qualifiedRecipientMapToSet plainRecipients
     in not (isUserPresent expectedRecipients senderDomain senderUser)
          ==> checkMessageClients senderDomain senderUser (QualifiedUserClients expectedRecipients) msg
          === (Just plainRecipients, QualifiedMismatch mempty mempty mempty)

checkMessageClientRedundantSender :: TestTree
checkMessageClientRedundantSender = testProperty "sender should be part of redundant" $
  \(msg0 :: QualifiedNewOtrMessage) (pickUserFromMsg :: Bool) (defaultSenderDomain :: Domain) (defaultSenderUser :: UserId) ->
    let plainRecipients = qualifiedUserClientMap . qualifiedOtrRecipientsMap . qualifiedNewOtrRecipients $ msg0
        (senderDomain, senderUser) =
          fromMaybe (defaultSenderDomain, defaultSenderUser) $ do
            guard pickUserFromMsg
            (domain, userClientMap) <- Map.lookupMin plainRecipients
            (user, _) <- Map.lookupMin userClientMap
            pure (domain, user)
        senderClientMessage =
          Map.singleton senderDomain
            . Map.singleton senderUser
            . Map.singleton (qualifiedNewOtrSender msg0)
            $ "msg to self"
        senderClient =
          QualifiedUserClients
            . Map.singleton senderDomain
            . Map.singleton senderUser
            . Set.singleton
            $ qualifiedNewOtrSender msg0
        msg =
          msg0
            { qualifiedNewOtrRecipients =
                qualifiedNewOtrRecipients msg0
                  <> QualifiedOtrRecipients (QualifiedUserClientMap senderClientMessage)
            }
        expectedRecipients = QualifiedUserClients $ qualifiedRecipientMapToSet plainRecipients
     in checkMessageClients senderDomain senderUser expectedRecipients msg
          === (Just plainRecipients, QualifiedMismatch mempty senderClient mempty)

-- | FUTUREWORK: Write a custom generator for this test. expected' and
-- expected'' are used along with msg to generate expected, this ensures that we
-- don't always get a disjoint set between the intended recipietns and expected
-- recipients.
checkMessageClientEverythingReported :: TestTree
checkMessageClientEverythingReported = testProperty "all intended and expected recipients should be part of valid and extras" $
  \(msg :: QualifiedNewOtrMessage) (senderDomain :: Domain) (senderUser :: UserId) (expected' :: QualifiedUserClients) (expected'' :: QualifiedUserClients) ->
    let expected = coerce (qualifiedDiff (qualifiedRecipientMapToSet @ByteString . coerce $ qualifiedNewOtrRecipients msg) (coerce expected')) <> expected''
        plainRecipients = qualifiedUserClientMap . qualifiedOtrRecipientsMap . qualifiedNewOtrRecipients $ msg
        intendedRecipients = QualifiedUserClients $ qualifiedRecipientMapToSet plainRecipients
        (maybeValidMessages, mismatch) = checkMessageClients senderDomain senderUser expected (msg {qualifiedNewOtrClientMismatchStrategy = MismatchIgnoreAll})
        validRecipients = maybe mempty qualifiedRecipientMapToSet maybeValidMessages
        extraRecipients = qmMissing mismatch <> qmDeleted mismatch <> qmRedundant mismatch
     in QualifiedUserClients validRecipients <> extraRecipients === intendedRecipients <> expected
