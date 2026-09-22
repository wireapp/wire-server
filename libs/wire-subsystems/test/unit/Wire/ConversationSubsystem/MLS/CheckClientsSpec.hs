-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it
-- under the terms of the GNU Affero General Public License as published by the
-- Free Software Foundation, either version 3 of the License, or (at your
-- option) any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
-- for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.ConversationSubsystem.MLS.CheckClientsSpec (spec) where

import Data.Domain (Domain (..))
import Data.Id
import Data.Map qualified as Map
import Data.Qualified
import Data.UUID qualified as UUID
import Galley.Types.Error (InternalError (..))
import Imports
import Polysemy
import Polysemy.Async (asyncToIOFinal)
import Polysemy.Error (Error, runError, throw)
import Test.Hspec
import Wire.API.Conversation
import Wire.API.Conversation.Protocol
import Wire.API.Error (runErrorS)
import Wire.API.Error.Galley (GalleyError (MLSClientMismatch, MLSIdentityMismatch), MLSProtocolError)
import Wire.API.Federation.Client (FederatorClient)
import Wire.API.Federation.Error (FederationError (..))
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.KeyPackage (KeyPackage)
import Wire.API.MLS.LeafNode (LeafIndex)
import Wire.API.MLS.SubConversation (ConvOrSubChoice (..))
import Wire.BrigAPIAccess (BrigAPIAccess (..))
import Wire.ConversationStore.MLS.Types
import Wire.ConversationSubsystem.MLS.CheckClients
import Wire.FederationAPIAccess (FederationAPIAccess (..))
import Wire.StoredConversation (MLSMigrationState (..))

data TestBrigFailure = TestBrigFailure
  deriving stock (Show)

data BrigBehavior
  = BrigCrashes
  | BrigUnused

spec :: Spec
spec = describe "Wire.ConversationSubsystem.MLS.CheckClients" do
  it "aborts the commit when a concurrent brig client-data fetch crashes" $ do
    result <- runCheckClients BrigCrashes
    case result of
      Right (Left (InternalErrorWithDescription _)) -> pure ()
      _ ->
        expectationFailure $
          "expected InternalErrorWithDescription, got: " <> show result

  it "still treats a hushed FederationError as user-unreachable" $ do
    result <- runCheckClients BrigUnused
    case result of
      Right (Right unreachable) ->
        fmap qUnqualified unreachable `shouldBe` [userId]
      _ ->
        expectationFailure $
          "expected unreachable user, got: " <> show result

-- | Runs 'checkClients' with a layering that mirrors galley's production
-- stack (cf. Galley.App): all pure error interpreters ('runError' and
-- 'runErrorS') sit outside 'asyncToIOFinal', so an 'Error'-effect throw
-- inside a spawned child collapses to 'Nothing' instead of propagating.
runCheckClients ::
  BrigBehavior ->
  IO (Either TestBrigFailure (Either InternalError [Qualified UserId]))
runCheckClients behavior =
  runFinal @IO
    . runError @TestBrigFailure
    . runError @InternalError
    . (fmap (fromMaybe (error "unexpected MLSIdentityMismatch")) . runErrorS @'MLSIdentityMismatch)
    . (fmap (fromMaybe (error "unexpected MLSClientMismatch")) . runErrorS @'MLSClientMismatch)
    . (fmap (either (error "unexpected MLSProtocolError") id) . runError @MLSProtocolError)
    . asyncToIOFinal
    . embedToFinal @IO
    . interpretTestBrig behavior
    . interpretTestFederation
    $ checkClients lConv csSuite (newCM (qUserId behavior))

interpretTestBrig ::
  (Polysemy.Member (Error TestBrigFailure) r) =>
  BrigBehavior ->
  Sem (BrigAPIAccess ': r) a ->
  Sem r a
interpretTestBrig behavior =
  interpret $ \case
    GetLocalMLSClients {} -> case behavior of
      BrigCrashes -> throw TestBrigFailure
      BrigUnused -> error "unexpected GetLocalMLSClients call in test"
    _ -> error "unexpected BrigAPIAccess call in test"

interpretTestFederation ::
  Sem (FederationAPIAccess FederatorClient ': r) a ->
  Sem r a
interpretTestFederation =
  interpret $ \case
    -- Mirrors the production federation failure path: getRemoteMLSClients
    -- throws the returned FederationError in client code, where
    -- getClientData hushes it into an inner 'Nothing'.
    RunFederatedEither _ _ -> pure (Left FederationNotImplemented)
    _ -> error "unexpected FederationAPIAccess call in test"

ownDomain :: Domain
ownDomain = Domain "example.com"

-- | The added user is remote, so the client-data fetch goes through the
-- federation path in both scenarios.
remoteDomain :: Domain
remoteDomain = Domain "other.example.com"

lConv :: Local ConvOrSubConv
lConv = toLocalUnsafe ownDomain convOrSub

convOrSub :: ConvOrSubConv
convOrSub =
  Conv
    MLSConversation
      { mcId = convId,
        mcMetadata = defConversationMetadata Nothing,
        mcMLSData = ConversationMLSData {cnvmlsGroupId = groupId, cnvmlsActiveData = Nothing},
        mcLocalMembers = [],
        mcRemoteMembers = [],
        mcMembers = mempty,
        mcIndexMap = mempty,
        mcMigrationState = MLSMigrationMLS
      }

-- | One user (not a conversation member) adding one client. With 'Nothing'
-- client data this user is classified unreachable. The user domain selects
-- the client-data fetch path: local via BrigAPIAccess, remote via the
-- federation path.
newCM :: Qualified UserId -> ClientMap (LeafIndex, Maybe KeyPackage)
newCM quser = ClientMap (Map.singleton quser (Map.singleton testClientId (0, Nothing)))

mkUuid :: String -> UUID.UUID
mkUuid = fromJust . UUID.fromString

userId :: UserId
userId = Id (mkUuid "00000000-0000-0000-0000-000000000001")

convId :: ConvId
convId = Id (mkUuid "00000000-0000-0000-0000-000000000002")

testClientId :: ClientId
testClientId = ClientId 3

groupId :: GroupId
groupId = GroupId "check-clients-spec"

csSuite :: CipherSuiteTag
csSuite = MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519

qUserId :: BrigBehavior -> Qualified UserId
qUserId = \case
  BrigCrashes -> Qualified userId ownDomain
  BrigUnused -> Qualified userId remoteDomain
