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
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.ConversationSubsystem.InterpreterSpec (spec) where

import Data.Default (def)
import Data.Domain (Domain (..))
import Data.Id
import Data.Map.Strict qualified as Map
import Data.Qualified
import Data.Tagged (Tagged)
import Data.UUID qualified as UUID
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..), Gen, arbitrary, chooseInt, counterexample, generate, ioProperty, vectorOf, (===))
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Config (ConversationSubsystemConfig (..))
import Wire.API.Conversation.Protocol (ConversationMLSData (..), Protocol (..))
import Wire.API.Conversation.Role hiding (DeleteConversation)
import Wire.API.Error.Galley (AdminlessConversation (..), GalleyError (..))
import Wire.API.Federation.Client (FederatorClient)
import Wire.API.Federation.Error (FederationError)
import Wire.API.Team.Feature (AllTeamFeatures, FeatureStatus (..), LockStatus (..), LockableFeature (..), PreventAdminlessGroupsConfig, npProject, npUpdate)
import Wire.API.User (AccountStatus (..), User (..), UserType (..), userId)
import Wire.BackendNotificationQueueAccess (BackendNotificationQueueAccess (..))
import Wire.BrigAPIAccess (BrigAPIAccess (..))
import Wire.ConversationStore (ConversationStore (..))
import Wire.ConversationSubsystem (RemoveMemberResponseMode (..))
import Wire.ConversationSubsystem.Update (removeMemberQualified)
import Wire.ExternalAccess (ExternalAccess (..))
import Wire.FeaturesConfigSubsystem (FeaturesConfigSubsystem (..))
import Wire.FederationAPIAccess (FederationAPIAccess (..))
import Wire.MockInterpreters.Now (defaultTime, interpretNowConst)
import Wire.MockInterpreters.TinyLog (noopLogger)
import Wire.NotificationSubsystem (NotificationSubsystem (..))
import Wire.ProposalStore (ProposalStore (..))
import Wire.Sem.Random (Random (..))
import Wire.StoredConversation
import Wire.TeamSubsystem (TeamSubsystem (..))

spec :: Spec
spec = focus $ describe "ConversationSubsystem.Interpreter" do
  prop "removeMemberQualified returns adminless-conversation error" $
    \convDomain
     teamId
     convId
     connId
     (MemberInputs localUserIds appUserIds botUserIds remoteMembers) ->
        ioProperty $ do
          let lusr = toLocalUnsafe convDomain leavingUserId
              qcnv = Qualified convId convDomain
              qvictim = Qualified leavingUserId convDomain
          users <-
            sequence
              ( fmap (mkUser convDomain UserTypeRegular) localUserIds
                  <> fmap (mkUser convDomain UserTypeApp) appUserIds
                  <> fmap (mkUser convDomain UserTypeBot) botUserIds
              )
          let conv =
                StoredConversation
                  { id_ = convId,
                    localMembers =
                      newMemberWithRole (leavingUserId, roleNameWireAdmin)
                        : [ newMemberWithRole (uid, roleNameWireMember)
                          | uid <- localUserIds <> appUserIds <> botUserIds
                          ],
                    remoteMembers = remoteMembers,
                    metadata = (defConversationMetadata (Just leavingUserId)) {cnvmTeam = Just teamId},
                    protocol = ProtocolMLS (ConversationMLSData (GroupId "mock-group-id") Nothing)
                  }
              features =
                npUpdate @PreventAdminlessGroupsConfig
                  (LockableFeature FeatureStatusEnabled LockStatusUnlocked def)
                  def
              expectedEligible =
                [ Qualified uid convDomain
                | uid <- localUserIds,
                  uid /= leavingUserId
                ]
              result =
                run
                  . runError @AdminlessConversation
                  . runError @(Tagged ('ActionDenied 'RemoveConversationMember) ())
                  . runError @(Tagged 'ConvNotFound ())
                  . runError @(Tagged 'InvalidOperation ())
                  . runError @FederationError
                  . runInputConst
                    ConversationSubsystemConfig
                      { mlsKeys = Nothing,
                        federationProtocols = Nothing,
                        legalholdDefaults = def,
                        maxConvSize = 500,
                        listClientsUsingBrig = False
                      }
                  . interpretConversationStore (Map.singleton convId conv)
                  . interpretBrig users
                  . interpretFeatures features
                  . interpretBackendNotificationQueueAccess
                  . interpretFederation
                  . interpretExternalAccess
                  . interpretNotificationSubsystem
                  . interpretProposalStore
                  . interpretTeamSubsystem
                  . interpretNowConst defaultTime
                  . interpretRandom
                  . noopLogger
                  $ removeMemberQualified RemoveMemberEligibleMembersResponse lusr connId qcnv qvictim
          pure $
            case result of
              Left err ->
                err === AdminlessConversation {eligibleMembers = expectedEligible}
              Right _ ->
                counterexample ("expected adminless-conversation, got " <> show result) False

data MemberInputs = MemberInputs
  { localUserIds :: [UserId],
    appUserIds :: [UserId],
    botUserIds :: [UserId],
    remoteMembers :: [RemoteMember]
  }
  deriving stock (Show)

instance Arbitrary MemberInputs where
  arbitrary = do
    pool <- filter (/= leavingUserId) <$> genDistinctPool
    localCount <- chooseInt (1, 5)
    appCount <- chooseInt (0, 5)
    botCount <- chooseInt (0, 5)
    remoteCount <- chooseInt (0, 5)
    remoteDomains <- vectorOf remoteCount arbitrary
    let (localPool, pool1) = splitAt localCount pool
        (appPool, pool2) = splitAt appCount pool1
        (botPool, pool3) = splitAt botCount pool2
        (remotePool, _) = splitAt remoteCount pool3
    pure
      MemberInputs
        { localUserIds = localPool,
          appUserIds = appPool,
          botUserIds = botPool,
          remoteMembers =
            [ RemoteMember
                { id_ = toRemoteUnsafe dom uid,
                  convRoleName = roleNameWireMember
                }
            | (dom, uid) <- zip remoteDomains remotePool
            ]
        }

-- Build one lazy infinite pool of distinct IDs and slice it into categories.
-- Using position in the stream, rather than per-category prefixes, makes the
-- disjointness guarantee obvious in the test data itself.
genDistinctPool :: Gen [UserId]
genDistinctPool = do
  hi <- arbitrary
  lo <- arbitrary
  pure [mkUserIdFromWords hi lo ix | ix <- [0 ..]]

leavingUserId :: UserId
leavingUserId = mkUserId "00000000-0000-0000-0000-000000000001"

interpretConversationStore ::
  Map.Map ConvId StoredConversation ->
  Sem (ConversationStore ': r) a ->
  Sem r a
interpretConversationStore store =
  interpret $ \case
    GetConversation cid -> pure (Map.lookup cid store)
    _ -> error "unexpected ConversationStore call in test"

interpretBrig ::
  [User] ->
  Sem (BrigAPIAccess ': r) a ->
  Sem r a
interpretBrig users =
  interpret $ \case
    GetUsers uids -> pure [u | u <- users, userId u `elem` uids]
    _ -> error "unexpected BrigAPIAccess call in test"

interpretFeatures ::
  AllTeamFeatures ->
  Sem (FeaturesConfigSubsystem ': r) a ->
  Sem r a
interpretFeatures features =
  interpret $ \case
    GetFeatureForTeam _ -> pure $ npProject features
    _ -> error "unexpected FeaturesConfigSubsystem call in test"

interpretFederation ::
  Sem (FederationAPIAccess FederatorClient ': r) a ->
  Sem r a
interpretFederation =
  interpret $ \case
    IsFederationConfigured -> pure False
    _ -> error "unexpected FederationAPIAccess call in test"

interpretExternalAccess ::
  Sem (ExternalAccess ': r) a ->
  Sem r a
interpretExternalAccess =
  interpret $ \case
    Deliver _ -> pure []
    DeliverAsync _ -> pure ()
    DeliverAndDeleteAsync _ _ -> pure ()

interpretNotificationSubsystem ::
  Sem (NotificationSubsystem ': r) a ->
  Sem r a
interpretNotificationSubsystem =
  interpret $ \case
    PushNotifications _ -> pure ()
    PushNotificationsSlowly _ -> pure ()
    PushNotificationAsync _ -> error "unexpected NotificationSubsystem call in test"
    CleanupUser _ -> pure ()
    UnregisterPushClient _ _ -> pure ()
    GetPushTokens _ -> pure []
    SetupConsumableNotifications _ _ -> pure ()

interpretBackendNotificationQueueAccess ::
  Sem (BackendNotificationQueueAccess ': r) a ->
  Sem r a
interpretBackendNotificationQueueAccess =
  interpret $ \case
    _ -> error "unexpected BackendNotificationQueueAccess call in test"

interpretProposalStore ::
  Sem (ProposalStore ': r) a ->
  Sem r a
interpretProposalStore =
  interpret $ \case
    StoreProposal _ _ _ -> pure ()
    GetProposal _ _ _ -> pure Nothing
    GetAllPendingProposalRefs _ _ -> pure []
    GetAllPendingProposals _ _ -> pure []
    DeleteAllProposals _ -> pure ()

interpretTeamSubsystem ::
  Sem (TeamSubsystem ': r) a ->
  Sem r a
interpretTeamSubsystem =
  interpret $ \case
    _ -> error "unexpected TeamSubsystem call in test"

interpretRandom ::
  Sem (Random ': r) a ->
  Sem r a
interpretRandom =
  interpret $ \case
    _ -> error "unexpected Random call in test"

mkUserId :: String -> UserId
mkUserId = Id . fromJust . UUID.fromString

mkUserIdFromWords :: Word32 -> Word32 -> Int -> UserId
mkUserIdFromWords prefix salt idx =
  Id $
    UUID.fromWords prefix salt 0 (fromIntegral idx)

mkUser :: Domain -> UserType -> UserId -> IO User
mkUser domain utype uid = do
  base <- generate arbitrary
  pure
    base
      { userQualifiedId = Qualified uid domain,
        userType = utype,
        userStatus = Active,
        userService = Nothing
      }
