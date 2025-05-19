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

module Galley.API.Action
  ( -- * Conversation action types
    ConversationActionTag (..),
    ConversationJoin (..),
    ConversationMemberUpdate (..),
    HasConversationActionEffects,
    HasConversationActionGalleyErrors,

    -- * Performing actions
    updateLocalConversation,
    updateLocalConversationUnchecked,
    NoChanges (..),
    LocalConversationUpdate (..),
    notifyTypingIndicator,
    pushTypingIndicatorEvents,

    -- * Utilities
    addMembersToLocalConversation,
    notifyConversationAction,
    updateLocalStateOfRemoteConv,
    addLocalUsersToRemoteConv,
    kickMember,
    ConversationUpdate,
    getFederationStatus,
    enforceFederationProtocol,
    checkFederationStatus,
    firstConflictOrFullyConnected,
  )
where

import Control.Arrow ((&&&))
import Control.Error (headMay)
import Control.Lens
import Data.ByteString.Conversion (toByteString')
import Data.Default
import Data.Domain (Domain (..))
import Data.Id
import Data.Json.Util
import Data.Kind
import Data.List qualified as List
import Data.List.Extra (nubOrd)
import Data.List.NonEmpty (nonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Misc
import Data.Qualified
import Data.Set ((\\))
import Data.Set qualified as Set
import Data.Singletons
import Data.Time.Clock
import Galley.API.Error
import Galley.API.MLS.Conversation
import Galley.API.MLS.Migration
import Galley.API.MLS.Removal
import Galley.API.Teams.Features.Get
import Galley.API.Util
import Galley.Data.Conversation
import Galley.Data.Conversation qualified as Data
import Galley.Data.Conversation.Types
import Galley.Data.Scope (Scope (ReusableCode))
import Galley.Data.Services
import Galley.Effects
import Galley.Effects.BackendNotificationQueueAccess
import Galley.Effects.BotAccess qualified as E
import Galley.Effects.BrigAccess qualified as E
import Galley.Effects.CodeStore qualified as E
import Galley.Effects.ConversationStore qualified as E
import Galley.Effects.FederatorAccess qualified as E
import Galley.Effects.FireAndForget qualified as E
import Galley.Effects.MemberStore qualified as E
import Galley.Effects.ProposalStore qualified as E
import Galley.Effects.SubConversationStore qualified as E
import Galley.Effects.TeamStore qualified as E
import Galley.Env (Env)
import Galley.Options
import Galley.Types.Conversations.Members
import Galley.Types.UserList
import Galley.Validation
import Imports hiding ((\\))
import Network.AMQP qualified as Q
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog
import Polysemy.TinyLog qualified as P
import System.Logger qualified as Log
import Wire.API.Connection (Relation (Accepted))
import Wire.API.Conversation hiding (Conversation, Member)
import Wire.API.Conversation qualified as AddPermission
import Wire.API.Conversation.Action
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.Event.LeaveReason
import Wire.API.Federation.API
import Wire.API.Federation.API.Brig
import Wire.API.Federation.API.Galley
import Wire.API.Federation.API.Galley qualified as F
import Wire.API.Federation.Error
import Wire.API.FederationStatus
import Wire.API.Push.V2 qualified as PushV2
import Wire.API.Routes.Internal.Brig.Connection
import Wire.API.Team.Feature
import Wire.API.Team.LegalHold
import Wire.API.Team.Member
import Wire.API.Team.Permission (Perm (AddRemoveConvMember, ModifyConvName))
import Wire.API.User as User
import Wire.NotificationSubsystem

data NoChanges = NoChanges

type family HasConversationActionEffects (tag :: ConversationActionTag) r :: Constraint where
  HasConversationActionEffects 'ConversationJoinTag r =
    ( Member BrigAccess r,
      Member (Error FederationError) r,
      Member (Error InternalError) r,
      Member (ErrorS 'NotATeamMember) r,
      Member (ErrorS 'NotConnected) r,
      Member (ErrorS ('ActionDenied 'LeaveConversation)) r,
      Member (ErrorS ('ActionDenied 'AddConversationMember)) r,
      Member (ErrorS 'InvalidOperation) r,
      Member (ErrorS 'ConvAccessDenied) r,
      Member (ErrorS 'ConvNotFound) r,
      Member (ErrorS 'TooManyMembers) r,
      Member (ErrorS 'MissingLegalholdConsent) r,
      Member (Error NonFederatingBackends) r,
      Member (Error UnreachableBackends) r,
      Member ExternalAccess r,
      Member FederatorAccess r,
      Member NotificationSubsystem r,
      Member (Input Env) r,
      Member (Input Opts) r,
      Member (Input UTCTime) r,
      Member LegalHoldStore r,
      Member MemberStore r,
      Member ProposalStore r,
      Member Random r,
      Member SubConversationStore r,
      Member TeamStore r,
      Member TinyLog r,
      Member ConversationStore r,
      Member (Error NoChanges) r
    )
  HasConversationActionEffects 'ConversationLeaveTag r =
    ( Member MemberStore r,
      Member (Error InternalError) r,
      Member (Error NoChanges) r,
      Member ExternalAccess r,
      Member FederatorAccess r,
      Member NotificationSubsystem r,
      Member (Input UTCTime) r,
      Member (Input Env) r,
      Member ProposalStore r,
      Member SubConversationStore r,
      Member Random r,
      Member TinyLog r
    )
  HasConversationActionEffects 'ConversationRemoveMembersTag r =
    ( Member MemberStore r,
      Member (Error NoChanges) r,
      Member SubConversationStore r,
      Member ProposalStore r,
      Member (Input Env) r,
      Member (Input UTCTime) r,
      Member ExternalAccess r,
      Member FederatorAccess r,
      Member NotificationSubsystem r,
      Member (Error InternalError) r,
      Member Random r,
      Member TinyLog r,
      Member (Error NoChanges) r
    )
  HasConversationActionEffects 'ConversationMemberUpdateTag r =
    ( Member MemberStore r,
      Member (ErrorS 'ConvMemberNotFound) r
    )
  HasConversationActionEffects 'ConversationDeleteTag r =
    ( Member BrigAccess r,
      Member CodeStore r,
      Member ConversationStore r,
      Member (Error FederationError) r,
      Member (ErrorS 'NotATeamMember) r,
      Member FederatorAccess r,
      Member MemberStore r,
      Member ProposalStore r,
      Member SubConversationStore r,
      Member TeamStore r
    )
  HasConversationActionEffects 'ConversationRenameTag r =
    ( Member (Error InvalidInput) r,
      Member ConversationStore r,
      Member TeamStore r,
      Member (ErrorS InvalidOperation) r
    )
  HasConversationActionEffects 'ConversationAccessDataTag r =
    ( Member BotAccess r,
      Member BrigAccess r,
      Member CodeStore r,
      Member (Error InternalError) r,
      Member (Error InvalidInput) r,
      Member (Error NoChanges) r,
      Member (ErrorS 'InvalidTargetAccess) r,
      Member (ErrorS ('ActionDenied 'RemoveConversationMember)) r,
      Member ExternalAccess r,
      Member FederatorAccess r,
      Member FireAndForget r,
      Member NotificationSubsystem r,
      Member (Input Env) r,
      Member MemberStore r,
      Member ProposalStore r,
      Member TeamStore r,
      Member TinyLog r,
      Member (Input UTCTime) r,
      Member ConversationStore r,
      Member SubConversationStore r,
      Member Random r
    )
  HasConversationActionEffects 'ConversationMessageTimerUpdateTag r =
    ( Member ConversationStore r,
      Member (Error NoChanges) r
    )
  HasConversationActionEffects 'ConversationReceiptModeUpdateTag r =
    ( Member ConversationStore r,
      Member (Error NoChanges) r,
      Member (ErrorS MLSReadReceiptsNotAllowed) r
    )
  HasConversationActionEffects 'ConversationUpdateProtocolTag r =
    ( Member ConversationStore r,
      Member (ErrorS 'ConvInvalidProtocolTransition) r,
      Member (ErrorS 'MLSMigrationCriteriaNotSatisfied) r,
      Member (Error NoChanges) r,
      Member BrigAccess r,
      Member ExternalAccess r,
      Member FederatorAccess r,
      Member NotificationSubsystem r,
      Member (Input Env) r,
      Member (Input Opts) r,
      Member (Input UTCTime) r,
      Member MemberStore r,
      Member ProposalStore r,
      Member Random r,
      Member SubConversationStore r,
      Member TeamFeatureStore r,
      Member TinyLog r
    )
  HasConversationActionEffects 'ConversationUpdateAddPermissionTag r =
    ( Member (Error NoChanges) r,
      Member ConversationStore r,
      Member (ErrorS 'InvalidTargetAccess) r
    )

type family HasConversationActionGalleyErrors (tag :: ConversationActionTag) :: EffectRow where
  HasConversationActionGalleyErrors 'ConversationJoinTag =
    '[ ErrorS ('ActionDenied 'LeaveConversation),
       ErrorS ('ActionDenied 'AddConversationMember),
       ErrorS 'NotATeamMember,
       ErrorS 'InvalidOperation,
       ErrorS 'ConvNotFound,
       ErrorS 'NotConnected,
       ErrorS 'ConvAccessDenied,
       ErrorS 'TooManyMembers,
       ErrorS 'MissingLegalholdConsent
     ]
  HasConversationActionGalleyErrors 'ConversationLeaveTag =
    '[ ErrorS ('ActionDenied 'LeaveConversation),
       ErrorS 'InvalidOperation,
       ErrorS 'ConvNotFound
     ]
  HasConversationActionGalleyErrors 'ConversationRemoveMembersTag =
    '[ ErrorS ('ActionDenied 'RemoveConversationMember),
       ErrorS 'InvalidOperation,
       ErrorS 'ConvNotFound
     ]
  HasConversationActionGalleyErrors 'ConversationMemberUpdateTag =
    '[ ErrorS ('ActionDenied 'ModifyOtherConversationMember),
       ErrorS 'InvalidOperation,
       ErrorS 'ConvNotFound,
       ErrorS 'ConvMemberNotFound
     ]
  HasConversationActionGalleyErrors 'ConversationDeleteTag =
    '[ ErrorS ('ActionDenied 'DeleteConversation),
       ErrorS 'NotATeamMember,
       ErrorS 'InvalidOperation,
       ErrorS 'ConvNotFound
     ]
  HasConversationActionGalleyErrors 'ConversationRenameTag =
    '[ ErrorS ('ActionDenied 'ModifyConversationName),
       ErrorS 'InvalidOperation,
       ErrorS 'ConvNotFound
     ]
  HasConversationActionGalleyErrors 'ConversationMessageTimerUpdateTag =
    '[ ErrorS ('ActionDenied 'ModifyConversationMessageTimer),
       ErrorS 'InvalidOperation,
       ErrorS 'ConvNotFound
     ]
  HasConversationActionGalleyErrors 'ConversationReceiptModeUpdateTag =
    '[ ErrorS ('ActionDenied 'ModifyConversationReceiptMode),
       ErrorS 'InvalidOperation,
       ErrorS 'MLSReadReceiptsNotAllowed,
       ErrorS 'ConvNotFound
     ]
  HasConversationActionGalleyErrors 'ConversationAccessDataTag =
    '[ ErrorS ('ActionDenied 'RemoveConversationMember),
       ErrorS ('ActionDenied 'ModifyConversationAccess),
       ErrorS 'InvalidOperation,
       ErrorS 'InvalidTargetAccess,
       ErrorS 'ConvNotFound
     ]
  HasConversationActionGalleyErrors 'ConversationUpdateProtocolTag =
    '[ ErrorS ('ActionDenied 'LeaveConversation),
       ErrorS 'InvalidOperation,
       ErrorS 'ConvNotFound,
       ErrorS 'ConvInvalidProtocolTransition,
       ErrorS 'MLSMigrationCriteriaNotSatisfied,
       ErrorS 'NotATeamMember,
       ErrorS OperationDenied,
       ErrorS 'TeamNotFound
     ]
  HasConversationActionGalleyErrors 'ConversationUpdateAddPermissionTag =
    '[ ErrorS ('ActionDenied 'ModifyAddPermission),
       ErrorS 'InvalidOperation,
       ErrorS 'ConvNotFound,
       ErrorS 'NotATeamMember,
       ErrorS OperationDenied,
       ErrorS 'TeamNotFound,
       ErrorS 'InvalidTargetAccess
     ]

enforceFederationProtocol ::
  ( Member (Error FederationError) r,
    Member (Input Opts) r
  ) =>
  ProtocolTag ->
  [Remote ()] ->
  Sem r ()
enforceFederationProtocol proto domains = do
  unless (null domains) $ do
    mAllowedProtos <- view (settings . federationProtocols) <$> input
    unless (maybe True (elem proto) mAllowedProtos) $
      throw FederationDisabledForProtocol

checkFederationStatus ::
  ( Member (Error UnreachableBackends) r,
    Member (Error NonFederatingBackends) r,
    Member FederatorAccess r
  ) =>
  RemoteDomains ->
  Sem r ()
checkFederationStatus req = do
  status <- getFederationStatus req
  case status of
    FullyConnected -> pure ()
    NotConnectedDomains dom1 dom2 -> throw (NonFederatingBackends dom1 dom2)

getFederationStatus ::
  ( Member (Error UnreachableBackends) r,
    Member FederatorAccess r
  ) =>
  RemoteDomains ->
  Sem r FederationStatus
getFederationStatus req = do
  fmap firstConflictOrFullyConnected
    . (ensureNoUnreachableBackends =<<)
    $ E.runFederatedConcurrentlyEither
      (Set.toList req.rdDomains)
      ( \qds ->
          fedClient @'Brig @"get-not-fully-connected-backends"
            (DomainSet . Set.map tDomain $ void qds `Set.delete` req.rdDomains)
      )

-- | "conflict" here means two remote domains that we are connected to
-- but are not connected to each other.
firstConflictOrFullyConnected :: [Remote NonConnectedBackends] -> FederationStatus
firstConflictOrFullyConnected =
  maybe
    FullyConnected
    (uncurry NotConnectedDomains)
    . headMay
    . mapMaybe toMaybeConflict
  where
    toMaybeConflict :: Remote NonConnectedBackends -> Maybe (Domain, Domain)
    toMaybeConflict r =
      headMay (Set.toList (nonConnectedBackends (tUnqualified r))) <&> (tDomain r,)

noChanges :: (Member (Error NoChanges) r) => Sem r a
noChanges = throw NoChanges

ensureAllowed ::
  forall tag mem r x.
  ( IsConvMember mem,
    HasConversationActionEffects tag r,
    Member (ErrorS ConvNotFound) r
  ) =>
  Sing tag ->
  Local x ->
  ConversationAction tag ->
  Conversation ->
  ConvOrTeamMember mem ->
  Sem r ()
ensureAllowed tag _ action conv (TeamMember tm) = do
  case tag of
    SConversationJoinTag -> do
      case action of
        ConversationJoin _ _ InternalAdd -> throwS @'ConvNotFound
        ConversationJoin _ _ ExternalAdd -> ensureChannelAndTeamAdmin conv tm
    _ -> throwS @'ConvNotFound
ensureAllowed tag loc action conv (ConvMember origUser) = do
  case tag of
    SConversationJoinTag ->
      mapErrorS @'InvalidAction @('ActionDenied 'AddConversationMember) $ do
        ensureConvRoleNotElevated origUser (role action)
        checkGroupIdSupport loc conv action
    SConversationDeleteTag ->
      for_ (convTeam conv) $ \tid -> do
        lusr <- ensureLocal loc (convMemberId loc origUser)
        void $ E.getTeamMember tid (tUnqualified lusr) >>= noteS @'NotATeamMember
    SConversationAccessDataTag -> do
      -- 'PrivateAccessRole' is for self-conversations, 1:1 conversations and
      -- so on; users not supposed to be able to make other conversations
      -- have 'PrivateAccessRole'
      when (PrivateAccess `elem` cupAccess action || Set.null (cupAccessRoles action)) $
        throwS @'InvalidTargetAccess
      -- Team conversations incur another round of checks
      case convTeam conv of
        Just _ -> do
          -- Access mode change might result in members being removed from the
          -- conversation, so the user must have the necessary permission flag
          ensureActionAllowed SRemoveConversationMember origUser
        Nothing ->
          -- not a team conv, so one of the other access roles has to allow this.
          when (Set.null $ cupAccessRoles action Set.\\ Set.fromList [TeamMemberAccessRole]) $
            throwS @'InvalidTargetAccess
    SConversationUpdateAddPermissionTag -> do
      unless (conv.convMetadata.cnvmGroupConvType == Just Channel) $ throwS @'InvalidTargetAccess
    SConversationReceiptModeUpdateTag -> do
      -- cannot update receipt mode of MLS conversations
      when (convProtocolTag conv == ProtocolMLSTag) $
        throwS @MLSReadReceiptsNotAllowed
    _ -> pure ()

-- | Returns additional members that resulted from the action (e.g. ConversationJoin)
-- and also returns the (possible modified) action that was performed
performAction ::
  forall tag r.
  ( HasConversationActionEffects tag r,
    Member BackendNotificationQueueAccess r,
    Member (Error FederationError) r
  ) =>
  Sing tag ->
  Qualified UserId ->
  Local Conversation ->
  ConversationAction tag ->
  Sem r (BotsAndMembers, ConversationAction tag)
performAction tag origUser lconv action = do
  let lcnv = fmap (.convId) lconv
      conv = tUnqualified lconv
  case tag of
    SConversationJoinTag ->
      performConversationJoin origUser lconv action
    SConversationLeaveTag -> do
      let victims = [origUser]
      lconv' <- traverse (convDeleteMembers (toUserList lconv victims)) lconv
      -- send remove proposals in the MLS case
      traverse_ (removeUser lconv' RemoveUserIncludeMain) victims
      pure (mempty, action)
    SConversationRemoveMembersTag -> do
      let presentVictims = filter (isConvMemberL lconv) (toList . crmTargets $ action)
      when (null presentVictims) noChanges
      traverse_ (convDeleteMembers (toUserList lconv presentVictims)) lconv
      -- send remove proposals in the MLS case
      traverse_ (removeUser lconv RemoveUserExcludeMain) presentVictims
      pure (mempty, action) -- FUTUREWORK: should we return the filtered action here?
    SConversationMemberUpdateTag -> do
      void $ ensureOtherMember lconv (cmuTarget action) conv
      E.setOtherMember lcnv (cmuTarget action) (cmuUpdate action)
      pure (mempty, action)
    SConversationDeleteTag -> do
      let deleteGroup groupId = do
            E.removeAllMLSClients groupId
            E.deleteAllProposals groupId

      let cid = conv.convId
      for_ (conv & mlsMetadata <&> cnvmlsGroupId . fst) $ \gidParent -> do
        sconvs <- E.listSubConversations cid
        for_ (Map.assocs sconvs) $ \(subid, mlsData) -> do
          let gidSub = cnvmlsGroupId mlsData
          E.deleteSubConversation cid subid
          deleteGroup gidSub
        deleteGroup gidParent

      key <- E.makeKey (tUnqualified lcnv)
      E.deleteCode key ReusableCode
      case convTeam conv of
        Nothing -> E.deleteConversation (tUnqualified lcnv)
        Just tid -> E.deleteTeamConversation tid (tUnqualified lcnv)

      pure (mempty, action)
    SConversationRenameTag -> do
      zusrMembership <- join <$> forM (cnvmTeam (convMetadata conv)) (flip E.getTeamMember (qUnqualified origUser))
      for_ zusrMembership $ \tm -> unless (tm `hasPermission` ModifyConvName) $ throwS @'InvalidOperation
      cn <- rangeChecked (cupName action)
      E.setConversationName (tUnqualified lcnv) cn
      pure (mempty, action)
    SConversationMessageTimerUpdateTag -> do
      when (Data.convMessageTimer conv == cupMessageTimer action) noChanges
      E.setConversationMessageTimer (tUnqualified lcnv) (cupMessageTimer action)
      pure (mempty, action)
    SConversationReceiptModeUpdateTag -> do
      when (Data.convReceiptMode conv == Just (cruReceiptMode action)) noChanges
      E.setConversationReceiptMode (tUnqualified lcnv) (cruReceiptMode action)
      pure (mempty, action)
    SConversationAccessDataTag -> do
      (bm, act) <- performConversationAccessData origUser lconv action
      pure (bm, act)
    SConversationUpdateProtocolTag -> do
      case (protocolTag (convProtocol (tUnqualified lconv)), action, convTeam (tUnqualified lconv)) of
        (ProtocolProteusTag, ProtocolMixedTag, Just _) -> do
          E.updateToMixedProtocol lcnv (convType (tUnqualified lconv))
          pure (mempty, action)
        (ProtocolMixedTag, ProtocolMLSTag, Just tid) -> do
          mig <- getFeatureForTeam @MlsMigrationConfig tid
          now <- input
          mlsConv <- mkMLSConversation conv >>= noteS @'ConvInvalidProtocolTransition
          ok <- checkMigrationCriteria now mlsConv mig
          unless ok $ throwS @'MLSMigrationCriteriaNotSatisfied
          removeExtraneousClients origUser lconv
          E.updateToMLSProtocol lcnv
          pure (mempty, action)
        (ProtocolProteusTag, ProtocolProteusTag, _) ->
          noChanges
        (ProtocolMixedTag, ProtocolMixedTag, _) ->
          noChanges
        (ProtocolMLSTag, ProtocolMLSTag, _) ->
          noChanges
        (_, _, _) -> throwS @'ConvInvalidProtocolTransition
    SConversationUpdateAddPermissionTag -> do
      when (conv.convMetadata.cnvmChannelAddPermission == Just (addPermission action)) noChanges
      E.updateChannelAddPermissions (tUnqualified lcnv) (addPermission action)
      pure (mempty, action)

performConversationJoin ::
  forall r.
  ( HasConversationActionEffects 'ConversationJoinTag r,
    Member BackendNotificationQueueAccess r
  ) =>
  Qualified UserId ->
  Local Conversation ->
  ConversationJoin ->
  Sem r (BotsAndMembers, ConversationJoin)
performConversationJoin qusr lconv (ConversationJoin invited role joinType) = do
  let newMembers = ulNewMembers lconv conv . toUserList lconv $ invited

  lusr <- ensureLocal lconv qusr
  ensureMemberLimit (convProtocolTag conv) (toList (convLocalMembers conv)) newMembers
  ensureAccess conv InviteAccess
  checkLocals lusr (convTeam conv) (ulLocals newMembers)
  enforceFederationProtocol (protocolTag conv.convProtocol) (fmap void (ulRemotes newMembers))
  checkRemotes lusr (ulRemotes newMembers)
  checkLHPolicyConflictsLocal (ulLocals newMembers)
  checkLHPolicyConflictsRemote (FutureWork (ulRemotes newMembers))
  checkRemoteBackendsConnected lusr
  checkTeamMemberAddPermission lusr
  addMembersToLocalConversation (fmap (.convId) lconv) newMembers role joinType
  where
    checkRemoteBackendsConnected :: Local x -> Sem r ()
    checkRemoteBackendsConnected loc = do
      let invitedRemoteUsers = snd . partitionQualified loc . NE.toList $ invited
          invitedRemoteDomains = Set.fromList $ void <$> invitedRemoteUsers
          existingRemoteDomains = Set.fromList $ void . rmId <$> convRemoteMembers (tUnqualified lconv)
          allInvitedAlreadyInConversation = null $ invitedRemoteDomains \\ existingRemoteDomains

      if not allInvitedAlreadyInConversation
        then checkFederationStatus (RemoteDomains (invitedRemoteDomains <> existingRemoteDomains))
        else -- even if there are no new remotes, we still need to check they are reachable
          void . (ensureNoUnreachableBackends =<<) $
            E.runFederatedConcurrentlyEither @_ @'Brig invitedRemoteUsers $ \_ ->
              pure ()

    conv :: Data.Conversation
    conv = tUnqualified lconv

    checkLocals ::
      Local UserId ->
      Maybe TeamId ->
      [UserId] ->
      Sem r ()
    checkLocals lusr (Just tid) newUsers = do
      tms <-
        Map.fromList . map (view Wire.API.Team.Member.userId &&& Imports.id)
          <$> E.selectTeamMembers tid newUsers
      let userMembershipMap = map (Imports.id &&& flip Map.lookup tms) newUsers
      ensureAccessRole (convAccessRoles conv) userMembershipMap
      ensureConnectedToLocalsOrSameTeam lusr newUsers
    checkLocals lusr Nothing newUsers = do
      ensureAccessRole (convAccessRoles conv) (map (,Nothing) newUsers)
      ensureConnectedToLocalsOrSameTeam lusr newUsers

    checkRemotes ::
      Local UserId ->
      [Remote UserId] ->
      Sem r ()
    checkRemotes lusr remotes = do
      -- if federator is not configured, we fail early, so we avoid adding
      -- remote members to the database
      unless (null remotes) $
        unlessM E.isFederationConfigured $
          throw FederationNotConfigured
      ensureConnectedToRemotes lusr remotes

    checkLHPolicyConflictsLocal ::
      [UserId] ->
      Sem r ()
    checkLHPolicyConflictsLocal newUsers = do
      let convUsers = convLocalMembers conv

      allNewUsersGaveConsent <- allLegalholdConsentGiven newUsers

      whenM (anyLegalholdActivated (lmId <$> convUsers)) $
        unless allNewUsersGaveConsent $
          throwS @'MissingLegalholdConsent

      whenM (anyLegalholdActivated newUsers) $ do
        unless allNewUsersGaveConsent $
          throwS @'MissingLegalholdConsent

        convUsersLHStatus <- do
          uidsStatus <- getLHStatusForUsers (lmId <$> convUsers)
          pure $ zipWith (\mem (_, status) -> (mem, status)) convUsers uidsStatus

        if any
          ( \(mem, status) ->
              lmConvRoleName mem == roleNameWireAdmin
                && consentGiven status == ConsentGiven
          )
          convUsersLHStatus
          then do
            for_ convUsersLHStatus $ \(mem, status) ->
              when (consentGiven status == ConsentNotGiven) $ do
                kickMember
                  qusr
                  lconv
                  (convBotsAndMembers (tUnqualified lconv))
                  (tUntagged (qualifyAs lconv (lmId mem)))
          else throwS @'MissingLegalholdConsent

    checkLHPolicyConflictsRemote ::
      FutureWork 'LegalholdPlusFederationNotImplemented [Remote UserId] ->
      Sem r ()
    checkLHPolicyConflictsRemote _remotes = pure ()

    checkTeamMemberAddPermission :: Local UserId -> Sem r ()
    checkTeamMemberAddPermission lusr = do
      case conv.convMetadata.cnvmTeam of
        Just tid -> do
          maybeTeamMember <- E.getTeamMember tid (tUnqualified lusr)
          case maybeTeamMember of
            Just tm -> do
              let isChannel = conv.convMetadata.cnvmGroupConvType == Just Channel
                  isConversationAdmin =
                    maybe False (\m -> m.lmConvRoleName == roleNameWireAdmin) $
                      find (\m -> m.lmId == lusr.tUntagged.qUnqualified) conv.convLocalMembers
                  isAddPermissionEveryone = conv.convMetadata.cnvmChannelAddPermission == Just AddPermission.Everyone

              if isChannel
                then do
                  -- at this point we know the conversation is a channel, the user is a team member, and when:
                  -- - the user is a conversation admin (including external partners) => they can add members
                  --   note: external partners can be allowed to create channels, in which case they will always be the channel's admin
                  -- - or the add-permission is set to everyone (including exteral partners) => they can add members
                  -- - or the user is a team admin => they can add members
                  unless (isConversationAdmin || isAddPermissionEveryone || isAdminOrOwner (tm ^. permissions)) $ throwS @'InvalidOperation
                else do
                  -- we know this is a group conversation and the user is a team member and they are conversation admin.
                  -- if they do not have the add/remove permission (which is currently only the case for external partners) they are not allowed to add members
                  -- note: it is a bit counterintuitive that external partners who are conversation admins are not allowed to add members,
                  -- while guests (non-team members) who are conversation admins are allowed to add members
                  unless (tm `hasPermission` AddRemoveConvMember) $ throwS @'InvalidOperation

            -- at this point we know this is a team conversation and the user is not a team member (guest).
            -- but the user is a conversation admin (which has been checked earlier) so they are allowed to add members
            Nothing -> pure ()
        -- this is not a team conversation and conv admin permissions have been checked earlier
        Nothing -> pure ()

performConversationAccessData ::
  ( HasConversationActionEffects 'ConversationAccessDataTag r,
    Member (Error FederationError) r,
    Member BackendNotificationQueueAccess r
  ) =>
  Qualified UserId ->
  Local Conversation ->
  ConversationAccessData ->
  Sem r (BotsAndMembers, ConversationAccessData)
performConversationAccessData qusr lconv action = do
  when (convAccessData conv == action) noChanges
  -- Remove conversation codes if CodeAccess is revoked
  when
    ( CodeAccess `elem` convAccess conv
        && CodeAccess `notElem` cupAccess action
    )
    $ do
      key <- E.makeKey (tUnqualified lcnv)
      E.deleteCode key ReusableCode

  -- Determine bots and members to be removed
  let filterBotsAndMembers =
        maybeRemoveBots >=> maybeRemoveGuests >=> maybeRemoveNonTeamMembers >=> maybeRemoveTeamMembers
  let current = convBotsAndMembers conv -- initial bots and members
  desired <- filterBotsAndMembers current -- desired bots and members
  let toRemove = bmDiff current desired -- bots and members to be removed

  -- Update Cassandra
  E.setConversationAccess (tUnqualified lcnv) action
  E.fireAndForget $ do
    -- Remove bots
    traverse_ (E.deleteBot (tUnqualified lcnv) . botMemId) (bmBots toRemove)

    -- Update current bots and members
    -- current bots and members but only desired bots
    let bmToNotify = current {bmBots = bmBots desired}

    -- Remove users and notify everyone
    for_ (bmQualifiedMembers lcnv toRemove) $
      kickMember qusr lconv bmToNotify

  pure (mempty, action)
  where
    lcnv = fmap (.convId) lconv
    conv = tUnqualified lconv

    maybeRemoveBots :: BotsAndMembers -> Sem r BotsAndMembers
    maybeRemoveBots bm =
      if Set.member ServiceAccessRole (cupAccessRoles action)
        then pure bm
        else pure $ bm {bmBots = mempty}

    maybeRemoveGuests :: (Member BrigAccess r) => BotsAndMembers -> Sem r BotsAndMembers
    maybeRemoveGuests bm =
      if Set.member GuestAccessRole (cupAccessRoles action)
        then pure bm
        else do
          activated <- map User.userId <$> E.lookupActivatedUsers (toList (bmLocals bm))
          -- FUTUREWORK: should we also remove non-activated remote users?
          pure $ bm {bmLocals = Set.fromList activated}

    maybeRemoveNonTeamMembers :: (Member TeamStore r) => BotsAndMembers -> Sem r BotsAndMembers
    maybeRemoveNonTeamMembers bm =
      if Set.member NonTeamMemberAccessRole (cupAccessRoles action)
        then pure bm
        else case convTeam conv of
          Just tid -> do
            onlyTeamUsers <- filterM (fmap isJust . E.getTeamMember tid) (toList (bmLocals bm))
            pure $ bm {bmLocals = Set.fromList onlyTeamUsers, bmRemotes = mempty}
          Nothing -> pure bm

    maybeRemoveTeamMembers :: (Member TeamStore r) => BotsAndMembers -> Sem r BotsAndMembers
    maybeRemoveTeamMembers bm =
      if Set.member TeamMemberAccessRole (cupAccessRoles action)
        then pure bm
        else case convTeam conv of
          Just tid -> do
            noTeamMembers <- filterM (fmap isNothing . E.getTeamMember tid) (toList (bmLocals bm))
            pure $ bm {bmLocals = Set.fromList noTeamMembers}
          Nothing -> pure bm

data LocalConversationUpdate = LocalConversationUpdate
  { lcuEvent :: Event,
    lcuUpdate :: ConversationUpdate
  }
  deriving (Show)

updateLocalConversation ::
  forall tag r.
  ( Member BackendNotificationQueueAccess r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (ErrorS ('ActionDenied (ConversationActionPermission tag))) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'ConvNotFound) r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member (Input UTCTime) r,
    HasConversationActionEffects tag r,
    SingI tag,
    Member TeamStore r
  ) =>
  Local ConvId ->
  Qualified UserId ->
  Maybe ConnId ->
  ConversationAction tag ->
  Sem r LocalConversationUpdate
updateLocalConversation lcnv qusr con action = do
  let tag = sing @tag
  conv <- getConversationWithError lcnv
  -- check that the action does not bypass the underlying protocol
  unless (protocolValidAction (convProtocol conv) tag action) $
    throwS @'InvalidOperation
  -- perform all authorisation checks and, if successful, then update itself
  updateLocalConversationUnchecked @tag (qualifyAs lcnv conv) qusr con action

-- | Similar to 'updateLocalConversationWithLocalUser', but takes a
-- 'Conversation' value directly, instead of a 'ConvId', and skips protocol
-- checks. All the other checks are still performed.
--
-- This is intended to be used by protocol-aware code, once all the
-- protocol-specific checks and updates have been performed, to finally apply
-- the changes to the conversation as seen by the backend.
updateLocalConversationUnchecked ::
  forall tag r.
  ( SingI tag,
    Member BackendNotificationQueueAccess r,
    Member (Error FederationError) r,
    Member (ErrorS ('ActionDenied (ConversationActionPermission tag))) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member (Input UTCTime) r,
    HasConversationActionEffects tag r,
    Member TeamStore r
  ) =>
  Local Conversation ->
  Qualified UserId ->
  Maybe ConnId ->
  ConversationAction tag ->
  Sem r LocalConversationUpdate
updateLocalConversationUnchecked lconv qusr con action = do
  let lcnv = fmap (.convId) lconv
      conv = tUnqualified lconv
  mTeamMember <- foldQualified lconv (getTeamMembership conv) (const $ pure Nothing) qusr
  ensureConversationActionAllowed (sing @tag) lcnv conv mTeamMember
  (extraTargets, action') <- performAction (sing @tag) qusr lconv action
  notifyConversationAction
    (sing @tag)
    qusr
    False
    con
    lconv
    (convBotsAndMembers (tUnqualified lconv) <> extraTargets)
    action'
  where
    getTeamMembership :: Conversation -> Local UserId -> Sem r (Maybe TeamMember)
    getTeamMembership conv luid = maybe (pure Nothing) (`E.getTeamMember` tUnqualified luid) conv.convMetadata.cnvmTeam

    ensureConversationActionAllowed :: Sing tag -> Local x -> Conversation -> Maybe TeamMember -> Sem r ()
    ensureConversationActionAllowed tag loc conv mTeamMember = do
      self <-
        noteS @'ConvNotFound $
          ConvMember <$> getConvMember lconv conv (maybe (ConvMemberNoTeam qusr) ConvMemberTeam mTeamMember) <|> TeamMember <$> mTeamMember

      unless
        (skipConversationRoleCheck tag conv mTeamMember)
        case self of
          ConvMember mem -> ensureActionAllowed (sConversationActionPermission tag) mem
          -- TeamMember is a special case, which will be handled in ensureAllowed
          TeamMember _ -> pure ()

      -- check if it is a group conversation (except for rename actions)
      when (fromSing tag /= ConversationRenameTag) $
        ensureGroupConversation conv

      -- extra action-specific checks
      ensureAllowed tag loc action conv self

    skipConversationRoleCheck :: Sing tag -> Conversation -> Maybe TeamMember -> Bool
    skipConversationRoleCheck SConversationJoinTag conv (Just _) = conv.convMetadata.cnvmChannelAddPermission == Just AddPermission.Everyone
    skipConversationRoleCheck _ _ _ = False

-- --------------------------------------------------------------------------------
-- -- Utilities

-- | Add users to a conversation without performing any checks. Return extra
-- notification targets and the action performed.
addMembersToLocalConversation ::
  ( Member MemberStore r,
    Member (Error NoChanges) r
  ) =>
  Local ConvId ->
  UserList UserId ->
  RoleName ->
  JoinType ->
  Sem r (BotsAndMembers, ConversationJoin)
addMembersToLocalConversation lcnv users role joinType = do
  (lmems, rmems) <- E.createMembers (tUnqualified lcnv) (fmap (,role) users)
  neUsers <- note NoChanges $ nonEmpty (ulAll lcnv users)
  let action = ConversationJoin neUsers role joinType
  pure (bmFromMembers lmems rmems, action)

notifyConversationAction ::
  forall tag r.
  ( Member BackendNotificationQueueAccess r,
    Member ExternalAccess r,
    Member (Error FederationError) r,
    Member NotificationSubsystem r,
    Member (Input UTCTime) r
  ) =>
  Sing tag ->
  Qualified UserId ->
  Bool ->
  Maybe ConnId ->
  Local Conversation ->
  BotsAndMembers ->
  ConversationAction (tag :: ConversationActionTag) ->
  Sem r LocalConversationUpdate
notifyConversationAction tag quid notifyOrigDomain con lconv targets action = do
  now <- input
  let lcnv = fmap (.convId) lconv
      conv = tUnqualified lconv
      e = conversationActionToEvent tag now quid (tUntagged lcnv) Nothing action
      mkUpdate uids =
        ConversationUpdate
          now
          quid
          (tUnqualified lcnv)
          uids
          (SomeConversationAction tag action)
  update <-
    fmap (fromMaybe (mkUpdate []) . asum . map tUnqualified) $
      enqueueNotificationsConcurrently Q.Persistent (toList (bmRemotes targets)) $
        \ruids -> do
          let update = mkUpdate (tUnqualified ruids)
          -- if notifyOrigDomain is false, filter out user from quid's domain,
          -- because quid's backend will update local state and notify its users
          -- itself using the ConversationUpdate returned by this function
          if notifyOrigDomain || tDomain ruids /= qDomain quid
            then do
              makeConversationUpdateBundle update >>= sendBundle
              pure Nothing
            else pure (Just update)

  -- notify local participants and bots
  pushConversationEvent con conv e (qualifyAs lcnv (bmLocals targets)) (bmBots targets)

  -- return both the event and the 'ConversationUpdate' structure corresponding
  -- to the originating domain (if it is remote)
  pure $ LocalConversationUpdate e update

-- | Update the local database with information on conversation members joining
-- or leaving. Finally, push out notifications to local users.
updateLocalStateOfRemoteConv ::
  ( Member BrigAccess r,
    Member NotificationSubsystem r,
    Member ExternalAccess r,
    Member (Input (Local ())) r,
    Member MemberStore r,
    Member P.TinyLog r
  ) =>
  Remote F.ConversationUpdate ->
  Maybe ConnId ->
  Sem r (Maybe Event)
updateLocalStateOfRemoteConv rcu con = do
  loc <- qualifyLocal ()
  let cu = tUnqualified rcu
      rconvId = fmap (.convId) rcu
      qconvId = tUntagged rconvId

  -- Note: we generally do not send notifications to users that are not part of
  -- the conversation (from our point of view), to prevent spam from the remote
  -- backend. See also the comment below.
  (presentUsers, allUsersArePresent) <-
    E.selectRemoteMembers cu.alreadyPresentUsers rconvId

  -- Perform action, and determine extra notification targets.
  --
  -- When new users are being added to the conversation, we consider them as
  -- notification targets. Since we check connections before letting
  -- people being added, this is safe against spam. However, if users that
  -- are not in the conversations are being removed or have their membership state
  -- updated, we do **not** add them to the list of targets, because we have no
  -- way to make sure that they are actually supposed to receive that notification.

  (mActualAction, extraTargets) <- case cu.action of
    sca@(SomeConversationAction singTag action) -> case singTag of
      SConversationJoinTag -> do
        let ConversationJoin toAdd role joinType = action
        let (localUsers, remoteUsers) = partitionQualified loc toAdd
        addedLocalUsers <- Set.toList <$> addLocalUsersToRemoteConv rconvId cu.origUserId localUsers
        let allAddedUsers = map (tUntagged . qualifyAs loc) addedLocalUsers <> map tUntagged remoteUsers
        pure $
          ( fmap
              (\users -> SomeConversationAction SConversationJoinTag (ConversationJoin users role joinType))
              (nonEmpty allAddedUsers),
            addedLocalUsers
          )
      SConversationLeaveTag -> do
        let users = foldQualified loc (pure . tUnqualified) (const []) cu.origUserId
        E.deleteMembersInRemoteConversation rconvId users
        pure (Just sca, [])
      SConversationRemoveMembersTag -> do
        let localUsers = getLocalUsers (tDomain loc) . crmTargets $ action
        E.deleteMembersInRemoteConversation rconvId localUsers
        pure (Just sca, [])
      SConversationMemberUpdateTag ->
        pure (Just sca, [])
      SConversationDeleteTag -> do
        E.deleteMembersInRemoteConversation rconvId presentUsers
        pure (Just sca, [])
      SConversationRenameTag -> pure (Just sca, [])
      SConversationMessageTimerUpdateTag -> pure (Just sca, [])
      SConversationReceiptModeUpdateTag -> pure (Just sca, [])
      SConversationAccessDataTag -> pure (Just sca, [])
      SConversationUpdateProtocolTag -> pure (Just sca, [])
      SConversationUpdateAddPermissionTag -> pure (Just sca, [])

  unless allUsersArePresent $
    P.warn $
      Log.field "conversation" (toByteString' cu.convId)
        . Log.field "domain" (toByteString' (tDomain rcu))
        . Log.msg
          ( "Attempt to send notification about conversation update \
            \to users not in the conversation" ::
              ByteString
          )

  -- Send notifications
  for mActualAction $ \(SomeConversationAction tag action) -> do
    let event = conversationActionToEvent tag cu.time cu.origUserId qconvId Nothing action
        targets = nubOrd $ presentUsers <> extraTargets
    -- FUTUREWORK: support bots?
    pushConversationEvent con () event (qualifyAs loc targets) [] $> event

addLocalUsersToRemoteConv ::
  ( Member BrigAccess r,
    Member MemberStore r,
    Member P.TinyLog r
  ) =>
  Remote ConvId ->
  Qualified UserId ->
  [UserId] ->
  Sem r (Set UserId)
addLocalUsersToRemoteConv remoteConvId qAdder localUsers = do
  connStatus <- E.getConnections localUsers (Just [qAdder]) (Just Accepted)
  let localUserIdsSet = Set.fromList localUsers
      adder = qUnqualified qAdder
      -- If alice@A creates a 1-1 conversation on B, it can appear as if alice is
      -- adding herself to a remote conversation. To make sure this is allowed, we
      -- always consider a user as connected to themself.
      connected =
        Set.fromList (fmap csv2From connStatus)
          <> if Set.member adder localUserIdsSet
            then Set.singleton adder
            else mempty
      unconnected = Set.difference localUserIdsSet connected
      connectedList = Set.toList connected

  -- FUTUREWORK: Consider handling the discrepancy between the views of the
  -- conversation-owning backend and the local backend
  unless (Set.null unconnected) $
    P.warn $
      Log.msg ("A remote user is trying to add unconnected local users to a remote conversation" :: Text)
        . Log.field "remote_user" (show qAdder)
        . Log.field "local_unconnected_users" (show unconnected)

  -- Update the local view of the remote conversation by adding only those local
  -- users that are connected to the adder
  E.createMembersInRemoteConversation remoteConvId connectedList
  pure connected

-- | Kick a user from a conversation and send notifications.
--
-- This function removes the given victim from the conversation by making them
-- leave, but then sends notifications as if the user was removed by someone
-- else.
kickMember ::
  ( Member BackendNotificationQueueAccess r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member ProposalStore r,
    Member (Input UTCTime) r,
    Member (Input Env) r,
    Member MemberStore r,
    Member SubConversationStore r,
    Member TinyLog r,
    Member Random r
  ) =>
  Qualified UserId ->
  Local Conversation ->
  BotsAndMembers ->
  Qualified UserId ->
  Sem r ()
kickMember qusr lconv targets victim = void . runError @NoChanges $ do
  (extraTargets, _) <-
    performAction
      SConversationLeaveTag
      victim
      lconv
      ()
  notifyConversationAction
    (sing @'ConversationRemoveMembersTag)
    qusr
    True
    Nothing
    lconv
    (targets <> extraTargets)
    (ConversationRemoveMembers (pure victim) EdReasonRemoved)

notifyTypingIndicator ::
  ( Member (Input UTCTime) r,
    Member (Input (Local ())) r,
    Member NotificationSubsystem r,
    Member FederatorAccess r
  ) =>
  Conversation ->
  Qualified UserId ->
  Maybe ConnId ->
  TypingStatus ->
  Sem r TypingDataUpdated
notifyTypingIndicator conv qusr mcon ts = do
  let origDomain = qDomain qusr
  now <- input
  lconv <- qualifyLocal (Data.convId conv)

  pushTypingIndicatorEvents qusr now (fmap lmId (Data.convLocalMembers conv)) mcon (tUntagged lconv) ts

  let (remoteMemsOrig, remoteMemsOther) = List.partition ((origDomain ==) . tDomain . rmId) (Data.convRemoteMembers conv)
      tdu users =
        TypingDataUpdated
          { time = now,
            origUserId = qusr,
            convId = Data.convId conv,
            usersInConv = users,
            typingStatus = ts
          }

  void $ E.runFederatedConcurrentlyEither (fmap rmId remoteMemsOther) $ \rmems -> do
    fedClient @'Galley @"on-typing-indicator-updated" (tdu (tUnqualified rmems))

  pure (tdu (fmap (tUnqualified . rmId) remoteMemsOrig))

pushTypingIndicatorEvents ::
  (Member NotificationSubsystem r) =>
  Qualified UserId ->
  UTCTime ->
  [UserId] ->
  Maybe ConnId ->
  Qualified ConvId ->
  TypingStatus ->
  Sem r ()
pushTypingIndicatorEvents qusr tEvent users mcon qcnv ts = do
  let e = Event qcnv Nothing qusr tEvent (EdTyping ts)
  pushNotifications
    [ def
        { origin = Just (qUnqualified qusr),
          json = toJSONObject e,
          recipients = map userRecipient users,
          conn = mcon,
          route = PushV2.RouteDirect,
          transient = True
        }
    ]
