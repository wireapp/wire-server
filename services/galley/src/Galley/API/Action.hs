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
    HasConversationActionGalleyErrors,

    -- * Performing actions
    updateLocalConversationUncheckedJoin,
    updateLocalConversationUncheckedRemoveMembers,
    updateLocalConversationJoin,
    updateLocalConversationLeave,
    updateLocalConversationMemberUpdate,
    updateLocalConversationDelete,
    updateLocalConversationRename,
    updateLocalConversationMessageTimerUpdate,
    updateLocalConversationReceiptModeUpdate,
    updateLocalConversationAccessData,
    updateLocalConversationRemoveMembers,
    updateLocalConversationUpdateProtocol,
    updateLocalConversationUpdateAddPermission,
    updateLocalConversationReset,
    updateLocalConversationHistoryUpdate,
    NoChanges (..),
    LocalConversationUpdate (..),
    notifyTypingIndicator,
    pushTypingIndicatorEvents,

    -- * Utilities
    addMembersToLocalConversation,
    sendConversationActionNotifications,
    updateLocalStateOfRemoteConv,
    addLocalUsersToRemoteConv,
    ConversationUpdate,
    ensureAllowed,
  )
where

import Control.Arrow ((&&&))
import Control.Lens
import Data.ByteString.Conversion (toByteString')
import Data.Default
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
import Data.Range (Range, Within, checkedEither)
import Data.Set ((\\))
import Data.Set qualified as Set
import Data.Singletons
import Data.Time.Clock
import GHC.TypeLits (KnownNat)
import Galley.API.Action.Kick
import Galley.API.Action.Leave
import Galley.API.Action.Notify
import Galley.API.Action.Reset
import Galley.API.MLS.Conversation
import Galley.API.MLS.Migration
import Galley.API.MLS.Removal
import Galley.API.Teams.Features.Get
import Galley.Effects
import Galley.Types.Error
import Imports hiding ((\\))
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Resource
import Polysemy.TinyLog
import Polysemy.TinyLog qualified as P
import System.Logger qualified as Log
import Wire.API.Connection (Relation (Accepted))
import Wire.API.Conversation hiding (Conversation, Member)
import Wire.API.Conversation qualified as AddPermission
import Wire.API.Conversation.Action
import Wire.API.Conversation.Config (ConversationSubsystemConfig (..))
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.API.Galley qualified as F
import Wire.API.Federation.Client (FederatorClient)
import Wire.API.Federation.Error
import Wire.API.FederationStatus
import Wire.API.History
import Wire.API.MLS.Group.Serialisation qualified as Serialisation
import Wire.API.MLS.SubConversation
import Wire.API.Push.V2 qualified as PushV2
import Wire.API.Routes.Internal.Brig.Connection
import Wire.API.Routes.Public.Galley.MLS
import Wire.API.Team.LegalHold
import Wire.API.Team.Member
import Wire.API.Team.Permission (Perm (AddRemoveConvMember, ModifyConvName))
import Wire.API.User as User
import Wire.BrigAPIAccess qualified as E
import Wire.CodeStore
import Wire.CodeStore qualified as E
import Wire.ConversationStore qualified as E
import Wire.ConversationSubsystem
import Wire.ConversationSubsystem.Util
import Wire.FeaturesConfigSubsystem
import Wire.FederationAPIAccess qualified as E
import Wire.FederationSubsystem
import Wire.FireAndForget qualified as E
import Wire.NotificationSubsystem
import Wire.ProposalStore qualified as E
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now
import Wire.StoredConversation
import Wire.StoredConversation qualified as Data
import Wire.TeamCollaboratorsSubsystem
import Wire.TeamSubsystem (TeamSubsystem)
import Wire.TeamSubsystem qualified as TeamSubsystem
import Wire.UserList

class IsConversationAction (tag :: ConversationActionTag) where
  type HasConversationActionEffects tag (r :: EffectRow) :: Constraint

  type HasConversationActionGalleyErrors tag :: EffectRow

  performAction ::
    forall r.
    ( HasConversationActionEffects tag r,
      SingI tag
    ) =>
    Local StoredConversation ->
    Qualified UserId ->
    Maybe ConnId ->
    ConversationAction tag ->
    Sem r (PerformActionResult tag)

  ensureAllowed ::
    forall mem r x.
    ( IsConvMember mem,
      HasConversationActionEffects tag r,
      Member (ErrorS ConvNotFound) r,
      Member (Error FederationError) r,
      Member TeamSubsystem r
    ) =>
    Local x ->
    ConversationAction tag ->
    StoredConversation ->
    ActorContext mem ->
    Sem r ()

instance IsConversationAction 'ConversationJoinTag where
  type
    HasConversationActionEffects 'ConversationJoinTag r =
      ( -- TODO: Replace with subsystems
        Member BackendNotificationQueueAccess r,
        Member ConversationSubsystem r,
        Member TeamCollaboratorsSubsystem r,
        Member FederationSubsystem r,
        Member TeamSubsystem r,
        Member (Input ConversationSubsystemConfig) r,
        Member BrigAPIAccess r,
        Member (Error FederationError) r,
        Member (ErrorS 'NotATeamMember) r,
        Member (ErrorS 'NotConnected) r,
        Member (ErrorS ('ActionDenied 'AddConversationMember)) r,
        Member (ErrorS 'InvalidOperation) r,
        Member (ErrorS 'ConvAccessDenied) r,
        Member (ErrorS 'TooManyMembers) r,
        Member (ErrorS 'MissingLegalholdConsent) r,
        Member (ErrorS 'GroupIdVersionNotSupported) r,
        Member (Error UnreachableBackends) r,
        Member ExternalAccess r,
        Member (FederationAPIAccess FederatorClient) r,
        Member NotificationSubsystem r,
        Member Now r,
        Member LegalHoldStore r,
        Member ProposalStore r,
        Member Random r,
        Member TeamStore r,
        Member TinyLog r,
        Member ConversationStore r,
        Member (Error NoChanges) r
      )

  type
    HasConversationActionGalleyErrors 'ConversationJoinTag =
      '[ ErrorS ('ActionDenied 'AddConversationMember),
         ErrorS 'GroupIdVersionNotSupported,
         ErrorS 'NotATeamMember,
         ErrorS 'InvalidOperation,
         ErrorS 'ConvNotFound,
         ErrorS 'NotConnected,
         ErrorS 'ConvAccessDenied,
         ErrorS 'TooManyMembers,
         ErrorS 'MissingLegalholdConsent
       ]

  performAction lconv qusr _conId action = do
    (extraTargets, action') <- performConversationJoin qusr lconv action
    pure
      PerformActionResult
        { extraTargets = extraTargets,
          action = action',
          extraConversationData = def
        }

  ensureAllowed _ action conv (ActorContext Nothing (Just tm)) =
    case action of
      ConversationJoin _ _ InternalAdd -> throwS @'ConvNotFound
      ConversationJoin _ _ ExternalAdd -> ensureManageChannelsPermission conv tm
  ensureAllowed loc action conv (ActorContext (Just origUser) _mTm) =
    mapErrorS @'InvalidAction @('ActionDenied 'AddConversationMember) $ do
      ensureConvRoleNotElevated origUser (role action)
      checkGroupIdSupport loc conv action
  ensureAllowed _ _ _ (ActorContext Nothing Nothing) = throwS @'ConvNotFound

instance IsConversationAction 'ConversationLeaveTag where
  type
    HasConversationActionEffects 'ConversationLeaveTag r =
      ( Member (Input ConversationSubsystemConfig) r,
        Member BackendNotificationQueueAccess r,
        Member ExternalAccess r,
        Member ConversationStore r,
        Member NotificationSubsystem r,
        Member ProposalStore r,
        Member Random r,
        Member Now r,
        Member (Error FederationError) r,
        Member TinyLog r
      )

  type
    HasConversationActionGalleyErrors 'ConversationLeaveTag =
      '[ ErrorS ('ActionDenied 'LeaveConversation),
         ErrorS 'InvalidOperation,
         ErrorS 'ConvNotFound
       ]

  performAction lconv qusr _conId () = do
    leaveConversation qusr lconv
    pure $ mkPerformActionResult ()

  ensureAllowed _ _action _conv (ActorContext Nothing (Just _tm)) =
    throwS @'ConvNotFound
  ensureAllowed _loc _action _conv (ActorContext (Just _origUser) _mTm) =
    pure ()
  ensureAllowed _ _ _ (ActorContext Nothing Nothing) = throwS @'ConvNotFound

instance IsConversationAction 'ConversationRemoveMembersTag where
  type
    HasConversationActionEffects 'ConversationRemoveMembersTag r =
      ( Member (Error NoChanges) r,
        Member ConversationStore r,
        Member (Input ConversationSubsystemConfig) r,
        Member (Error FederationError) r,
        Member TinyLog r,
        Member BackendNotificationQueueAccess r,
        Member ExternalAccess r,
        Member ProposalStore r,
        Member Now r,
        Member Random r,
        Member NotificationSubsystem r
      )

  type
    HasConversationActionGalleyErrors 'ConversationRemoveMembersTag =
      '[ ErrorS ('ActionDenied 'RemoveConversationMember),
         ErrorS 'InvalidOperation,
         ErrorS 'ConvNotFound
       ]

  performAction lconv _qusr _conId action = do
    let presentVictims = filter (isConvMemberL lconv) (toList . crmTargets $ action)
    when (null presentVictims) noChanges
    traverse_ (convDeleteMembers (toUserList lconv presentVictims)) lconv
    -- send remove proposals in the MLS case
    traverse_ (removeUser lconv RemoveUserExcludeMain) presentVictims
    pure $ mkPerformActionResult action -- FUTUREWORK: should we return the filtered action here?

  ensureAllowed _ _action conv (ActorContext Nothing (Just tm)) =
    ensureManageChannelsPermission conv tm
  ensureAllowed _loc _action _conv (ActorContext (Just _origUser) _mTm) =
    pure ()
  ensureAllowed _ _ _ (ActorContext Nothing Nothing) = throwS @'ConvNotFound

instance IsConversationAction 'ConversationMemberUpdateTag where
  type
    HasConversationActionEffects 'ConversationMemberUpdateTag r =
      ( Member (ErrorS 'ConvMemberNotFound) r,
        Member ConversationStore r
      )

  type
    HasConversationActionGalleyErrors 'ConversationMemberUpdateTag =
      '[ ErrorS ('ActionDenied 'ModifyOtherConversationMember),
         ErrorS 'InvalidOperation,
         ErrorS 'ConvNotFound,
         ErrorS 'ConvMemberNotFound
       ]

  performAction lconv _qusr _conId action = do
    let lcnv = fmap (.id_) lconv
        storedConv = tUnqualified lconv
    void $ ensureOtherMember lconv (cmuTarget action) storedConv
    E.setOtherMember lcnv (cmuTarget action) (cmuUpdate action)
    pure $ mkPerformActionResult action

  ensureAllowed _ _action _conv (ActorContext Nothing (Just _tm)) =
    throwS @'ConvNotFound
  ensureAllowed _loc _action _conv (ActorContext (Just _origUser) _mTm) =
    pure ()
  ensureAllowed _ _ _ (ActorContext Nothing Nothing) = throwS @'ConvNotFound

instance IsConversationAction 'ConversationDeleteTag where
  type
    HasConversationActionEffects 'ConversationDeleteTag r =
      ( Member (ErrorS 'NotATeamMember) r,
        Member ConversationStore r,
        Member ProposalStore r,
        Member CodeStore r
      )

  type
    HasConversationActionGalleyErrors 'ConversationDeleteTag =
      '[ ErrorS ('ActionDenied 'DeleteConversation),
         ErrorS 'NotATeamMember,
         ErrorS 'InvalidOperation,
         ErrorS 'ConvNotFound
       ]

  performAction lconv _qusr _conId () = do
    let lcnv = fmap (.id_) lconv
        storedConv = tUnqualified lconv
    let deleteGroup groupId = do
          E.removeAllMLSClients groupId
          E.deleteAllProposals groupId

    let cid = storedConv.id_
    for_ (storedConv & mlsMetadata <&> cnvmlsGroupId . fst) $ \gidParent -> do
      sconvs <- E.listSubConversations cid
      for_ (Map.assocs sconvs) $ \(subid, mlsData) -> do
        let gidSub = cnvmlsGroupId mlsData
        E.deleteSubConversation cid subid
        deleteGroup gidSub
      deleteGroup gidParent

    key <- E.makeKey (tUnqualified lcnv)
    E.deleteCode key
    case convTeam storedConv of
      Nothing -> E.deleteConversation (tUnqualified lcnv)
      Just tid -> E.deleteTeamConversation tid (tUnqualified lcnv)

    pure $ mkPerformActionResult ()

  ensureAllowed _ _action _conv (ActorContext Nothing (Just _tm)) =
    throwS @'ConvNotFound
  ensureAllowed loc _action conv (ActorContext (Just origUser) _mTm) =
    for_ (convTeam conv) $ \tid -> do
      lusr <- ensureLocal loc (convMemberId loc origUser)
      void $ TeamSubsystem.internalGetTeamMember (tUnqualified lusr) tid >>= noteS @'NotATeamMember
  ensureAllowed _ _ _ (ActorContext Nothing Nothing) = throwS @'ConvNotFound

instance IsConversationAction 'ConversationRenameTag where
  type
    HasConversationActionEffects 'ConversationRenameTag r =
      ( Member TeamSubsystem r,
        Member (ErrorS InvalidOperation) r,
        Member (Error InvalidInput) r,
        Member ConversationStore r
      )

  type
    HasConversationActionGalleyErrors 'ConversationRenameTag =
      '[ ErrorS ('ActionDenied 'ModifyConversationName),
         ErrorS 'InvalidOperation,
         ErrorS 'ConvNotFound
       ]

  performAction lconv qusr _conId action = do
    let lcnv = fmap (.id_) lconv
        storedConv = tUnqualified lconv
    zusrMembership <- join <$> forM storedConv.metadata.cnvmTeam (TeamSubsystem.internalGetTeamMember (qUnqualified qusr))
    for_ zusrMembership $ \tm -> unless (tm `hasPermission` ModifyConvName) $ throwS @'InvalidOperation
    cn <- rangeChecked (cupName action)
    E.setConversationName (tUnqualified lcnv) cn
    pure $ mkPerformActionResult action

  ensureAllowed _ _action conv (ActorContext Nothing (Just tm)) =
    ensureManageChannelsPermission conv tm
  ensureAllowed _loc _action _conv (ActorContext (Just _origUser) _mTm) =
    pure ()
  ensureAllowed _ _ _ (ActorContext Nothing Nothing) = throwS @'ConvNotFound

instance IsConversationAction 'ConversationAccessDataTag where
  type
    HasConversationActionEffects 'ConversationAccessDataTag r =
      ( Member BrigAPIAccess r,
        Member CodeStore r,
        Member (Error NoChanges) r,
        Member (ErrorS 'InvalidTargetAccess) r,
        Member (ErrorS ('ActionDenied 'RemoveConversationMember)) r,
        Member ExternalAccess r,
        Member FireAndForget r,
        Member NotificationSubsystem r,
        Member (Input ConversationSubsystemConfig) r,
        Member ProposalStore r,
        Member TinyLog r,
        Member Now r,
        Member ConversationStore r,
        Member Random r,
        Member (Error FederationError) r,
        Member BackendNotificationQueueAccess r,
        Member ConversationSubsystem r,
        Member TeamSubsystem r
      )

  type
    HasConversationActionGalleyErrors 'ConversationAccessDataTag =
      '[ ErrorS ('ActionDenied 'RemoveConversationMember),
         ErrorS ('ActionDenied 'ModifyConversationAccess),
         ErrorS 'InvalidOperation,
         ErrorS 'InvalidTargetAccess,
         ErrorS 'ConvNotFound
       ]

  performAction lconv qusr _conId action = do
    (bm, act) <- performConversationAccessData qusr lconv action
    pure
      PerformActionResult
        { extraTargets = bm,
          action = act,
          extraConversationData = def
        }

  ensureAllowed _ _action _conv (ActorContext Nothing (Just _tm)) =
    throwS @'ConvNotFound
  ensureAllowed _loc action conv (ActorContext (Just origUser) mTm) = do
    -- 'PrivateAccessRole' is for self-conversations, 1:1 conversations and
    -- so on; users not supposed to be able to make other conversations
    -- have 'PrivateAccessRole'
    when (PrivateAccess `elem` cupAccess action || Set.null (cupAccessRoles action)) $
      throwS @'InvalidTargetAccess
    -- Team conversations incur another round of checks
    case convTeam conv of
      Just _ -> do
        -- Access mode change might result in members being removed from the
        -- conversation, so the user must have the necessary permission flag,
        -- unless the actor is a team member with ManageChannels on a channel.
        unless (maybe False (hasManageChannelsPermission conv) mTm) $ ensureActionAllowed SRemoveConversationMember origUser
      Nothing ->
        -- not a team conv, so one of the other access roles has to allow this.
        when (Set.null $ cupAccessRoles action Set.\\ Set.fromList [TeamMemberAccessRole]) $
          throwS @'InvalidTargetAccess
  ensureAllowed _ _ _ (ActorContext Nothing Nothing) = throwS @'ConvNotFound

instance IsConversationAction 'ConversationHistoryUpdateTag where
  type
    HasConversationActionEffects 'ConversationHistoryUpdateTag r =
      ( Member ConversationStore r,
        Member (ErrorS HistoryNotSupported) r
      )

  type
    HasConversationActionGalleyErrors 'ConversationHistoryUpdateTag =
      '[ ErrorS (ActionDenied ModifyConversationAccess),
         ErrorS HistoryNotSupported,
         ErrorS InvalidOperation,
         ErrorS ConvNotFound
       ]

  performAction lconv _qusr _conId action = do
    let lcnv = fmap (.id_) lconv
        storedConv = tUnqualified lconv
    when (storedConv.metadata.cnvmGroupConvType /= Just Channel) $ do
      throwS @HistoryNotSupported
    E.setConversationHistory (tUnqualified lcnv) action
    pure $ mkPerformActionResult action

  ensureAllowed _ _action _conv (ActorContext Nothing (Just _tm)) =
    throwS @'ConvNotFound
  ensureAllowed _loc _action _conv (ActorContext (Just _origUser) _mTm) =
    pure ()
  ensureAllowed _ _ _ (ActorContext Nothing Nothing) = throwS @'ConvNotFound

instance IsConversationAction 'ConversationMessageTimerUpdateTag where
  type
    HasConversationActionEffects 'ConversationMessageTimerUpdateTag r =
      ( Member ConversationStore r,
        Member (Error NoChanges) r
      )

  type
    HasConversationActionGalleyErrors 'ConversationMessageTimerUpdateTag =
      '[ ErrorS ('ActionDenied 'ModifyConversationMessageTimer),
         ErrorS 'InvalidOperation,
         ErrorS 'ConvNotFound
       ]

  performAction lconv _qusr _conId action = do
    let lcnv = fmap (.id_) lconv
        storedConv = tUnqualified lconv
    when (Data.convMessageTimer storedConv == cupMessageTimer action) noChanges
    E.setConversationMessageTimer (tUnqualified lcnv) (cupMessageTimer action)
    pure $ mkPerformActionResult action

  ensureAllowed _ _action _conv (ActorContext Nothing (Just _)) =
    throwS @'ConvNotFound
  ensureAllowed _loc _action _conv (ActorContext (Just _) _) =
    pure ()
  ensureAllowed _ _ _ (ActorContext Nothing Nothing) = throwS @'ConvNotFound

instance IsConversationAction 'ConversationReceiptModeUpdateTag where
  type
    HasConversationActionEffects 'ConversationReceiptModeUpdateTag r =
      ( Member (ErrorS MLSReadReceiptsNotAllowed) r,
        Member ConversationStore r,
        Member (Error NoChanges) r
      )

  type
    HasConversationActionGalleyErrors 'ConversationReceiptModeUpdateTag =
      '[ ErrorS ('ActionDenied 'ModifyConversationReceiptMode),
         ErrorS 'InvalidOperation,
         ErrorS 'MLSReadReceiptsNotAllowed,
         ErrorS 'ConvNotFound
       ]

  performAction lconv _qusr _conId action = do
    let lcnv = fmap (.id_) lconv
        storedConv = tUnqualified lconv
    when (Data.convReceiptMode storedConv == Just (cruReceiptMode action)) noChanges
    E.setConversationReceiptMode (tUnqualified lcnv) (cruReceiptMode action)
    pure $ mkPerformActionResult action

  ensureAllowed _ _action _conv (ActorContext Nothing (Just _tm)) =
    throwS @'ConvNotFound
  ensureAllowed _loc _action conv (ActorContext (Just _origUser) _mTm) = do
    -- cannot update receipt mode of MLS conversations
    when (convProtocolTag conv == ProtocolMLSTag) $
      throwS @MLSReadReceiptsNotAllowed
  ensureAllowed _ _ _ (ActorContext Nothing Nothing) = throwS @'ConvNotFound

instance IsConversationAction 'ConversationUpdateProtocolTag where
  type
    HasConversationActionEffects 'ConversationUpdateProtocolTag r =
      ( Member FeaturesConfigSubsystem r,
        Member BackendNotificationQueueAccess r,
        Member NotificationSubsystem r,
        Member BrigAPIAccess r,
        Member ExternalAccess r,
        Member TinyLog r,
        Member (Error NoChanges) r,
        Member ConversationStore r,
        Member Now r,
        Member Random r,
        Member (Input ConversationSubsystemConfig) r,
        Member ProposalStore r,
        Member (ErrorS ConvInvalidProtocolTransition) r,
        Member (FederationAPIAccess FederatorClient) r,
        Member (ErrorS MLSMigrationCriteriaNotSatisfied) r,
        Member (Error FederationError) r
      )

  type
    HasConversationActionGalleyErrors 'ConversationUpdateProtocolTag =
      '[ ErrorS ('ActionDenied 'LeaveConversation),
         ErrorS 'InvalidOperation,
         ErrorS 'ConvNotFound,
         ErrorS 'ConvInvalidProtocolTransition,
         ErrorS 'MLSMigrationCriteriaNotSatisfied
       ]

  performAction lconv qusr _conId action = do
    let lcnv = fmap (.id_) lconv
        storedConv = tUnqualified lconv
    case (protocolTag (tUnqualified lconv).protocol, action, convTeam (tUnqualified lconv)) of
      (ProtocolProteusTag, ProtocolMixedTag, Just _) -> do
        let gid = Serialisation.newGroupId (convType (tUnqualified lconv)) $ Conv <$> tUntagged lcnv
            epoch = Epoch 0
        E.updateToMixedProtocol (tUnqualified lcnv) gid epoch
        pure $ mkPerformActionResult action
      (ProtocolMixedTag, ProtocolMLSTag, Just tid) -> do
        mig <- getFeatureForTeam tid
        now <- Now.get
        mlsConv <- mkMLSConversation storedConv >>= noteS @'ConvInvalidProtocolTransition
        ok <- checkMigrationCriteria now mlsConv mig
        unless ok $ throwS @'MLSMigrationCriteriaNotSatisfied
        removeExtraneousClients qusr lconv
        E.updateToMLSProtocol (tUnqualified lcnv)
        pure $ mkPerformActionResult action
      (ProtocolProteusTag, ProtocolProteusTag, _) ->
        noChanges
      (ProtocolMixedTag, ProtocolMixedTag, _) ->
        noChanges
      (ProtocolMLSTag, ProtocolMLSTag, _) ->
        noChanges
      (_, _, _) -> throwS @'ConvInvalidProtocolTransition

  ensureAllowed _ _action _conv (ActorContext Nothing (Just _tm)) =
    throwS @'ConvNotFound
  ensureAllowed _loc _action _conv (ActorContext (Just _origUser) _mTm) =
    pure ()
  ensureAllowed _ _ _ (ActorContext Nothing Nothing) = throwS @'ConvNotFound

instance IsConversationAction 'ConversationUpdateAddPermissionTag where
  type
    HasConversationActionEffects 'ConversationUpdateAddPermissionTag r =
      ( Member (ErrorS 'InvalidTargetAccess) r,
        Member (Error NoChanges) r,
        Member ConversationStore r
      )

  type
    HasConversationActionGalleyErrors 'ConversationUpdateAddPermissionTag =
      '[ ErrorS ('ActionDenied 'ModifyAddPermission),
         ErrorS 'InvalidOperation,
         ErrorS 'ConvNotFound,
         ErrorS 'InvalidTargetAccess
       ]

  performAction lconv _qusr _conId action = do
    let lcnv = fmap (.id_) lconv
        storedConv = tUnqualified lconv
    when (storedConv.metadata.cnvmChannelAddPermission == Just (addPermission action)) noChanges
    E.updateChannelAddPermissions (tUnqualified lcnv) (addPermission action)
    pure $ mkPerformActionResult action

  ensureAllowed _ _action _conv (ActorContext Nothing (Just _tm)) =
    throwS @'ConvNotFound
  ensureAllowed _loc _action conv (ActorContext (Just _origUser) _mTm) = do
    unless (conv.metadata.cnvmGroupConvType == Just Channel) $ throwS @'InvalidTargetAccess
  ensureAllowed _ _ _ (ActorContext Nothing Nothing) = throwS @'ConvNotFound

instance IsConversationAction 'ConversationResetTag where
  type
    HasConversationActionEffects 'ConversationResetTag r =
      ( Member BackendNotificationQueueAccess r,
        Member (FederationAPIAccess FederatorClient) r,
        Member ExternalAccess r,
        Member ConversationSubsystem r,
        Member ConversationStore r,
        Member NotificationSubsystem r,
        Member ProposalStore r,
        Member E.MLSCommitLockStore r,
        Member Resource r,
        Member (Input ConversationSubsystemConfig) r,
        Member (ErrorS MLSStaleMessage) r,
        Member (ErrorS ConvNotFound) r,
        Member (ErrorS InvalidOperation) r,
        Member Random r,
        Member Now r,
        Member TinyLog r
      )

  type
    HasConversationActionGalleyErrors 'ConversationResetTag =
      '[ ErrorS (ActionDenied LeaveConversation),
         ErrorS MLSStaleMessage,
         ErrorS InvalidOperation,
         ErrorS ConvNotFound
       ]

  performAction lconv qusr _conId action = do
    newGroupId <- resetLocalMLSMainConversation qusr lconv action
    pure
      PerformActionResult
        { extraTargets = mempty,
          action = action,
          extraConversationData = ExtraConversationData (Just newGroupId)
        }

  ensureAllowed _ _action _conv (ActorContext Nothing (Just _tm)) =
    throwS @'ConvNotFound
  ensureAllowed _loc _action _conv (ActorContext (Just _origUser) _mTm) =
    pure ()
  ensureAllowed _ _ _ (ActorContext Nothing Nothing) = throwS @'ConvNotFound

noChanges :: (Member (Error NoChanges) r) => Sem r a
noChanges = throw NoChanges

data PerformActionResult tag
  = PerformActionResult
  { extraTargets :: BotsAndMembers,
    action :: ConversationAction tag,
    extraConversationData :: ExtraConversationData
  }

mkPerformActionResult :: ConversationAction tag -> PerformActionResult tag
mkPerformActionResult action =
  PerformActionResult
    { extraTargets = mempty,
      action = action,
      extraConversationData = def
    }

performConversationJoin ::
  forall r.
  (HasConversationActionEffects 'ConversationJoinTag r) =>
  Qualified UserId ->
  Local StoredConversation ->
  ConversationJoin ->
  Sem r (BotsAndMembers, ConversationJoin)
performConversationJoin qusr lconv (ConversationJoin invited role joinType) = do
  let newMembers = ulNewMembers lconv conv . toUserList lconv $ invited

  lusr <- ensureLocal lconv qusr
  ensureMemberLimit (convProtocolTag conv) (toList conv.localMembers) newMembers
  ensureAccess conv InviteAccess
  checkLocals lusr (convTeam conv) (ulLocals newMembers)
  enforceFederationProtocol (protocolTag conv.protocol) (fmap void (ulRemotes newMembers))
  checkRemotes lusr (ulRemotes newMembers)
  checkLHPolicyConflictsLocal (ulLocals newMembers)
  checkLHPolicyConflictsRemote (FutureWork (ulRemotes newMembers))
  checkRemoteBackendsConnected lusr
  checkTeamMemberAddPermission lusr
  setOutOfSyncFlag lconv newMembers
  addMembersToLocalConversation (fmap (.id_) lconv) newMembers role joinType
  where
    checkRemoteBackendsConnected :: Local x -> Sem r ()
    checkRemoteBackendsConnected loc = do
      let invitedRemoteUsers = snd . partitionQualified loc . NE.toList $ invited
          invitedRemoteDomains = Set.fromList $ void <$> invitedRemoteUsers
          existingRemoteDomains = Set.fromList $ void . (.id_) <$> (tUnqualified lconv).remoteMembers
          allInvitedAlreadyInConversation = null $ invitedRemoteDomains \\ existingRemoteDomains

      if not allInvitedAlreadyInConversation
        then checkFederationStatus (RemoteDomains (invitedRemoteDomains <> existingRemoteDomains))
        else -- even if there are no new remotes, we still need to check they are reachable
          void . (ensureNoUnreachableBackends =<<) $
            E.runFederatedConcurrentlyEither @_ @_ @'Brig invitedRemoteUsers $ \_ ->
              pure ()

    conv :: StoredConversation
    conv = tUnqualified lconv

    checkLocals ::
      Local UserId ->
      Maybe TeamId ->
      [UserId] ->
      Sem r ()
    checkLocals lusr (Just tid) newUsers = do
      tms <-
        Map.fromList . map (view Wire.API.Team.Member.userId &&& Imports.id)
          <$> TeamSubsystem.internalSelectTeamMembers tid newUsers
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

    -- \| Guard conversation member additions against legal-hold consent conflicts:
    -- - if any conv member has LH enabled then all new users must give consent
    -- - if any new user has LH enabled then all new users must give consent
    -- - if new users have LH enabled then
    --   - ensure that a consented conv admin exists
    --   - and kick all existing members that do not consent to LH from the conversation
    -- See also: "Brig.API.Connection.checkLegalholdPolicyConflict"
    -- and "Galley.API.LegalHold.Conflicts.guardLegalholdPolicyConflictsUid".
    checkLHPolicyConflictsLocal ::
      [UserId] ->
      Sem r ()
    checkLHPolicyConflictsLocal newUsers = do
      let convUsers = conv.localMembers

      allNewUsersGaveConsent <- allLegalholdConsentGiven newUsers

      whenM (anyLegalholdActivated ((.id_) <$> convUsers)) $
        unless allNewUsersGaveConsent $
          throwS @'MissingLegalholdConsent

      whenM (anyLegalholdActivated newUsers) $ do
        unless allNewUsersGaveConsent $
          throwS @'MissingLegalholdConsent

        convUsersLHStatus <- do
          uidsStatus <- getLHStatusForUsers ((.id_) <$> convUsers)
          pure $ zipWith (\mem (_, status) -> (mem, status)) convUsers uidsStatus

        if any
          ( \(mem, status) ->
              mem.convRoleName == roleNameWireAdmin
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
                  (tUntagged (qualifyAs lconv mem.id_))
          else throwS @'MissingLegalholdConsent

    checkLHPolicyConflictsRemote ::
      FutureWork 'LegalholdPlusFederationNotImplemented [Remote UserId] ->
      Sem r ()
    checkLHPolicyConflictsRemote _remotes = pure ()

    checkTeamMemberAddPermission :: Local UserId -> Sem r ()
    checkTeamMemberAddPermission lusr = do
      case conv.metadata.cnvmTeam of
        Just tid -> do
          maybeTeamMember <- TeamSubsystem.internalGetTeamMember (tUnqualified lusr) tid
          case maybeTeamMember of
            Just tm -> do
              let isChannel = conv.metadata.cnvmGroupConvType == Just Channel
                  isConversationAdmin =
                    maybe False (\m -> m.convRoleName == roleNameWireAdmin) $
                      find (\m -> m.id_ == lusr.tUntagged.qUnqualified) conv.localMembers
                  isAddPermissionEveryone = conv.metadata.cnvmChannelAddPermission == Just AddPermission.Everyone

              if isChannel
                then do
                  -- at this point we know the conversation is a channel, the user is a team member, and when:
                  -- - the user is a conversation admin (including external partners) => they can add members
                  --   note: external partners can be allowed to create channels, in which case they will always be the channel's admin
                  -- - or the add-permission is set to everyone (including exteral partners) => they can add members
                  -- - or the user is a team admin => they can add members
                  unless (isConversationAdmin || isAddPermissionEveryone || tm `hasPermission` ManageChannels) $ throwS @'InvalidOperation
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
  (HasConversationActionEffects 'ConversationAccessDataTag r) =>
  Qualified UserId ->
  Local StoredConversation ->
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
      E.deleteCode key

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
    lcnv = fmap (.id_) lconv
    conv = tUnqualified lconv

    maybeRemoveBots :: BotsAndMembers -> Sem r BotsAndMembers
    maybeRemoveBots bm =
      if Set.member ServiceAccessRole (cupAccessRoles action)
        then pure bm
        else pure $ bm {bmBots = mempty}

    maybeRemoveGuests :: (Member BrigAPIAccess r) => BotsAndMembers -> Sem r BotsAndMembers
    maybeRemoveGuests bm =
      if Set.member GuestAccessRole (cupAccessRoles action)
        then pure bm
        else do
          activated <- map User.userId <$> E.lookupActivatedUsers (toList (bmLocals bm))
          -- FUTUREWORK: should we also remove non-activated remote users?
          pure $ bm {bmLocals = Set.fromList activated}

    maybeRemoveNonTeamMembers :: (Member TeamSubsystem r) => BotsAndMembers -> Sem r BotsAndMembers
    maybeRemoveNonTeamMembers bm =
      if Set.member NonTeamMemberAccessRole (cupAccessRoles action)
        then pure bm
        else case convTeam conv of
          Just tid -> do
            onlyTeamUsers <- filterM (fmap isJust . flip TeamSubsystem.internalGetTeamMember tid) (toList (bmLocals bm))
            pure $ bm {bmLocals = Set.fromList onlyTeamUsers, bmRemotes = mempty}
          Nothing -> pure bm

    maybeRemoveTeamMembers :: (Member TeamSubsystem r) => BotsAndMembers -> Sem r BotsAndMembers
    maybeRemoveTeamMembers bm =
      if Set.member TeamMemberAccessRole (cupAccessRoles action)
        then pure bm
        else case convTeam conv of
          Just tid -> do
            noTeamMembers <- filterM (fmap isNothing . flip TeamSubsystem.internalGetTeamMember tid) (toList (bmLocals bm))
            pure $ bm {bmLocals = Set.fromList noTeamMembers}
          Nothing -> pure bm

updateLocalConversationJoin ::
  ( Member BackendNotificationQueueAccess r,
    Member (Error FederationError) r,
    Member (ErrorS ('ActionDenied (ConversationActionPermission 'ConversationJoinTag))) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'ConvNotFound) r,
    Member ConversationSubsystem r,
    Member FederationSubsystem r,
    Member TeamCollaboratorsSubsystem r,
    Member TeamSubsystem r,
    Member (Input ConversationSubsystemConfig) r,
    Member BrigAPIAccess r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'NotConnected) r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'TooManyMembers) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member (ErrorS 'GroupIdVersionNotSupported) r,
    Member (Error UnreachableBackends) r,
    Member ExternalAccess r,
    Member (FederationAPIAccess FederatorClient) r,
    Member NotificationSubsystem r,
    Member Now r,
    Member LegalHoldStore r,
    Member ProposalStore r,
    Member Random r,
    Member TeamStore r,
    Member TinyLog r,
    Member ConversationStore r,
    Member (Error NoChanges) r
  ) =>
  Local ConvId ->
  Qualified UserId ->
  Maybe ConnId ->
  ConversationJoin ->
  Sem r LocalConversationUpdate
updateLocalConversationJoin =
  updateLocalConversation @'ConversationJoinTag

updateLocalConversationLeave ::
  ( Member BackendNotificationQueueAccess r,
    Member (Error FederationError) r,
    Member (ErrorS ('ActionDenied (ConversationActionPermission 'ConversationLeaveTag))) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'ConvNotFound) r,
    Member ConversationSubsystem r,
    Member TeamSubsystem r,
    Member (Input ConversationSubsystemConfig) r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member Now r,
    Member ProposalStore r,
    Member ConversationStore r,
    Member Random r,
    Member TinyLog r
  ) =>
  Local ConvId ->
  Qualified UserId ->
  Maybe ConnId ->
  Sem r LocalConversationUpdate
updateLocalConversationLeave lcnvId qusr connId =
  updateLocalConversation @'ConversationLeaveTag lcnvId qusr connId ()

updateLocalConversationMemberUpdate ::
  ( Member (Error FederationError) r,
    Member (ErrorS ('ActionDenied (ConversationActionPermission 'ConversationMemberUpdateTag))) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'ConvNotFound) r,
    Member ConversationSubsystem r,
    Member TeamSubsystem r,
    Member (ErrorS ConvMemberNotFound) r,
    Member ConversationStore r
  ) =>
  Local ConvId ->
  Qualified UserId ->
  Maybe ConnId ->
  ConversationMemberUpdate ->
  Sem r LocalConversationUpdate
updateLocalConversationMemberUpdate =
  updateLocalConversation @'ConversationMemberUpdateTag

updateLocalConversationDelete ::
  ( Member (ErrorS ('ActionDenied (ConversationActionPermission 'ConversationDeleteTag))) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'ConvNotFound) r,
    Member ConversationSubsystem r,
    Member TeamSubsystem r,
    Member CodeStore r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (ErrorS 'NotATeamMember) r,
    Member ProposalStore r
  ) =>
  Local ConvId ->
  Qualified UserId ->
  Maybe ConnId ->
  Sem r LocalConversationUpdate
updateLocalConversationDelete lcnvId uid connId =
  updateLocalConversation @'ConversationDeleteTag lcnvId uid connId ()

updateLocalConversationRename ::
  ( Member (Error FederationError) r,
    Member (ErrorS ('ActionDenied (ConversationActionPermission 'ConversationRenameTag))) r,
    Member (ErrorS 'ConvNotFound) r,
    Member ConversationSubsystem r,
    Member TeamSubsystem r,
    Member (Error InvalidInput) r,
    Member ConversationStore r,
    Member (ErrorS InvalidOperation) r
  ) =>
  Local ConvId ->
  Qualified UserId ->
  Maybe ConnId ->
  ConversationRename ->
  Sem r LocalConversationUpdate
updateLocalConversationRename =
  updateLocalConversation @'ConversationRenameTag

updateLocalConversationMessageTimerUpdate ::
  ( Member (Error FederationError) r,
    Member (ErrorS ('ActionDenied (ConversationActionPermission 'ConversationMessageTimerUpdateTag))) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'ConvNotFound) r,
    Member ConversationSubsystem r,
    Member TeamSubsystem r,
    Member ConversationStore r,
    Member (Error NoChanges) r
  ) =>
  Local ConvId ->
  Qualified UserId ->
  Maybe ConnId ->
  ConversationMessageTimerUpdate ->
  Sem r LocalConversationUpdate
updateLocalConversationMessageTimerUpdate =
  updateLocalConversation @'ConversationMessageTimerUpdateTag

updateLocalConversationReceiptModeUpdate ::
  ( Member (Error FederationError) r,
    Member (ErrorS ('ActionDenied (ConversationActionPermission 'ConversationReceiptModeUpdateTag))) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'ConvNotFound) r,
    Member ConversationSubsystem r,
    Member TeamSubsystem r,
    Member ConversationStore r,
    Member (Error NoChanges) r,
    Member (ErrorS MLSReadReceiptsNotAllowed) r
  ) =>
  Local ConvId ->
  Qualified UserId ->
  Maybe ConnId ->
  ConversationReceiptModeUpdate ->
  Sem r LocalConversationUpdate
updateLocalConversationReceiptModeUpdate =
  updateLocalConversation @'ConversationReceiptModeUpdateTag

updateLocalConversationAccessData ::
  ( Member BackendNotificationQueueAccess r,
    Member (Error FederationError) r,
    Member (ErrorS ('ActionDenied (ConversationActionPermission 'ConversationAccessDataTag))) r,
    Member (ErrorS 'ConvNotFound) r,
    Member ConversationSubsystem r,
    Member (Error NoChanges) r,
    Member TinyLog r,
    Member ConversationStore r,
    Member BrigAPIAccess r,
    Member CodeStore r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member ProposalStore r,
    Member Now r,
    Member Random r,
    Member FireAndForget r,
    Member TeamSubsystem r,
    Member (Input ConversationSubsystemConfig) r,
    Member (ErrorS ('ActionDenied 'RemoveConversationMember)) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'InvalidTargetAccess) r
  ) =>
  Local ConvId ->
  Qualified UserId ->
  Maybe ConnId ->
  ConversationAccessData ->
  Sem r LocalConversationUpdate
updateLocalConversationAccessData =
  updateLocalConversation @'ConversationAccessDataTag

updateLocalConversationRemoveMembers ::
  ( Member BackendNotificationQueueAccess r,
    Member (Error FederationError) r,
    Member (ErrorS ('ActionDenied (ConversationActionPermission 'ConversationRemoveMembersTag))) r,
    Member ConversationSubsystem r,
    Member (Error NoChanges) r,
    Member TinyLog r,
    Member ConversationStore r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member ProposalStore r,
    Member Now r,
    Member Random r,
    Member TeamSubsystem r,
    Member (Input ConversationSubsystemConfig) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'ConvNotFound) r
  ) =>
  Local ConvId ->
  Qualified UserId ->
  Maybe ConnId ->
  ConversationRemoveMembers ->
  Sem r LocalConversationUpdate
updateLocalConversationRemoveMembers =
  updateLocalConversation @'ConversationRemoveMembersTag

updateLocalConversationUpdateProtocol ::
  ( Member BackendNotificationQueueAccess r,
    Member (Error FederationError) r,
    Member (ErrorS ('ActionDenied (ConversationActionPermission 'ConversationUpdateProtocolTag))) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'ConvNotFound) r,
    Member ConversationSubsystem r,
    Member (Error NoChanges) r,
    Member (FederationAPIAccess FederatorClient) r,
    Member TinyLog r,
    Member ConversationStore r,
    Member BrigAPIAccess r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member ProposalStore r,
    Member Now r,
    Member Random r,
    Member TeamSubsystem r,
    Member FeaturesConfigSubsystem r,
    Member (Input ConversationSubsystemConfig) r,
    Member (ErrorS 'ConvInvalidProtocolTransition) r,
    Member (ErrorS 'MLSMigrationCriteriaNotSatisfied) r
  ) =>
  Local ConvId ->
  Qualified UserId ->
  Maybe ConnId ->
  ProtocolTag ->
  Sem r LocalConversationUpdate
updateLocalConversationUpdateProtocol =
  updateLocalConversation @'ConversationUpdateProtocolTag

updateLocalConversationUpdateAddPermission ::
  ( Member (Error FederationError) r,
    Member (ErrorS ('ActionDenied (ConversationActionPermission 'ConversationUpdateAddPermissionTag))) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'ConvNotFound) r,
    Member ConversationSubsystem r,
    Member (Error NoChanges) r,
    Member ConversationStore r,
    Member TeamSubsystem r,
    Member (ErrorS 'InvalidTargetAccess) r
  ) =>
  Local ConvId ->
  Qualified UserId ->
  Maybe ConnId ->
  AddPermissionUpdate ->
  Sem r LocalConversationUpdate
updateLocalConversationUpdateAddPermission =
  updateLocalConversation @'ConversationUpdateAddPermissionTag

updateLocalConversationReset ::
  ( Member BackendNotificationQueueAccess r,
    Member (Error FederationError) r,
    Member (ErrorS ('ActionDenied (ConversationActionPermission 'ConversationResetTag))) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'ConvNotFound) r,
    Member ConversationSubsystem r,
    Member (FederationAPIAccess FederatorClient) r,
    Member TinyLog r,
    Member ConversationStore r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member ProposalStore r,
    Member Now r,
    Member Random r,
    Member Resource r,
    Member E.MLSCommitLockStore r,
    Member TeamSubsystem r,
    Member (Input ConversationSubsystemConfig) r,
    Member (ErrorS MLSStaleMessage) r
  ) =>
  Local ConvId ->
  Qualified UserId ->
  Maybe ConnId ->
  MLSReset ->
  Sem r LocalConversationUpdate
updateLocalConversationReset =
  updateLocalConversation @'ConversationResetTag

updateLocalConversationHistoryUpdate ::
  ( Member (Error FederationError) r,
    Member (ErrorS ('ActionDenied (ConversationActionPermission 'ConversationHistoryUpdateTag))) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'ConvNotFound) r,
    Member ConversationSubsystem r,
    Member ConversationStore r,
    Member TeamSubsystem r,
    Member (ErrorS HistoryNotSupported) r
  ) =>
  Local ConvId ->
  Qualified UserId ->
  Maybe ConnId ->
  History ->
  Sem r LocalConversationUpdate
updateLocalConversationHistoryUpdate =
  updateLocalConversation @'ConversationHistoryUpdateTag

updateLocalConversationUncheckedJoin ::
  ( Member BackendNotificationQueueAccess r,
    Member (Error FederationError) r,
    Member (ErrorS ('ActionDenied (ConversationActionPermission 'ConversationJoinTag))) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'ConvNotFound) r,
    Member ConversationSubsystem r,
    Member FederationSubsystem r,
    Member TeamCollaboratorsSubsystem r,
    Member TeamSubsystem r,
    Member (Input ConversationSubsystemConfig) r,
    Member BrigAPIAccess r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'NotConnected) r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'TooManyMembers) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member (ErrorS 'GroupIdVersionNotSupported) r,
    Member (Error UnreachableBackends) r,
    Member ExternalAccess r,
    Member (FederationAPIAccess FederatorClient) r,
    Member NotificationSubsystem r,
    Member Now r,
    Member LegalHoldStore r,
    Member ProposalStore r,
    Member Random r,
    Member TeamStore r,
    Member TinyLog r,
    Member ConversationStore r,
    Member (Error NoChanges) r
  ) =>
  Local StoredConversation ->
  Qualified UserId ->
  Maybe ConnId ->
  ConversationJoin ->
  Sem r LocalConversationUpdate
updateLocalConversationUncheckedJoin =
  updateLocalConversationUnchecked @'ConversationJoinTag

updateLocalConversationUncheckedRemoveMembers ::
  ( Member BackendNotificationQueueAccess r,
    Member (Error FederationError) r,
    Member (ErrorS ('ActionDenied (ConversationActionPermission 'ConversationRemoveMembersTag))) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'ConvNotFound) r,
    Member ConversationSubsystem r,
    Member TeamSubsystem r,
    Member (Input ConversationSubsystemConfig) r,
    Member (Error NoChanges) r,
    Member ConversationStore r,
    Member ProposalStore r,
    Member Now r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member Random r,
    Member TinyLog r
  ) =>
  Local StoredConversation ->
  Qualified UserId ->
  Maybe ConnId ->
  ConversationRemoveMembers ->
  Sem r LocalConversationUpdate
updateLocalConversationUncheckedRemoveMembers =
  updateLocalConversationUnchecked @'ConversationRemoveMembersTag

updateLocalConversation ::
  forall tag r.
  ( Member ConversationStore r,
    Member (Error FederationError) r,
    Member (ErrorS ('ActionDenied (ConversationActionPermission tag))) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'ConvNotFound) r,
    Member ConversationSubsystem r,
    HasConversationActionEffects tag r,
    IsConversationAction tag,
    SingI tag,
    Member TeamSubsystem r
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
  unless (protocolValidAction conv.protocol tag action) $
    throwS @'InvalidOperation
  -- perform all authorisation checks and, if successful, then update itself
  updateLocalConversationUnchecked @tag (qualifyAs lcnv conv) qusr con action

-- | Similar to 'updateLocalConversationWithLocalUser', but takes a
-- 'StoredConversation' value directly, instead of a 'ConvId', and skips protocol
-- checks. All the other checks are still performed.
--
-- This is intended to be used by protocol-aware code, once all the
-- protocol-specific checks and updates have been performed, to finally apply
-- the changes to the conversation as seen by the backend.
updateLocalConversationUnchecked ::
  forall tag r.
  ( SingI tag,
    IsConversationAction tag,
    Member (Error FederationError) r,
    Member (ErrorS ('ActionDenied (ConversationActionPermission tag))) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member ConversationSubsystem r,
    HasConversationActionEffects tag r,
    Member TeamSubsystem r
  ) =>
  Local StoredConversation ->
  Qualified UserId ->
  Maybe ConnId ->
  ConversationAction tag ->
  Sem r LocalConversationUpdate
updateLocalConversationUnchecked lconv qusr con action = do
  let lcnv = fmap (.id_) lconv
      conv = tUnqualified lconv
  mTeamMember <- foldQualified lconv (getTeamMembership conv) (const $ pure Nothing) qusr
  ensureConversationActionAllowed (sing @tag) lcnv conv mTeamMember
  par <- performAction @tag lconv qusr con action
  sendConversationActionNotifications
    (sing @tag)
    qusr
    False
    con
    lconv
    (convBotsAndMembers (tUnqualified lconv) <> par.extraTargets)
    par.action
    par.extraConversationData
  where
    getTeamMembership :: StoredConversation -> Local UserId -> Sem r (Maybe TeamMember)
    getTeamMembership conv luid = maybe (pure Nothing) (TeamSubsystem.internalGetTeamMember (tUnqualified luid)) conv.metadata.cnvmTeam

    ensureConversationActionAllowed :: Sing tag -> Local x -> StoredConversation -> Maybe TeamMember -> Sem r ()
    ensureConversationActionAllowed tag loc conv mTeamMember = do
      let hasChannelManagePerm = maybe False (hasManageChannelsPermission conv) mTeamMember
          mMem = getConvMember lconv conv qusr :: Maybe (Either LocalMember RemoteMember)
      -- If the actor is a conversation member, enforce conversation-role
      -- permission unless we intentionally skip it (channel overrides or
      -- special join case).
      unless
        (skipConversationRoleCheck tag conv mTeamMember || (hasChannelManagePerm && channelAdminOverride tag))
        (for_ mMem (ensureActionAllowed (sConversationActionPermission tag)))

      checkConversationType (fromSing tag) conv

      -- extra action-specific checks
      ensureAllowed @tag loc action conv (ActorContext mMem mTeamMember)

    skipConversationRoleCheck :: Sing tag -> StoredConversation -> Maybe TeamMember -> Bool
    skipConversationRoleCheck SConversationJoinTag conv (Just _) = conv.metadata.cnvmChannelAddPermission == Just AddPermission.Everyone
    skipConversationRoleCheck _ _ _ = False

    -- channelAdminOverride is necessary to let team admins act as "channel admins" even if their conversation_role isn't wire_admin,
    -- but only for the intended actions. It’s placed here so we bypass only the generic role check and still enforce
    -- all channel- and protocol-specific rules afterwards.
    channelAdminOverride :: Sing tag -> Bool
    channelAdminOverride = \case
      SConversationJoinTag -> True
      SConversationRemoveMembersTag -> True
      SConversationMemberUpdateTag -> True
      SConversationRenameTag -> True
      SConversationMessageTimerUpdateTag -> True
      SConversationAccessDataTag -> True
      SConversationUpdateAddPermissionTag -> True
      SConversationDeleteTag -> True
      _ -> False

-- --------------------------------------------------------------------------------
-- -- Utilities

-- | Add users to a conversation without performing any checks. Return extra
-- notification targets and the action performed.
addMembersToLocalConversation ::
  ( Member (Error NoChanges) r,
    Member ConversationStore r
  ) =>
  Local ConvId ->
  UserList UserId ->
  RoleName ->
  JoinType ->
  Sem r (BotsAndMembers, ConversationJoin)
addMembersToLocalConversation lcnv users role joinType = do
  (lmems, rmems) <- E.upsertMembers (tUnqualified lcnv) (fmap (,role) users)
  neUsers <- note NoChanges $ nonEmpty (ulAll lcnv users)
  let action = ConversationJoin neUsers role joinType
  pure (bmFromMembers lmems rmems, action)

setOutOfSyncFlag :: (Member ConversationStore r) => Local StoredConversation -> UserList UserId -> Sem r ()
setOutOfSyncFlag (tUnqualified -> conv) newMembers =
  let goingOutOfSync
        | ulNull newMembers = False
        | otherwise = case conv.protocol of
            ProtocolMLS _ -> True
            ProtocolProteus -> False
            -- no need to keep track of out of sync flag for mixed conversations
            ProtocolMixed _ -> False
   in when goingOutOfSync $
        E.setConversationOutOfSync conv.id_ True

-- | Update the local database with information on conversation members joining
-- or leaving. Finally, push out notifications to local users.
updateLocalStateOfRemoteConv ::
  ( Member BrigAPIAccess r,
    Member NotificationSubsystem r,
    Member ExternalAccess r,
    Member (Input (Local ())) r,
    Member ConversationStore r,
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
  (presentUsers, _) <-
    E.selectRemoteMembers cu.alreadyPresentUsers rconvId

  -- Perform action, and determine extra notification targets.
  --
  -- When new users are being added to the conversation, we consider them as
  -- notification targets. Since we check connections before letting
  -- people being added, this is safe against spam. However, if users that
  -- are not in the conversations are being removed or have their membership state
  -- updated, we do **not** add them to the list of targets, because we have no
  -- way to make sure that they are actually supposed to receive that notification.

  let sca = cu.action
  (mActualAction, extraTargets) <- case cu.action of
    SomeConversationAction SConversationJoinTag action -> do
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
    SomeConversationAction SConversationLeaveTag _ -> do
      let users = foldQualified loc (pure . tUnqualified) (const []) cu.origUserId
      E.deleteMembersInRemoteConversation rconvId users
      pure (Just sca, [])
    SomeConversationAction SConversationRemoveMembersTag action -> do
      let localUsers = getLocalUsers (tDomain loc) . crmTargets $ action
      E.deleteMembersInRemoteConversation rconvId localUsers
      pure (Just sca, [])
    SomeConversationAction SConversationMemberUpdateTag _ ->
      pure (Just sca, [])
    SomeConversationAction SConversationDeleteTag _ -> do
      E.deleteMembersInRemoteConversation rconvId presentUsers
      pure (Just sca, [])
    SomeConversationAction SConversationRenameTag _ -> pure (Just sca, [])
    SomeConversationAction SConversationMessageTimerUpdateTag _ -> pure (Just sca, [])
    SomeConversationAction SConversationReceiptModeUpdateTag _ -> pure (Just sca, [])
    SomeConversationAction SConversationAccessDataTag _ -> pure (Just sca, [])
    SomeConversationAction SConversationUpdateProtocolTag _ -> pure (Just sca, [])
    SomeConversationAction SConversationUpdateAddPermissionTag _ -> pure (Just sca, [])
    SomeConversationAction SConversationResetTag _ -> pure (Just sca, [])
    SomeConversationAction SConversationHistoryUpdateTag _ -> pure (Just sca, [])

  -- On conversation join, the member(s) joining are not included in the presentUsers,
  -- however they are included in the alreadyPresentUsers from the incoming request.
  -- To have a meaningful check here, we need to include the extra targets (the newly added users)
  -- when matching the present users against the alreadyPresentUsers.
  let targets = nubOrd $ presentUsers <> extraTargets
      allUsersExceptExtraTargetsArePresent = Set.fromList targets == Set.fromList cu.alreadyPresentUsers
  unless allUsersExceptExtraTargetsArePresent $
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
    let event = conversationActionToEvent tag cu.time (EventFromUser cu.origUserId) qconvId (fromMaybe def cu.extraConversationData) Nothing Nothing action
    -- FUTUREWORK: support bots?
    pushConversationEvent con () event (qualifyAs loc targets) [] $> event

addLocalUsersToRemoteConv ::
  ( Member BrigAPIAccess r,
    Member ConversationStore r,
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
  E.upsertMembersInRemoteConversation remoteConvId connectedList
  pure connected

notifyTypingIndicator ::
  ( Member Now r,
    Member (Input (Local ())) r,
    Member NotificationSubsystem r,
    Member (FederationAPIAccess FederatorClient) r
  ) =>
  StoredConversation ->
  Qualified UserId ->
  Maybe ConnId ->
  TypingStatus ->
  Sem r TypingDataUpdated
notifyTypingIndicator conv qusr mcon ts = do
  now <- Now.get
  lconv <- qualifyLocal conv.id_
  let origDomain = qDomain qusr
      (remoteMemsOrig, remoteMemsOther) = List.partition (\m -> origDomain == tDomain m.id_) conv.remoteMembers
      localMembers = fmap (.id_) (tryRemoveSelfFromLocalUsers lconv conv.localMembers)
      remoteMembersFromOriginDomain = fmap (tUnqualified . (.id_)) (tryRemoveSelfFromRemoteUsers lconv remoteMemsOrig)
      remoteMembersFromOtherDomains = fmap (.id_) remoteMemsOther
      tdu users =
        TypingDataUpdated
          { time = now,
            origUserId = qusr,
            convId = conv.id_,
            usersInConv = users,
            typingStatus = ts
          }

  pushTypingIndicatorEvents qusr now localMembers mcon (tUntagged lconv) ts

  void $ E.runFederatedConcurrentlyEither remoteMembersFromOtherDomains $ \rmems -> do
    fedClient @'Galley @"on-typing-indicator-updated" (tdu (tUnqualified rmems))

  pure (tdu remoteMembersFromOriginDomain)
  where
    tryRemoveSelfFromLocalUsers :: Local x -> [LocalMember] -> [LocalMember]
    tryRemoveSelfFromLocalUsers l ms = foldQualified l (\usr -> filter (\m -> m.id_ /= tUnqualified usr) ms) (const ms) qusr

    tryRemoveSelfFromRemoteUsers :: Local x -> [RemoteMember] -> [RemoteMember]
    tryRemoveSelfFromRemoteUsers l rms = foldQualified l (const rms) (\rusr -> filter (\rm -> rm.id_ /= rusr) rms) qusr

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
  let e = Event qcnv Nothing (EventFromUser qusr) tEvent Nothing (EdTyping ts)
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

rangeChecked :: (KnownNat n, KnownNat m, Member (Error InvalidInput) r, Within a n m) => a -> Sem r (Range n m a)
rangeChecked = either (throw . InvalidRange . fromString) pure . checkedEither
{-# INLINE rangeChecked #-}
