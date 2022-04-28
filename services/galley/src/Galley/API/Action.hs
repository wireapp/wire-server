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
    updateLocalConversationWithLocalUser,
    updateLocalConversationWithLocalUserUnchecked,
    updateLocalConversationWithRemoteUser,
    NoChanges (..),

    -- * Utilities
    ensureConversationActionAllowed,
    addMembersToLocalConversation,
    notifyConversationAction,
    notifyRemoteConversationAction,
    ConversationUpdate,
  )
where

import qualified Brig.Types.User as User
import Control.Arrow
import Control.Lens
import Data.ByteString.Conversion (toByteString')
import Data.Id
import Data.Kind
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map as Map
import Data.Misc
import Data.Qualified
import qualified Data.Set as S
import qualified Data.Set as Set
import Data.Singletons
import Data.Time.Clock
import Galley.API.Error
import Galley.API.Util
import Galley.Data.Conversation
import qualified Galley.Data.Conversation as Data
import Galley.Data.Services
import Galley.Data.Types
import Galley.Effects
import qualified Galley.Effects.BotAccess as E
import qualified Galley.Effects.BrigAccess as E
import qualified Galley.Effects.CodeStore as E
import qualified Galley.Effects.ConversationStore as E
import qualified Galley.Effects.FederatorAccess as E
import qualified Galley.Effects.FireAndForget as E
import qualified Galley.Effects.MemberStore as E
import qualified Galley.Effects.TeamStore as E
import Galley.Options
import Galley.Types.Conversations.Members
import Galley.Types.UserList
import Galley.Validation
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import qualified Polysemy.TinyLog as P
import qualified System.Logger as Log
import Wire.API.Conversation hiding (Conversation, Member)
import Wire.API.Conversation.Action
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.Federation.API (Component (Galley), fedClient)
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import Wire.API.Team.LegalHold
import Wire.API.Team.Member

data NoChanges = NoChanges

type family HasConversationActionEffects (tag :: ConversationActionTag) r :: Constraint where
  HasConversationActionEffects 'ConversationJoinTag r =
    Members
      '[ BrigAccess,
         Error FederationError,
         ErrorS 'NotATeamMember,
         ErrorS 'NotConnected,
         ErrorS ('ActionDenied 'LeaveConversation),
         ErrorS ('ActionDenied 'AddConversationMember),
         ErrorS 'InvalidOperation,
         ErrorS 'ConvAccessDenied,
         ErrorS 'ConvNotFound,
         ErrorS 'TooManyMembers,
         ErrorS 'MissingLegalholdConsent,
         ExternalAccess,
         FederatorAccess,
         GundeckAccess,
         Input Opts,
         Input UTCTime,
         LegalHoldStore,
         MemberStore,
         TeamStore,
         ConversationStore,
         Error NoChanges
       ]
      r
  HasConversationActionEffects 'ConversationLeaveTag r =
    (Members '[MemberStore, Error NoChanges] r)
  HasConversationActionEffects 'ConversationRemoveMembersTag r =
    (Members '[MemberStore, Error NoChanges] r)
  HasConversationActionEffects 'ConversationMemberUpdateTag r =
    (Members '[MemberStore, ErrorS 'ConvMemberNotFound] r)
  HasConversationActionEffects 'ConversationDeleteTag r =
    Members '[Error FederationError, ErrorS 'NotATeamMember, CodeStore, TeamStore, ConversationStore] r
  HasConversationActionEffects 'ConversationRenameTag r =
    Members '[Error InvalidInput, ConversationStore] r
  HasConversationActionEffects 'ConversationAccessDataTag r =
    Members
      '[ BotAccess,
         BrigAccess,
         CodeStore,
         Error InvalidInput,
         Error NoChanges,
         ErrorS 'InvalidTargetAccess,
         ErrorS ('ActionDenied 'RemoveConversationMember),
         ExternalAccess,
         FederatorAccess,
         FireAndForget,
         GundeckAccess,
         MemberStore,
         TeamStore,
         Input UTCTime,
         ConversationStore
       ]
      r
  HasConversationActionEffects 'ConversationMessageTimerUpdateTag r =
    Members '[ConversationStore, Error NoChanges] r
  HasConversationActionEffects 'ConversationReceiptModeUpdateTag r =
    Members '[ConversationStore, Error NoChanges] r

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
       ErrorS 'ConvNotFound
     ]
  HasConversationActionGalleyErrors 'ConversationAccessDataTag =
    '[ ErrorS ('ActionDenied 'RemoveConversationMember),
       ErrorS ('ActionDenied 'ModifyConversationAccess),
       ErrorS 'InvalidOperation,
       ErrorS 'InvalidTargetAccess,
       ErrorS 'ConvNotFound
     ]

noChanges :: Member (Error NoChanges) r => Sem r a
noChanges = throw NoChanges

ensureAllowed ::
  forall tag mem r x.
  (IsConvMember mem, HasConversationActionEffects tag r) =>
  Sing tag ->
  Local x ->
  ConversationAction tag ->
  Conversation ->
  mem ->
  Sem r ()
ensureAllowed tag loc action conv origUser = do
  case tag of
    SConversationJoinTag ->
      mapErrorS @'InvalidAction @('ActionDenied 'AddConversationMember) $
        ensureConvRoleNotElevated origUser (cjRole action)
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
    _ -> pure ()

-- | Returns additional members that resulted from the action (e.g. ConversationJoin)
-- and also returns the (possible modified) action that was performed
performAction ::
  forall tag r.
  (HasConversationActionEffects tag r) =>
  Sing tag ->
  Qualified UserId ->
  Local ConvId ->
  Conversation ->
  ConversationAction tag ->
  Sem r (BotsAndMembers, ConversationAction tag)
performAction tag origUser lcnv cnv action =
  case tag of
    SConversationJoinTag -> do
      performConversationJoin origUser lcnv cnv action
    SConversationLeaveTag -> do
      let presentVictims = filter (isConvMember lcnv cnv) (toList action)
      when (null presentVictims) noChanges
      E.deleteMembers (convId cnv) (toUserList lcnv presentVictims)
      pure (mempty, action) -- FUTUREWORK: should we return the filtered action here?
    SConversationRemoveMembersTag -> do
      let presentVictims = filter (isConvMember lcnv cnv) (toList action)
      when (null presentVictims) noChanges
      E.deleteMembers (convId cnv) (toUserList lcnv presentVictims)
      pure (mempty, action) -- FUTUREWORK: should we return the filtered action here?
    SConversationMemberUpdateTag -> do
      void $ ensureOtherMember lcnv (cmuTarget action) cnv
      E.setOtherMember lcnv (cmuTarget action) (cmuUpdate action)
      pure (mempty, action)
    SConversationDeleteTag -> do
      key <- E.makeKey (tUnqualified lcnv)
      E.deleteCode key ReusableCode
      case convTeam cnv of
        Nothing -> E.deleteConversation (tUnqualified lcnv)
        Just tid -> E.deleteTeamConversation tid (tUnqualified lcnv)
      pure (mempty, action)
    SConversationRenameTag -> do
      cn <- rangeChecked (cupName action)
      E.setConversationName (tUnqualified lcnv) cn
      pure (mempty, action)
    SConversationMessageTimerUpdateTag -> do
      when (Data.convMessageTimer cnv == cupMessageTimer action) noChanges
      E.setConversationMessageTimer (tUnqualified lcnv) (cupMessageTimer action)
      pure (mempty, action)
    SConversationReceiptModeUpdateTag -> do
      when (Data.convReceiptMode cnv == Just (cruReceiptMode action)) noChanges
      E.setConversationReceiptMode (tUnqualified lcnv) (cruReceiptMode action)
      pure (mempty, action)
    SConversationAccessDataTag -> do
      (bm, act) <- performConversationAccessData origUser lcnv cnv action
      pure (bm, act)

performConversationJoin ::
  (HasConversationActionEffects 'ConversationJoinTag r) =>
  Qualified UserId ->
  Local ConvId ->
  Conversation ->
  ConversationJoin ->
  Sem r (BotsAndMembers, ConversationJoin)
performConversationJoin qusr lcnv conv (ConversationJoin invited role) = do
  let newMembers = ulNewMembers lcnv conv . toUserList lcnv $ invited

  lusr <- ensureLocal lcnv qusr
  ensureMemberLimit (toList (convLocalMembers conv)) newMembers
  ensureAccess conv InviteAccess
  checkLocals lusr (convTeam conv) (ulLocals newMembers)
  checkRemotes lusr (ulRemotes newMembers)
  checkLHPolicyConflictsLocal (ulLocals newMembers)
  checkLHPolicyConflictsRemote (FutureWork (ulRemotes newMembers))

  addMembersToLocalConversation lcnv newMembers role
  where
    checkLocals ::
      Members
        '[ BrigAccess,
           ErrorS 'NotATeamMember,
           ErrorS 'NotConnected,
           ErrorS 'ConvAccessDenied,
           TeamStore
         ]
        r =>
      Local UserId ->
      Maybe TeamId ->
      [UserId] ->
      Sem r ()
    checkLocals lusr (Just tid) newUsers = do
      tms <-
        Map.fromList . map (view userId &&& id)
          <$> E.selectTeamMembers tid newUsers
      let userMembershipMap = map (id &&& flip Map.lookup tms) newUsers
      ensureAccessRole (convAccessRoles conv) userMembershipMap
      ensureConnectedOrSameTeam lusr newUsers
    checkLocals lusr Nothing newUsers = do
      ensureAccessRole (convAccessRoles conv) (zip newUsers $ repeat Nothing)
      ensureConnectedOrSameTeam lusr newUsers

    checkRemotes ::
      Members
        '[ BrigAccess,
           Error FederationError,
           ErrorS 'NotConnected,
           FederatorAccess
         ]
        r =>
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
      Members
        '[ ConversationStore,
           ErrorS ('ActionDenied 'LeaveConversation),
           ErrorS 'InvalidOperation,
           ErrorS 'ConvNotFound,
           ErrorS 'MissingLegalholdConsent,
           ExternalAccess,
           FederatorAccess,
           GundeckAccess,
           Input Opts,
           Input UTCTime,
           LegalHoldStore,
           MemberStore,
           TeamStore
         ]
        r =>
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
                let lvictim = qualifyAs lcnv (lmId mem)
                void . runError @NoChanges $
                  updateLocalConversationWithLocalUser @'ConversationLeaveTag lcnv lvictim Nothing $
                    pure (qUntagged lvictim)
          else throwS @'MissingLegalholdConsent

    checkLHPolicyConflictsRemote ::
      FutureWork 'LegalholdPlusFederationNotImplemented [Remote UserId] ->
      Sem r ()
    checkLHPolicyConflictsRemote _remotes = pure ()

performConversationAccessData ::
  (HasConversationActionEffects 'ConversationAccessDataTag r) =>
  Qualified UserId ->
  Local ConvId ->
  Conversation ->
  ConversationAccessData ->
  Sem r (BotsAndMembers, ConversationAccessData)
performConversationAccessData qusr lcnv conv action = do
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
    void . for_ (nonEmpty (bmQualifiedMembers lcnv toRemove)) $ \usersToRemove -> do
      void . runError @NoChanges $ performAction SConversationLeaveTag qusr lcnv conv usersToRemove
      notifyConversationAction (sing @'ConversationLeaveTag) qusr Nothing lcnv bmToNotify usersToRemove
  pure (mempty, action)
  where
    maybeRemoveBots :: Member BrigAccess r => BotsAndMembers -> Sem r BotsAndMembers
    maybeRemoveBots bm =
      if Set.member ServiceAccessRole (cupAccessRoles action)
        then pure bm
        else pure $ bm {bmBots = mempty}

    maybeRemoveGuests :: Member BrigAccess r => BotsAndMembers -> Sem r BotsAndMembers
    maybeRemoveGuests bm =
      if Set.member GuestAccessRole (cupAccessRoles action)
        then pure bm
        else do
          activated <- map User.userId <$> E.lookupActivatedUsers (toList (bmLocals bm))
          -- FUTUREWORK: should we also remove non-activated remote users?
          pure $ bm {bmLocals = Set.fromList activated}

    maybeRemoveNonTeamMembers :: Member TeamStore r => BotsAndMembers -> Sem r BotsAndMembers
    maybeRemoveNonTeamMembers bm =
      if Set.member NonTeamMemberAccessRole (cupAccessRoles action)
        then pure bm
        else case convTeam conv of
          Just tid -> do
            onlyTeamUsers <- filterM (fmap isJust . E.getTeamMember tid) (toList (bmLocals bm))
            pure $ bm {bmLocals = Set.fromList onlyTeamUsers, bmRemotes = mempty}
          Nothing -> pure bm

    maybeRemoveTeamMembers :: Member TeamStore r => BotsAndMembers -> Sem r BotsAndMembers
    maybeRemoveTeamMembers bm =
      if Set.member TeamMemberAccessRole (cupAccessRoles action)
        then pure bm
        else case convTeam conv of
          Just tid -> do
            noTeamMembers <- filterM (fmap isNothing . E.getTeamMember tid) (toList (bmLocals bm))
            pure $ bm {bmLocals = Set.fromList noTeamMembers}
          Nothing -> pure bm

updateLocalConversationWithLocalUser ::
  forall tag r.
  ( Members
      '[ ConversationStore,
         Error NoChanges,
         ErrorS ('ActionDenied (ConversationActionPermission tag)),
         ErrorS 'InvalidOperation,
         ErrorS 'ConvNotFound,
         ExternalAccess,
         FederatorAccess,
         GundeckAccess,
         Input UTCTime
       ]
      r,
    HasConversationActionEffects tag r,
    SingI tag
  ) =>
  Local ConvId ->
  Local UserId ->
  Maybe ConnId ->
  ConversationAction tag ->
  Sem r Event
updateLocalConversationWithLocalUser lcnv lusr con action = do
  let tag = sing @tag

  -- retrieve conversation
  conv <- getConversationWithError lcnv

  -- check that the action does not bypass the underlying protocol
  unless (protocolValidAction (convProtocol conv) (fromSing tag)) $
    throwS @'InvalidOperation

  -- perform all authorisation checks and, if successful, the update itself
  updateLocalConversationWithLocalUserUnchecked @tag conv lusr con action

-- | Similar to 'updateLocalConversationWithLocalUser', but takes a
-- 'Conversation' value directly, instead of a 'ConvId', and skips protocol
-- checks. All the other checks are still performed.
--
-- This is intended to be used by protocol-aware code, once all the
-- protocol-specific checks and updates have been performed, to finally apply
-- the changes to the conversation as seen by the backend.
updateLocalConversationWithLocalUserUnchecked ::
  forall tag r.
  ( SingI tag,
    Member (ErrorS ('ActionDenied (ConversationActionPermission tag))) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member GundeckAccess r,
    Member (Input UTCTime) r,
    HasConversationActionEffects tag r
  ) =>
  Conversation ->
  Local UserId ->
  Maybe ConnId ->
  ConversationAction tag ->
  Sem r Event
updateLocalConversationWithLocalUserUnchecked conv lusr con action = do
  let tag = sing @tag
      lcnv = qualifyAs lusr (convId conv)

  -- retrieve member
  self <- noteS @'ConvNotFound $ getConvMember lusr conv lusr

  -- perform checks
  ensureConversationActionAllowed (sing @tag) lcnv action conv self

  -- perform action
  (extraTargets, action') <- performAction tag (qUntagged lusr) lcnv conv action

  notifyConversationAction
    (sing @tag)
    (qUntagged lusr)
    con
    lcnv
    (convBotsAndMembers conv <> extraTargets)
    action'

updateLocalConversationWithRemoteUser ::
  forall tag r.
  ( Members
      '[ ConversationStore,
         Error NoChanges,
         ErrorS ('ActionDenied (ConversationActionPermission tag)),
         ErrorS 'InvalidOperation,
         ErrorS 'ConvNotFound,
         ExternalAccess,
         FederatorAccess,
         GundeckAccess,
         Input UTCTime
       ]
      r,
    HasConversationActionEffects tag r
  ) =>
  Sing tag ->
  Local ConvId ->
  Remote UserId ->
  ConversationAction tag ->
  Sem r ConversationUpdate
updateLocalConversationWithRemoteUser tag lcnv rusr action = do
  -- retrieve conversation
  (conv, self) <- getConversationAndMemberWithError @'ConvNotFound (qUntagged rusr) lcnv

  -- perform checks
  ensureConversationActionAllowed tag lcnv action conv self

  -- perform action
  (extraTargets, action') <- performAction tag (qUntagged rusr) lcnv conv action

  -- filter out user from rusr's domain, because rusr's backend will update
  -- local state and notify its users itself using the ConversationUpdate
  -- returned by this function
  let targets = convBotsAndMembers conv <> extraTargets
      remotes = bmRemotes targets
      remotesUserDomain = S.filter ((== tDomain rusr) . tDomain) remotes
      remotesOtherDomain = remotes Set.\\ remotesUserDomain

  void $
    notifyConversationAction
      tag
      (qUntagged rusr)
      Nothing
      lcnv
      (targets {bmRemotes = remotesOtherDomain})
      action'

  now <- input

  pure $
    ConversationUpdate
      { cuTime = now,
        cuOrigUserId = qUntagged rusr,
        cuConvId = tUnqualified lcnv,
        cuAlreadyPresentUsers = tUnqualified <$> S.toList remotesUserDomain,
        cuAction = SomeConversationAction tag action'
      }

-- --------------------------------------------------------------------------------
-- -- Utilities

ensureConversationActionAllowed ::
  forall tag mem x r.
  ( IsConvMember mem,
    HasConversationActionEffects tag r,
    Members
      '[ ErrorS ('ActionDenied (ConversationActionPermission tag)),
         ErrorS 'InvalidOperation
       ]
      r
  ) =>
  Sing tag ->
  Local x ->
  ConversationAction (tag :: ConversationActionTag) ->
  Conversation ->
  mem ->
  Sem r ()
ensureConversationActionAllowed tag loc action conv self = do
  -- general action check
  ensureActionAllowed (sConversationActionPermission tag) self

  -- check if it is a group conversation (except for rename actions)
  when (fromSing tag /= ConversationRenameTag) $
    ensureGroupConversation conv

  -- extra action-specific checks
  ensureAllowed tag loc action conv self

-- | Add users to a conversation without performing any checks. Return extra
-- notification targets and the action performed.
addMembersToLocalConversation ::
  Members '[MemberStore, Error NoChanges] r =>
  Local ConvId ->
  UserList UserId ->
  RoleName ->
  Sem r (BotsAndMembers, ConversationJoin)
addMembersToLocalConversation lcnv users role = do
  (lmems, rmems) <- E.createMembers (tUnqualified lcnv) (fmap (,role) users)
  neUsers <- note NoChanges $ nonEmpty (ulAll lcnv users)
  let action = ConversationJoin neUsers role
  pure (bmFromMembers lmems rmems, action)

notifyConversationAction ::
  forall tag r.
  Members '[FederatorAccess, ExternalAccess, GundeckAccess, Input UTCTime] r =>
  Sing tag ->
  Qualified UserId ->
  Maybe ConnId ->
  Local ConvId ->
  BotsAndMembers ->
  ConversationAction (tag :: ConversationActionTag) ->
  Sem r Event
notifyConversationAction tag quid con lcnv targets action = do
  now <- input
  let e = conversationActionToEvent tag now quid (qUntagged lcnv) action

  E.runFederatedConcurrently_ (toList (bmRemotes targets)) $ \ruids ->
    fedClient @'Galley @"on-conversation-updated" $
      ConversationUpdate now quid (tUnqualified lcnv) (tUnqualified ruids) (SomeConversationAction tag action)

  -- notify local participants and bots
  pushConversationEvent con e (qualifyAs lcnv (bmLocals targets)) (bmBots targets) $> e

-- | Notify all local members about a remote conversation update that originated
-- from a local user
notifyRemoteConversationAction ::
  Members
    '[ FederatorAccess,
       ExternalAccess,
       GundeckAccess,
       MemberStore,
       Input (Local ()),
       P.TinyLog
     ]
    r =>
  Remote ConversationUpdate ->
  ConnId ->
  Sem r Event
notifyRemoteConversationAction rconvUpdate con = do
  let convUpdate = tUnqualified rconvUpdate
      rconvId = qualifyAs rconvUpdate . cuConvId $ convUpdate

  let event =
        case cuAction convUpdate of
          SomeConversationAction tag action ->
            conversationActionToEvent tag (cuTime convUpdate) (cuOrigUserId convUpdate) (qUntagged rconvId) action

  -- Note: we generally do not send notifications to users that are not part of
  -- the conversation (from our point of view), to prevent spam from the remote
  -- backend.
  (presentUsers, allUsersArePresent) <-
    E.selectRemoteMembers (cuAlreadyPresentUsers convUpdate) rconvId
  loc <- qualifyLocal ()
  let localPresentUsers = qualifyAs loc presentUsers

  unless allUsersArePresent $
    P.warn $
      Log.field "conversation" (toByteString' . tUnqualified $ rconvId)
        . Log.field "domain" (toByteString' (tDomain rconvUpdate))
        . Log.msg
          ( "Attempt to send notification about conversation update \
            \to users not in the conversation" ::
              ByteString
          )

  -- FUTUREWORK: Check if presentUsers contain bots when federated bots are
  -- implemented.
  let bots = []

  pushConversationEvent (Just con) event localPresentUsers bots $> event
