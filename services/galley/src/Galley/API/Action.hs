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
    ensureConversationActionAllowed,
    addMembersToLocalConversation,
    notifyConversationAction,
    notifyRemoteConversationAction,
    ConversationUpdate,
  )
where

import Control.Arrow ((&&&))
import Control.Lens
import Data.ByteString.Conversion (toByteString')
import Data.Id
import Data.Kind
import qualified Data.List as List
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map as Map
import Data.Misc
import Data.Qualified
import qualified Data.Set as Set
import Data.Singletons
import Data.Time.Clock
import Galley.API.Error
import Galley.API.MLS.Removal
import Galley.API.Util
import Galley.App
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
import Galley.Effects.GundeckAccess
import qualified Galley.Effects.MemberStore as E
import Galley.Effects.ProposalStore
import qualified Galley.Effects.TeamStore as E
import Galley.Intra.Push
import Galley.Options
import Galley.Types.Conversations.Members
import Galley.Types.UserList
import Galley.Validation
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog
import qualified Polysemy.TinyLog as P
import qualified System.Logger as Log
import Wire.API.Conversation hiding (Conversation, Member)
import Wire.API.Conversation.Action
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.Federation.API (Component (Galley), fedClient)
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import Wire.API.Team.LegalHold
import Wire.API.Team.Member
import qualified Wire.API.User as User

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
      Member ExternalAccess r,
      Member FederatorAccess r,
      Member GundeckAccess r,
      Member (Input Env) r,
      Member (Input Opts) r,
      Member (Input UTCTime) r,
      Member LegalHoldStore r,
      Member MemberStore r,
      Member ProposalStore r,
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
      Member GundeckAccess r,
      Member (Input UTCTime) r,
      Member (Input Env) r,
      Member ProposalStore r,
      Member TinyLog r
    )
  HasConversationActionEffects 'ConversationRemoveMembersTag r =
    ( Member MemberStore r,
      Member (Error NoChanges) r
    )
  HasConversationActionEffects 'ConversationMemberUpdateTag r =
    ( Member MemberStore r,
      Member (ErrorS 'ConvMemberNotFound) r
    )
  HasConversationActionEffects 'ConversationDeleteTag r =
    ( Member (Error FederationError) r,
      Member (ErrorS 'NotATeamMember) r,
      Member CodeStore r,
      Member TeamStore r,
      Member ConversationStore r
    )
  HasConversationActionEffects 'ConversationRenameTag r =
    ( Member (Error InvalidInput) r,
      Member ConversationStore r
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
      Member GundeckAccess r,
      Member (Input Env) r,
      Member MemberStore r,
      Member ProposalStore r,
      Member TeamStore r,
      Member TinyLog r,
      Member (Input UTCTime) r,
      Member ConversationStore r
    )
  HasConversationActionEffects 'ConversationMessageTimerUpdateTag r =
    ( Member ConversationStore r,
      Member (Error NoChanges) r
    )
  HasConversationActionEffects 'ConversationReceiptModeUpdateTag r =
    ( Member ConversationStore r,
      Member (Error NoChanges) r
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
  ( HasConversationActionEffects tag r
  ) =>
  Sing tag ->
  Qualified UserId ->
  Local Conversation ->
  ConversationAction tag ->
  Sem r (BotsAndMembers, ConversationAction tag)
performAction tag origUser lconv action = do
  let lcnv = fmap convId lconv
      conv = tUnqualified lconv
  case tag of
    SConversationJoinTag -> do
      performConversationJoin origUser lconv action
    SConversationLeaveTag -> do
      let victims = [origUser]
      E.deleteMembers (tUnqualified lcnv) (toUserList lconv victims)
      -- update in-memory view of the conversation
      let lconv' =
            lconv <&> \c ->
              foldQualified
                lconv
                ( \lu ->
                    c
                      { convLocalMembers =
                          filter (\lm -> lmId lm /= tUnqualified lu) (convLocalMembers c)
                      }
                )
                ( \ru ->
                    c
                      { convRemoteMembers =
                          filter (\rm -> rmId rm /= ru) (convRemoteMembers c)
                      }
                )
                origUser
      traverse_ (removeUser lconv') victims
      pure (mempty, action)
    SConversationRemoveMembersTag -> do
      let presentVictims = filter (isConvMemberL lconv) (toList action)
      when (null presentVictims) noChanges
      E.deleteMembers (tUnqualified lcnv) (toUserList lconv presentVictims)
      pure (mempty, action) -- FUTUREWORK: should we return the filtered action here?
    SConversationMemberUpdateTag -> do
      void $ ensureOtherMember lconv (cmuTarget action) conv
      E.setOtherMember lcnv (cmuTarget action) (cmuUpdate action)
      pure (mempty, action)
    SConversationDeleteTag -> do
      key <- E.makeKey (tUnqualified lcnv)
      E.deleteCode key ReusableCode
      case convTeam conv of
        Nothing -> E.deleteConversation (tUnqualified lcnv)
        Just tid -> E.deleteTeamConversation tid (tUnqualified lcnv)
      pure (mempty, action)
    SConversationRenameTag -> do
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

performConversationJoin ::
  ( HasConversationActionEffects 'ConversationJoinTag r
  ) =>
  Qualified UserId ->
  Local Conversation ->
  ConversationJoin ->
  Sem r (BotsAndMembers, ConversationJoin)
performConversationJoin qusr lconv (ConversationJoin invited role) = do
  let newMembers = ulNewMembers lconv conv . toUserList lconv $ invited

  lusr <- ensureLocal lconv qusr
  ensureMemberLimit (toList (convLocalMembers conv)) newMembers
  ensureAccess conv InviteAccess
  checkLocals lusr (convTeam conv) (ulLocals newMembers)
  checkRemotes lusr (ulRemotes newMembers)
  checkLHPolicyConflictsLocal (ulLocals newMembers)
  checkLHPolicyConflictsRemote (FutureWork (ulRemotes newMembers))

  addMembersToLocalConversation (fmap convId lconv) newMembers role
  where
    conv :: Data.Conversation
    conv = tUnqualified lconv

    checkLocals ::
      ( Member BrigAccess r,
        Member (ErrorS 'NotATeamMember) r,
        Member (ErrorS 'NotConnected) r,
        Member (ErrorS 'ConvAccessDenied) r,
        Member TeamStore r
      ) =>
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
      ( Member BrigAccess r,
        Member (Error FederationError) r,
        Member (ErrorS 'NotConnected) r,
        Member FederatorAccess r
      ) =>
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
      ( Member (Error InternalError) r,
        Member (ErrorS 'MissingLegalholdConsent) r,
        Member ExternalAccess r,
        Member FederatorAccess r,
        Member GundeckAccess r,
        Member (Input Env) r,
        Member (Input Opts) r,
        Member (Input UTCTime) r,
        Member LegalHoldStore r,
        Member MemberStore r,
        Member ProposalStore r,
        Member TeamStore r,
        Member TinyLog r
      ) =>
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

performConversationAccessData ::
  ( HasConversationActionEffects 'ConversationAccessDataTag r
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
    lcnv = fmap convId lconv
    conv = tUnqualified lconv

    maybeRemoveBots :: BotsAndMembers -> Sem r BotsAndMembers
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

data LocalConversationUpdate = LocalConversationUpdate
  { lcuEvent :: Event,
    lcuUpdate :: ConversationUpdate
  }

updateLocalConversation ::
  forall tag r.
  ( Member ConversationStore r,
    Member (ErrorS ('ActionDenied (ConversationActionPermission tag))) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'ConvNotFound) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member GundeckAccess r,
    Member (Input UTCTime) r,
    Member (Logger (Log.Msg -> Log.Msg)) r,
    HasConversationActionEffects tag r,
    SingI tag
  ) =>
  Local ConvId ->
  Qualified UserId ->
  Maybe ConnId ->
  ConversationAction tag ->
  Sem r LocalConversationUpdate
updateLocalConversation lcnv qusr con action = do
  let tag = sing @tag

  -- retrieve conversation
  conv <- getConversationWithError lcnv

  -- check that the action does not bypass the underlying protocol
  unless (protocolValidAction (convProtocol conv) (fromSing tag)) $
    throwS @'InvalidOperation

  -- perform all authorisation checks and, if successful, the update itself
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
    Member (ErrorS ('ActionDenied (ConversationActionPermission tag))) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member GundeckAccess r,
    Member (Input UTCTime) r,
    Member (Logger (Log.Msg -> Log.Msg)) r,
    HasConversationActionEffects tag r
  ) =>
  Local Conversation ->
  Qualified UserId ->
  Maybe ConnId ->
  ConversationAction tag ->
  Sem r LocalConversationUpdate
updateLocalConversationUnchecked lconv qusr con action = do
  let tag = sing @tag
      lcnv = fmap convId lconv
      conv = tUnqualified lconv

  -- retrieve member
  self <- noteS @'ConvNotFound $ getConvMember lconv conv qusr

  -- perform checks
  ensureConversationActionAllowed (sing @tag) lcnv action conv self

  -- perform action
  (extraTargets, action') <- performAction tag qusr lconv action

  notifyConversationAction
    -- Removing members should be fault tolerant.
    ( case tag of
        SConversationRemoveMembersTag -> False
        _ -> True
    )
    (sing @tag)
    qusr
    False
    con
    lconv
    (convBotsAndMembers conv <> extraTargets)
    action'

-- --------------------------------------------------------------------------------
-- -- Utilities

ensureConversationActionAllowed ::
  forall tag mem x r.
  ( IsConvMember mem,
    HasConversationActionEffects tag r,
    ( Member (ErrorS ('ActionDenied (ConversationActionPermission tag))) r,
      Member (ErrorS 'InvalidOperation) r
    )
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
  ( Member MemberStore r,
    Member (Error NoChanges) r
  ) =>
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
  ( Member FederatorAccess r,
    Member ExternalAccess r,
    Member GundeckAccess r,
    Member (Input UTCTime) r,
    Member (Logger (Log.Msg -> Log.Msg)) r
  ) =>
  Bool ->
  Sing tag ->
  Qualified UserId ->
  Bool ->
  Maybe ConnId ->
  Local Conversation ->
  BotsAndMembers ->
  ConversationAction (tag :: ConversationActionTag) ->
  Sem r LocalConversationUpdate
notifyConversationAction failEarly tag quid notifyOrigDomain con lconv targets action = do
  now <- input
  let lcnv = fmap convId lconv
      conv = tUnqualified lconv
      e = conversationActionToEvent tag now quid (tUntagged lcnv) Nothing action

  let mkUpdate uids =
        ConversationUpdate
          now
          quid
          (tUnqualified lcnv)
          uids
          (SomeConversationAction tag action)

  -- call `on-new-remote-conversation` on backends that are seeing this
  -- conversation for the first time
  let newDomains =
        Set.difference
          (Set.map void (bmRemotes targets))
          (Set.fromList (map (void . rmId) (convRemoteMembers conv)))
  let nrc =
        NewRemoteConversation
          { nrcConvId = convId conv,
            nrcProtocol = convProtocol conv
          }
  let errorIntolerant = do
        E.runFederatedConcurrently_ (toList newDomains) $ \_ -> do
          void $ fedClient @'Galley @"on-new-remote-conversation" nrc
        fmap (fromMaybe (mkUpdate []) . asum . map tUnqualified)
          . E.runFederatedConcurrently (toList (bmRemotes targets))
          $ \ruids -> do
            let update = mkUpdate (tUnqualified ruids)
            -- if notifyOrigDomain is false, filter out user from quid's domain,
            -- because quid's backend will update local state and notify its users
            -- itself using the ConversationUpdate returned by this function
            if notifyOrigDomain || tDomain ruids /= qDomain quid
              then fedClient @'Galley @"on-conversation-updated" update $> Nothing
              else pure (Just update)
      errorTolerant = do
        fedEithers <- E.runFederatedConcurrentlyEither (toList newDomains) $ \_ -> do
          void $ fedClient @'Galley @"on-new-remote-conversation" nrc
        for_ fedEithers $
          either
            (logError "on-new-remote-conversation" "An error occurred while communicating with federated server: ")
            (pure . tUnqualified)
        updates <-
          E.runFederatedConcurrentlyEither (toList (bmRemotes targets)) $
            \ruids -> do
              let update = mkUpdate (tUnqualified ruids)
              -- if notifyOrigDomain is false, filter out user from quid's domain,
              -- because quid's backend will update local state and notify its users
              -- itself using the ConversationUpdate returned by this function
              if notifyOrigDomain || tDomain ruids /= qDomain quid
                then fedClient @'Galley @"on-conversation-updated" update $> Nothing
                else pure (Just update)
        let f = fromMaybe (mkUpdate []) . asum . map tUnqualified . rights
            update = f updates
        for_ (lefts updates) $
          logError
            "on-conversation-update"
            "An error occurred while communicating with federated server: "
        pure update

  update <- if failEarly then errorIntolerant else errorTolerant

  -- notify local participants and bots
  pushConversationEvent con e (qualifyAs lcnv (bmLocals targets)) (bmBots targets)

  -- return both the event and the 'ConversationUpdate' structure corresponding
  -- to the originating domain (if it is remote)
  pure $ LocalConversationUpdate e update
  where
    logError :: Show a => String -> String -> (a, FederationError) -> Sem r ()
    logError field msg e =
      P.warn $
        Log.field "federation call" field . Log.msg (msg <> show e)

-- | Notify all local members about a remote conversation update that originated
-- from a local user
notifyRemoteConversationAction ::
  ( Member ExternalAccess r,
    Member GundeckAccess r,
    Member MemberStore r,
    Member P.TinyLog r
  ) =>
  Local x ->
  Remote ConversationUpdate ->
  Maybe ConnId ->
  Sem r Event
notifyRemoteConversationAction loc rconvUpdate con = do
  let convUpdate = tUnqualified rconvUpdate
      rconvId = qualifyAs rconvUpdate . cuConvId $ convUpdate

  let event =
        case cuAction convUpdate of
          SomeConversationAction tag action ->
            conversationActionToEvent tag (cuTime convUpdate) (cuOrigUserId convUpdate) (tUntagged rconvId) Nothing action

  -- Note: we generally do not send notifications to users that are not part of
  -- the conversation (from our point of view), to prevent spam from the remote
  -- backend.
  (presentUsers, allUsersArePresent) <-
    E.selectRemoteMembers (cuAlreadyPresentUsers convUpdate) rconvId
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

  pushConversationEvent con event localPresentUsers bots $> event

-- | Kick a user from a conversation and send notifications.
--
-- This function removes the given victim from the conversation by making them
-- leave, but then sends notifications as if the user was removed by someone
-- else.
kickMember ::
  ( Member (Error InternalError) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member GundeckAccess r,
    Member ProposalStore r,
    Member (Input UTCTime) r,
    Member (Input Env) r,
    Member MemberStore r,
    Member TinyLog r
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
    False
    (sing @'ConversationRemoveMembersTag)
    qusr
    True
    Nothing
    lconv
    (targets <> extraTargets)
    (pure victim)

notifyTypingIndicator ::
  ( Member (Input UTCTime) r,
    Member (Input (Local ())) r,
    Member GundeckAccess r,
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
  let tdu users =
        TypingDataUpdated
          { tudTime = now,
            tudOrigUserId = qusr,
            tudConvId = Data.convId conv,
            tudUsersInConv = users,
            tudTypingStatus = ts
          }

  void $ E.runFederatedConcurrentlyEither (fmap rmId remoteMemsOther) $ \rmems -> do
    fedClient @'Galley @"on-typing-indicator-updated" (tdu (tUnqualified rmems))

  pure (tdu (fmap (tUnqualified . rmId) remoteMemsOrig))

pushTypingIndicatorEvents ::
  (Member GundeckAccess r) =>
  Qualified UserId ->
  UTCTime ->
  [UserId] ->
  Maybe ConnId ->
  Qualified ConvId ->
  TypingStatus ->
  Sem r ()
pushTypingIndicatorEvents qusr tEvent users mcon qcnv ts = do
  let e = Event qcnv Nothing qusr tEvent (EdTyping ts)
  for_ (newPushLocal ListComplete (qUnqualified qusr) (ConvEvent e) (userRecipient <$> users)) $ \p ->
    push1 $
      p
        & pushConn .~ mcon
        & pushRoute .~ RouteDirect
        & pushTransient .~ True
