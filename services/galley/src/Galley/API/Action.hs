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
    ConversationDelete (..),
    ConversationJoin (..),
    ConversationLeave (..),
    ConversationMemberUpdate (..),
    HasConversationActionEffects,

    -- * Performing actions
    updateLocalConversationWithLocalUser,
    updateLocalConversationWithRemoteUser,
    NoChanges (..),

    -- * Utilities
    ensureConversationActionAllowed,
    addMembersToLocalConversation,
    notifyConversationAction,
    ConversationUpdate,
  )
where

import qualified Brig.Types.User as User
import Control.Arrow
import Control.Lens
import Data.Id
import Data.Kind
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map as Map
import Data.Misc
import Data.Qualified
import qualified Data.Set as S
import qualified Data.Set as Set
import Data.Singletons (Sing, SingI, demote, sing)
import Data.Time.Clock
import Galley.API.Error
import Galley.API.Util
import Galley.Data.Conversation
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
import Wire.API.Conversation hiding (Conversation, Member)
import Wire.API.Conversation.Action
import Wire.API.Conversation.Role
import Wire.API.ErrorDescription
import Wire.API.Event.Conversation hiding (Conversation)
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
         Error ActionError,
         Error ConversationError,
         Error FederationError,
         Error InvalidInput,
         Error LegalHoldError,
         Error NotATeamMember,
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
    (Members '[MemberStore, Error ConversationError] r)
  HasConversationActionEffects 'ConversationDeleteTag r =
    Members '[Error FederationError, Error NotATeamMember, CodeStore, TeamStore, ConversationStore] r
  HasConversationActionEffects 'ConversationRenameTag r =
    Members '[Error ActionError, Error InvalidInput, ConversationStore] r
  HasConversationActionEffects 'ConversationAccessDataTag r =
    Members
      '[ BotAccess,
         BrigAccess,
         CodeStore,
         Error ActionError,
         Error InvalidInput,
         Error InvalidInput,
         Error NoChanges,
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

-- -- | A sum type consisting of all possible conversation actions.
-- data ConversationAction
--   = ConversationActionAddMembers (NonEmpty (Qualified UserId)) RoleName
--   | ConversationActionRemoveMembers (NonEmpty (Qualified UserId))
--   | ConversationActionRename ConversationRename
--   | ConversationActionMessageTimerUpdate ConversationMessageTimerUpdate
--   | ConversationActionReceiptModeUpdate ConversationReceiptModeUpdate
--   | ConversationActionMemberUpdate (Qualified UserId) OtherMemberUpdate
--   | ConversationActionAccessUpdate ConversationAccessData
--   | ConversationActionDelete
--   deriving stock (Eq, Show, Generic)
--   deriving (Arbitrary) via (GenericUniform ConversationAction)
--   deriving (ToJSON, FromJSON) via (CustomEncoded ConversationAction)

noChanges :: Member (Error NoChanges) r => Sem r a
noChanges = throw NoChanges

ensureAllowed ::
  forall tag mem r x.
  (IsConvMember mem, SingI tag, HasConversationActionEffects tag r) =>
  Local x ->
  ConversationAction tag ->
  Conversation ->
  mem ->
  Sem r ()
ensureAllowed loc action conv origUser = do
  case (sing @tag) of
    SConversationJoinTag -> ensureConvRoleNotElevated origUser (cjRole action)
    SConversationDeleteTag ->
      for_ (convTeam conv) $ \tid -> do
        lusr <- ensureLocal loc (convMemberId loc origUser)
        void $ E.getTeamMember tid (tUnqualified lusr) >>= noteED @NotATeamMember
    SConversationAccessDataTag -> do
      -- 'PrivateAccessRole' is for self-conversations, 1:1 conversations and
      -- so on; users are not supposed to be able to make other conversations
      -- have 'PrivateAccessRole'
      when (PrivateAccess `elem` cupAccess action || Set.null (cupAccessRoles action)) $
        throw InvalidTargetAccess
      -- Team conversations incur another round of checks
      case convTeam conv of
        Just _ -> do
          -- Access mode change might result in members being removed from the
          -- conversation, so the user must have the necessary permission flag
          ensureActionAllowed RemoveConversationMember origUser
        Nothing ->
          -- not a team conv, so one of the other access roles has to allow this.
          when (Set.null $ cupAccessRoles action Set.\\ Set.fromList [TeamMemberAccessRole]) $
            throw InvalidTargetAccess
    _ -> pure ()

-- | Returns additional members that resulted from the action (e.g. ConversationJoin)
-- and also returns the (possible modified) action that was performed
performAction ::
  forall tag r.
  ( HasConversationActionEffects tag r,
    SingI tag
  ) =>
  Qualified UserId ->
  Local ConvId ->
  Conversation ->
  ConversationAction tag ->
  Sem r (BotsAndMembers, ConversationAction tag)
performAction origUser lcnv cnv action =
  case (sing @tag) of
    SConversationJoinTag -> do
      performConversationJoin origUser lcnv cnv action
    SConversationLeaveTag -> do
      let presentVictims = filter (isConvMember lcnv cnv) (toList (clUsers action))
      when (null presentVictims) noChanges
      E.deleteMembers (convId cnv) (toUserList lcnv presentVictims)
      pure (mempty, action) -- FUTUREWORK: should we return the filtered action here?
    SConversationRemoveMembersTag -> do
      let presentVictims = filter (isConvMember lcnv cnv) (toList (crmTargets action))
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
      when (convMessageTimer cnv == cupMessageTimer action) noChanges
      E.setConversationMessageTimer (tUnqualified lcnv) (cupMessageTimer action)
      pure (mempty, action)
    SConversationReceiptModeUpdateTag -> do
      when (convReceiptMode cnv == Just (cruReceiptMode action)) noChanges
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
           Error ActionError,
           Error ConversationError,
           Error NotATeamMember,
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
           Error ActionError,
           Error FederationError,
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
           Error ActionError,
           Error ConversationError,
           Error LegalHoldError,
           Error InvalidInput,
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
          throw MissingLegalholdConsent

      whenM (anyLegalholdActivated newUsers) $ do
        unless allNewUsersGaveConsent $
          throw MissingLegalholdConsent

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
                    ConversationLeave (pure (qUntagged lvictim))
          else throw MissingLegalholdConsent

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
      let rAction = ConversationLeave usersToRemove
      void . runError @NoChanges $ performAction @'ConversationLeaveTag qusr lcnv conv rAction
      notifyConversationAction (sing @'ConversationLeaveTag) qusr Nothing lcnv bmToNotify rAction
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
         Error ActionError,
         Error ConversationError,
         Error InvalidInput,
         Error NoChanges,
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
  -- retrieve conversation
  (conv, self) <- getConversationAndMemberWithError ConvNotFound lusr lcnv

  -- perform checks
  ensureConversationActionAllowed @tag lcnv action conv self

  -- perform action
  (extraTargets, action') <- performAction @tag (qUntagged lusr) lcnv conv action

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
         Error ActionError,
         Error ConversationError,
         Error InvalidInput,
         Error NoChanges,
         ExternalAccess,
         FederatorAccess,
         GundeckAccess,
         Input UTCTime
       ]
      r,
    HasConversationActionEffects tag r,
    SingI tag
  ) =>
  Sing tag ->
  Local ConvId ->
  Remote UserId ->
  ConversationAction tag ->
  Sem r ConversationUpdate
updateLocalConversationWithRemoteUser _tag lcnv rusr action = do
  -- retrieve conversation
  (conv, self) <- getConversationAndMemberWithError ConvNotFound (qUntagged rusr) lcnv

  -- perform checks
  ensureConversationActionAllowed @tag lcnv action conv self

  -- perform action
  (extraTargets, action') <- performAction @tag (qUntagged rusr) lcnv conv action

  -- filter out user from rusr's domain, because rusr's backend will update
  -- local state and notify its users itself using the ConversationUpdate
  -- returned by this function
  let targets = convBotsAndMembers conv <> extraTargets
      remotes = bmRemotes targets
      remotesUserDomain = S.filter ((== tDomain rusr) . tDomain) remotes
      remotesOtherDomain = remotes Set.\\ remotesUserDomain

  void $
    notifyConversationAction
      (sing @tag)
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
        cuAction = SomeConversationAction @tag sing action'
      }

-- --------------------------------------------------------------------------------
-- -- Utilities

ensureConversationActionAllowed ::
  forall tag mem x r.
  ( IsConvMember mem,
    HasConversationActionEffects tag r,
    Members '[Error ActionError, Error InvalidInput] r,
    SingI tag
  ) =>
  Local x ->
  ConversationAction (tag :: ConversationActionTag) ->
  Conversation ->
  mem ->
  Sem r ()
ensureConversationActionAllowed loc action conv self = do
  -- general action check
  ensureActionAllowed (conversationActionPermission (demote @tag)) self

  -- check if it is a group conversation (except for rename actions)
  when (demote @tag /= ConversationRenameTag) $
    ensureGroupConversation conv

  -- extra action-specific checks
  ensureAllowed @tag loc action conv self

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
  (SingI tag) =>
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
  let e = conversationActionToEvent (sing @tag) now quid (qUntagged lcnv) action

  E.runFederatedConcurrently_ (toList (bmRemotes targets)) $ \ruids ->
    fedClient @'Galley @"on-conversation-updated" $
      ConversationUpdate now quid (tUnqualified lcnv) (tUnqualified ruids) (SomeConversationAction tag action)

  -- notify local participants and bots
  pushConversationEvent con e (qualifyAs lcnv (bmLocals targets)) (bmBots targets) $> e
