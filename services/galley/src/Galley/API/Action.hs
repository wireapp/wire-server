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

module Galley.API.Action
  ( -- * Conversation action class
    IsConversationAction (..),

    -- * Conversation action types
    ConversationDelete (..),
    ConversationJoin (..),
    ConversationLeave (..),
    ConversationMemberUpdate (..),

    -- * Performing actions
    updateLocalConversation,
    NoChanges,

    -- * Utilities
    ensureConversationActionAllowed,
    addMembersToLocalConversation,
    notifyConversationAction,
  )
where

import qualified Brig.Types.User as User
import Control.Arrow
import Control.Lens
import Data.Id
import Data.Kind
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Map as Map
import Data.Misc
import Data.Qualified
import qualified Data.Set as Set
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
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import Wire.API.Team.LegalHold
import Wire.API.Team.Member

data NoChanges = NoChanges

noChanges :: Member (Error NoChanges) r => Sem r a
noChanges = throw NoChanges

-- | An update to a conversation, including addition and removal of members.
-- Used to send notifications to users and to remote backends.
class IsConversationAction a where
  type HasConversationActionEffects a (r :: EffectRow) :: Constraint

  conversationAction :: a -> ConversationAction
  ensureAllowed ::
    (IsConvMember mem, HasConversationActionEffects a r) =>
    Local x ->
    a ->
    Conversation ->
    mem ->
    Sem r ()
  ensureAllowed _ _ _ _ = pure ()
  conversationActionTag' :: Qualified UserId -> a -> Action
  performAction ::
    ( HasConversationActionEffects a r,
      Members '[ConversationStore, Error NoChanges] r
    ) =>
    Qualified UserId ->
    Local ConvId ->
    Conversation ->
    a ->
    Sem r (BotsAndMembers, a)

-- | The action of some users joining a conversation.
data ConversationJoin = ConversationJoin
  { cjUsers :: NonEmpty (Qualified UserId),
    cjRole :: RoleName
  }

-- | The action of some users leaving a conversation.
newtype ConversationLeave = ConversationLeave
  {clUsers :: NonEmpty (Qualified UserId)}

-- | The action of promoting/demoting a member of a conversation.
data ConversationMemberUpdate = ConversationMemberUpdate
  { cmuTarget :: Qualified UserId,
    cmuUpdate :: OtherMemberUpdate
  }

-- | The action of deleting a conversation.
data ConversationDelete = ConversationDelete

instance IsConversationAction ConversationJoin where
  type
    HasConversationActionEffects ConversationJoin r =
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
           TeamStore
         ]
        r

  conversationAction cj = ConversationActionAddMembers (cjUsers cj) (cjRole cj)
  ensureAllowed _ cj _ self = ensureConvRoleNotElevated self (cjRole cj)
  conversationActionTag' _ _ = AddConversationMember
  performAction qusr lcnv conv (ConversationJoin invited role) = do
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
        ensureAccessRole (convAccessRole conv) userMembershipMap
        ensureConnectedOrSameTeam lusr newUsers
      checkLocals lusr Nothing newUsers = do
        ensureAccessRole (convAccessRole conv) (zip newUsers $ repeat Nothing)
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
                  let qvictim = qUntagged (qualifyAs lcnv (lmId mem))
                  void . runError @NoChanges $
                    updateLocalConversation lcnv qvictim Nothing $
                      ConversationLeave (pure qvictim)
            else throw MissingLegalholdConsent

      checkLHPolicyConflictsRemote ::
        FutureWork 'LegalholdPlusFederationNotImplemented [Remote UserId] ->
        Sem r ()
      checkLHPolicyConflictsRemote _remotes = pure ()

instance IsConversationAction ConversationLeave where
  type
    HasConversationActionEffects ConversationLeave r =
      (Members '[MemberStore] r)
  conversationAction cl = ConversationActionRemoveMembers (clUsers cl)
  conversationActionTag' qusr a
    | pure qusr == clUsers a = LeaveConversation
    | otherwise = RemoveConversationMember
  performAction _qusr lcnv conv action = do
    let presentVictims = filter (isConvMember lcnv conv) (toList (clUsers action))
    when (null presentVictims) noChanges
    E.deleteMembers (convId conv) (toUserList lcnv presentVictims)
    pure (mempty, action) -- FUTUREWORK: should we return the filtered action here?

instance IsConversationAction ConversationMemberUpdate where
  type
    HasConversationActionEffects ConversationMemberUpdate r =
      (Members '[MemberStore, Error ConversationError] r)
  conversationAction cmu = ConversationActionMemberUpdate (cmuTarget cmu) (cmuUpdate cmu)
  conversationActionTag' _ _ = ModifyOtherConversationMember
  performAction _qusr lcnv conv action = do
    void $ ensureOtherMember lcnv (cmuTarget action) conv
    E.setOtherMember lcnv (cmuTarget action) (cmuUpdate action)
    pure (mempty, action)

instance IsConversationAction ConversationDelete where
  type
    HasConversationActionEffects ConversationDelete r =
      Members '[Error FederationError, Error NotATeamMember, CodeStore, TeamStore] r
  conversationAction ConversationDelete = ConversationActionDelete
  ensureAllowed loc ConversationDelete conv self =
    for_ (convTeam conv) $ \tid -> do
      lusr <- ensureLocal loc (convMemberId loc self)
      void $ E.getTeamMember tid (tUnqualified lusr) >>= noteED @NotATeamMember
  conversationActionTag' _ _ = DeleteConversation
  performAction _ lcnv conv action = do
    key <- E.makeKey (tUnqualified lcnv)
    E.deleteCode key ReusableCode
    case convTeam conv of
      Nothing -> E.deleteConversation (tUnqualified lcnv)
      Just tid -> E.deleteTeamConversation tid (tUnqualified lcnv)
    pure (mempty, action)

instance IsConversationAction ConversationRename where
  type
    HasConversationActionEffects ConversationRename r =
      Members '[Error ActionError, Error InvalidInput] r

  conversationAction = ConversationActionRename
  conversationActionTag' _ _ = ModifyConversationName
  performAction _ lcnv _ action = do
    cn <- rangeChecked (cupName action)
    E.setConversationName (tUnqualified lcnv) cn
    pure (mempty, action)

instance IsConversationAction ConversationMessageTimerUpdate where
  type HasConversationActionEffects ConversationMessageTimerUpdate r = ()
  conversationAction = ConversationActionMessageTimerUpdate
  conversationActionTag' _ _ = ModifyConversationMessageTimer
  performAction _ lcnv conv action = do
    when (convMessageTimer conv == cupMessageTimer action) noChanges
    E.setConversationMessageTimer (tUnqualified lcnv) (cupMessageTimer action)
    pure (mempty, action)

instance IsConversationAction ConversationReceiptModeUpdate where
  type HasConversationActionEffects ConversationReceiptModeUpdate r = ()
  conversationAction = ConversationActionReceiptModeUpdate
  conversationActionTag' _ _ = ModifyConversationReceiptMode
  performAction _ lcnv conv action = do
    when (convReceiptMode conv == Just (cruReceiptMode action)) noChanges
    E.setConversationReceiptMode (tUnqualified lcnv) (cruReceiptMode action)
    pure (mempty, action)

instance IsConversationAction ConversationAccessData where
  type
    HasConversationActionEffects ConversationAccessData r =
      Members
        '[ BotAccess,
           BrigAccess,
           CodeStore,
           Error ActionError,
           Error InvalidInput,
           ExternalAccess,
           FederatorAccess,
           FireAndForget,
           GundeckAccess,
           MemberStore,
           TeamStore,
           Input UTCTime
         ]
        r
  conversationAction = ConversationActionAccessUpdate
  ensureAllowed _ target conv self = do
    -- 'PrivateAccessRole' is for self-conversations, 1:1 conversations and
    -- so on; users are not supposed to be able to make other conversations
    -- have 'PrivateAccessRole'
    when
      ( PrivateAccess `elem` cupAccess target
          || PrivateAccessRole == cupAccessRole target
      )
      $ throw InvalidTargetAccess
    -- Team conversations incur another round of checks
    case convTeam conv of
      Just _ -> do
        -- Access mode change might result in members being removed from the
        -- conversation, so the user must have the necessary permission flag
        ensureActionAllowed RemoveConversationMember self
      Nothing ->
        when (cupAccessRole target == TeamAccessRole) $
          throw InvalidTargetAccess
  conversationActionTag' _ _ = ModifyConversationAccess
  performAction qusr lcnv conv action = do
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
    let filterBotsAndMembers = filterActivated >=> filterTeammates
    let current = convBotsAndMembers conv -- initial bots and members
    desired <- filterBotsAndMembers current -- desired bots and members
    let toRemove = bmDiff current desired -- bots and members to be removed

    -- Update Cassandra
    E.setConversationAccess (tUnqualified lcnv) action
    E.fireAndForget $ do
      -- Remove bots
      traverse_ (E.deleteBot (tUnqualified lcnv) . botMemId) (bmBots toRemove)

      -- Update current bots and members
      let current' = current {bmBots = bmBots desired}

      -- Remove users and notify everyone
      void . for_ (nonEmpty (bmQualifiedMembers lcnv toRemove)) $ \usersToRemove -> do
        let rAction = ConversationLeave usersToRemove
        void . runError @NoChanges $ performAction qusr lcnv conv rAction
        notifyConversationAction qusr Nothing lcnv current' (conversationAction rAction)
    pure (mempty, action)
    where
      filterActivated :: Member BrigAccess r => BotsAndMembers -> Sem r BotsAndMembers
      filterActivated bm
        | convAccessRole conv > ActivatedAccessRole
            && cupAccessRole action <= ActivatedAccessRole = do
          activated <- map User.userId <$> E.lookupActivatedUsers (toList (bmLocals bm))
          -- FUTUREWORK: should we also remove non-activated remote users?
          pure $ bm {bmLocals = Set.fromList activated}
        | otherwise = pure bm

      filterTeammates :: Member TeamStore r => BotsAndMembers -> Sem r BotsAndMembers
      filterTeammates bm = do
        -- In a team-only conversation we also want to remove bots and guests
        case (cupAccessRole action, convTeam conv) of
          (TeamAccessRole, Just tid) -> do
            onlyTeamUsers <- flip filterM (toList (bmLocals bm)) $ \user ->
              isJust <$> E.getTeamMember tid user
            pure $
              BotsAndMembers
                { bmLocals = Set.fromList onlyTeamUsers,
                  bmBots = mempty,
                  bmRemotes = mempty
                }
          _ -> pure bm

-- | Update a local conversation, and notify all local and remote members.
updateLocalConversation ::
  ( IsConversationAction a,
    Members
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
    HasConversationActionEffects a r
  ) =>
  Local ConvId ->
  Qualified UserId ->
  Maybe ConnId ->
  a ->
  Sem r Event
updateLocalConversation lcnv qusr con action = do
  -- retrieve conversation
  (conv, self) <- getConversationAndMemberWithError ConvNotFound qusr lcnv

  -- perform checks
  ensureConversationActionAllowed lcnv action conv self

  -- perform action
  (extraTargets, action') <- performAction qusr lcnv conv action

  -- send notifications to both local and remote users
  notifyConversationAction
    qusr
    con
    lcnv
    (convBotsAndMembers conv <> extraTargets)
    (conversationAction action')

--------------------------------------------------------------------------------
-- Utilities

ensureConversationActionAllowed ::
  ( IsConvMember mem,
    IsConversationAction a,
    HasConversationActionEffects a r,
    Members '[Error ActionError, Error InvalidInput] r
  ) =>
  Local x ->
  a ->
  Conversation ->
  mem ->
  Sem r ()
ensureConversationActionAllowed loc action conv self = do
  let tag = conversationActionTag' (convMemberId loc self) action
  -- general action check
  ensureActionAllowed tag self
  -- check if it is a group conversation (except for rename actions)
  when (tag /= ModifyConversationName) $
    ensureGroupConversation conv
  -- extra action-specific checks
  ensureAllowed loc action conv self

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
  Members '[FederatorAccess, ExternalAccess, GundeckAccess, Input UTCTime] r =>
  Qualified UserId ->
  Maybe ConnId ->
  Local ConvId ->
  BotsAndMembers ->
  ConversationAction ->
  Sem r Event
notifyConversationAction quid con lcnv targets action = do
  now <- input
  let e = conversationActionToEvent now quid (qUntagged lcnv) action

  -- notify remote participants
  E.runFederatedConcurrently_ (toList (bmRemotes targets)) $ \ruids ->
    fedClient @'Galley @VL @"on-conversation-updated" $
      ConversationUpdate now quid (tUnqualified lcnv) (tUnqualified ruids) action

  -- notify local participants and bots
  pushConversationEvent con e (qualifyAs lcnv (bmLocals targets)) (bmBots targets) $> e
