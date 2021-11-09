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

    -- * Utilities
    ensureConversationActionAllowed,
    addMembersToLocalConversation,
    notifyConversationAction,
  )
where

import qualified Brig.Types.User as User
import Control.Lens
import Control.Monad.Trans.Maybe
import Data.Id
import Data.Kind
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Misc
import Data.Qualified
import qualified Data.Set as Set
import Data.Time.Clock
import Galley.API.Error
import Galley.API.Util
import Galley.App
import Galley.Data.Conversation
import Galley.Data.Services
import Galley.Data.Types
import Galley.Effects
import qualified Galley.Effects.BotAccess as E
import qualified Galley.Effects.BrigAccess as E
import qualified Galley.Effects.CodeStore as E
import qualified Galley.Effects.ConversationStore as E
import qualified Galley.Effects.FederatorAccess as E
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
import qualified Wire.API.Federation.API.Galley as F
import Wire.API.Federation.Client
import Wire.API.Team.LegalHold
import Wire.API.Team.Member

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
    Galley r ()
  ensureAllowed _ _ _ _ = pure ()
  conversationActionTag' :: Qualified UserId -> a -> Action
  performAction ::
    ( HasConversationActionEffects a r,
      Members '[ConversationStore] r
    ) =>
    Qualified UserId ->
    Local ConvId ->
    Conversation ->
    a ->
    MaybeT (Galley r) (BotsAndMembers, a)

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

    lift $ do
      lusr <- liftSem $ ensureLocal lcnv qusr
      ensureMemberLimit (toList (convLocalMembers conv)) newMembers
      liftSem $ ensureAccess conv InviteAccess
      checkLocals lusr (convTeam conv) (ulLocals newMembers)
      checkRemotes lusr (ulRemotes newMembers)
      checkLHPolicyConflictsLocal (ulLocals newMembers)
      checkLHPolicyConflictsRemote (FutureWork (ulRemotes newMembers))

    addMembersToLocalConversation lcnv newMembers role
    where
      userIsMember u = (^. userId . to (== u))

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
        Galley r ()
      checkLocals lusr (Just tid) newUsers = do
        tms <- liftSem $ E.selectTeamMembers tid newUsers
        let userMembershipMap = map (\u -> (u, find (userIsMember u) tms)) newUsers
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
             FederatorAccess,
             TeamStore
           ]
          r =>
        Local UserId ->
        [Remote UserId] ->
        Galley r ()
      checkRemotes lusr remotes = do
        -- if federator is not configured, we fail early, so we avoid adding
        -- remote members to the database
        liftSem . unless (null remotes) $
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
             LegalHoldStore,
             MemberStore,
             TeamStore
           ]
          r =>
        [UserId] ->
        Galley r ()
      checkLHPolicyConflictsLocal newUsers = do
        let convUsers = convLocalMembers conv

        allNewUsersGaveConsent <- allLegalholdConsentGiven newUsers

        whenM (anyLegalholdActivated (lmId <$> convUsers)) $
          unless allNewUsersGaveConsent $
            liftSem $ throw MissingLegalholdConsent

        whenM (anyLegalholdActivated newUsers) $ do
          unless allNewUsersGaveConsent $
            liftSem $ throw MissingLegalholdConsent

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
                  void . runMaybeT $
                    updateLocalConversation lcnv qvictim Nothing $
                      ConversationLeave (pure qvictim)
            else liftSem $ throw MissingLegalholdConsent

      checkLHPolicyConflictsRemote ::
        FutureWork 'LegalholdPlusFederationNotImplemented [Remote UserId] ->
        Galley r ()
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
    guard . not . null $ presentVictims
    lift . liftSem $ E.deleteMembers (convId conv) (toUserList lcnv presentVictims)
    pure (mempty, action) -- FUTUREWORK: should we return the filtered action here?

instance IsConversationAction ConversationMemberUpdate where
  type
    HasConversationActionEffects ConversationMemberUpdate r =
      (Members '[MemberStore, Error ConversationError] r)
  conversationAction cmu = ConversationActionMemberUpdate (cmuTarget cmu) (cmuUpdate cmu)
  conversationActionTag' _ _ = ModifyOtherConversationMember
  performAction _qusr lcnv conv action = lift . liftSem $ do
    void $ ensureOtherMember lcnv (cmuTarget action) conv
    E.setOtherMember lcnv (cmuTarget action) (cmuUpdate action)
    pure (mempty, action)

instance IsConversationAction ConversationDelete where
  type
    HasConversationActionEffects ConversationDelete r =
      Members '[Error FederationError, Error NotATeamMember, CodeStore, TeamStore] r
  conversationAction ConversationDelete = ConversationActionDelete
  ensureAllowed loc ConversationDelete conv self =
    liftSem . for_ (convTeam conv) $ \tid -> do
      lusr <- ensureLocal loc (convMemberId loc self)
      void $ E.getTeamMember tid (tUnqualified lusr) >>= noteED @NotATeamMember
  conversationActionTag' _ _ = DeleteConversation
  performAction _ lcnv conv action = lift . liftSem $ do
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
  performAction _ lcnv _ action = lift . liftSem $ do
    cn <- rangeChecked (cupName action)
    E.setConversationName (tUnqualified lcnv) cn
    pure (mempty, action)

instance IsConversationAction ConversationMessageTimerUpdate where
  type HasConversationActionEffects ConversationMessageTimerUpdate r = ()
  conversationAction = ConversationActionMessageTimerUpdate
  conversationActionTag' _ _ = ModifyConversationMessageTimer
  performAction _ lcnv conv action = do
    guard $ convMessageTimer conv /= cupMessageTimer action
    lift . liftSem $ E.setConversationMessageTimer (tUnqualified lcnv) (cupMessageTimer action)
    pure (mempty, action)

instance IsConversationAction ConversationReceiptModeUpdate where
  type HasConversationActionEffects ConversationReceiptModeUpdate r = ()
  conversationAction = ConversationActionReceiptModeUpdate
  conversationActionTag' _ _ = ModifyConversationReceiptMode
  performAction _ lcnv conv action = do
    guard $ convReceiptMode conv /= Just (cruReceiptMode action)
    lift . liftSem $ E.setConversationReceiptMode (tUnqualified lcnv) (cruReceiptMode action)
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
           TeamStore
         ]
        r
  conversationAction = ConversationActionAccessUpdate
  ensureAllowed _ target conv self = do
    -- 'PrivateAccessRole' is for self-conversations, 1:1 conversations and
    -- so on; users are not supposed to be able to make other conversations
    -- have 'PrivateAccessRole'
    liftSem $
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
        liftSem $
          when (cupAccessRole target == TeamAccessRole) $
            throw InvalidTargetAccess
  conversationActionTag' _ _ = ModifyConversationAccess
  performAction qusr lcnv conv action = do
    guard $ convAccessData conv /= action
    -- Remove conversation codes if CodeAccess is revoked
    when
      ( CodeAccess `elem` convAccess conv
          && CodeAccess `notElem` cupAccess action
      )
      $ lift $ do
        key <- mkKey (tUnqualified lcnv)
        liftSem $ E.deleteCode key ReusableCode

    -- Determine bots and members to be removed
    let filterBotsAndMembers = filterActivated >=> filterTeammates
    let current = convBotsAndMembers conv -- initial bots and members
    desired <- lift . liftSem $ filterBotsAndMembers current -- desired bots and members
    let toRemove = bmDiff current desired -- bots and members to be removed

    -- Update Cassandra
    lift . liftSem $ E.setConversationAccess (tUnqualified lcnv) action
    lift . fireAndForget $ do
      -- Remove bots
      traverse_ (liftSem . E.deleteBot (tUnqualified lcnv) . botMemId) (bmBots toRemove)

      -- Update current bots and members
      let current' = current {bmBots = bmBots desired}

      -- Remove users and notify everyone
      void . for_ (nonEmpty (bmQualifiedMembers lcnv toRemove)) $ \usersToRemove -> do
        let rAction = ConversationLeave usersToRemove
        void . runMaybeT $ performAction qusr lcnv conv rAction
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
         ExternalAccess,
         FederatorAccess,
         GundeckAccess
       ]
      r,
    HasConversationActionEffects a r
  ) =>
  Local ConvId ->
  Qualified UserId ->
  Maybe ConnId ->
  a ->
  MaybeT (Galley r) Event
updateLocalConversation lcnv qusr con action = do
  -- retrieve conversation
  (conv, self) <-
    lift $
      getConversationAndMemberWithError ConvNotFound qusr lcnv

  -- perform checks
  lift $ ensureConversationActionAllowed lcnv action conv self

  -- perform action
  (extraTargets, action') <- performAction qusr lcnv conv action

  -- send notifications to both local and remote users
  lift $
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
  Galley r ()
ensureConversationActionAllowed loc action conv self = do
  let tag = conversationActionTag' (convMemberId loc self) action
  -- general action check
  ensureActionAllowed tag self
  -- check if it is a group conversation (except for rename actions)
  when (tag /= ModifyConversationName) $
    liftSem $ ensureGroupConversation conv
  -- extra action-specific checks
  ensureAllowed loc action conv self

-- | Add users to a conversation without performing any checks. Return extra
-- notification targets and the action performed.
addMembersToLocalConversation ::
  Members '[MemberStore] r =>
  Local ConvId ->
  UserList UserId ->
  RoleName ->
  MaybeT (Galley r) (BotsAndMembers, ConversationJoin)
addMembersToLocalConversation lcnv users role = do
  (lmems, rmems) <- lift . liftSem $ E.createMembers (tUnqualified lcnv) (fmap (,role) users)
  neUsers <- maybe mzero pure . nonEmpty . ulAll lcnv $ users
  let action = ConversationJoin neUsers role
  pure (bmFromMembers lmems rmems, action)

notifyConversationAction ::
  Members '[FederatorAccess, ExternalAccess, GundeckAccess] r =>
  Qualified UserId ->
  Maybe ConnId ->
  Local ConvId ->
  BotsAndMembers ->
  ConversationAction ->
  Galley r Event
notifyConversationAction quid con lcnv targets action = do
  now <- liftIO getCurrentTime
  let e = conversationActionToEvent now quid (qUntagged lcnv) action

  -- notify remote participants
  liftSem $
    E.runFederatedConcurrently_ (toList (bmRemotes targets)) $ \ruids ->
      F.onConversationUpdated F.clientRoutes (tDomain lcnv) $
        F.ConversationUpdate now quid (tUnqualified lcnv) (tUnqualified ruids) action

  -- notify local participants and bots
  pushConversationEvent con e (qualifyAs lcnv (bmLocals targets)) (bmBots targets) $> e
