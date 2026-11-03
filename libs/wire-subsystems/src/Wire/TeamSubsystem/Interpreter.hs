-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.TeamSubsystem.Interpreter where

import Control.Lens (view, (%~), (^.))
import Data.ByteString.Conversion (toByteString')
import Data.Default
import Data.Id
import Data.Json.Util
import Data.LegalHold (UserLegalHoldStatus (..), defUserLegalHoldStatus)
import Data.List.Extra qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map qualified as Map
import Data.Qualified
import Data.Time
import Galley.Types.Error
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog qualified as P
import System.Logger.Class qualified as Log
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Conversation qualified as Conv
import Wire.API.Event.Team
import Wire.API.Team.FeatureFlags (FanoutLimit)
import Wire.API.Team.HardTruncationLimit
import Wire.API.Team.LegalHold
import Wire.API.Team.LegalHold qualified as Public
import Wire.API.Team.Member
import Wire.API.Team.Member.Info (TeamMemberInfoList (TeamMemberInfoList))
import Wire.API.User.Client.Prekey
import Wire.BrigAPIAccess
import Wire.BrigAPIAccess qualified as Brig
import Wire.ConversationStore
import Wire.ConversationStore qualified as ConvStore
import Wire.ExternalAccess
import Wire.ExternalAccess qualified as ExternalAccess
import Wire.LegalHoldStore (LegalHoldStore)
import Wire.LegalHoldStore qualified as LH
import Wire.LegalHoldStore qualified as LegalHoldData
import Wire.NotificationSubsystem
import Wire.Sem.Now
import Wire.Sem.Now qualified as Now
import Wire.SparAPIAccess
import Wire.SparAPIAccess qualified as Spar
import Wire.StoredConversation
import Wire.TeamJournal
import Wire.TeamJournal qualified as Journal
import Wire.TeamStore (TeamStore)
import Wire.TeamStore qualified as TeamStore
import Wire.TeamSubsystem

newtype TeamSubsystemConfig = TeamSubsystemConfig {concurrentDeletionEvents :: Int}

interpretTeamSubsystem ::
  ( Member TeamStore r,
    Member LegalHoldStore r,
    Member BrigAPIAccess r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member Now r,
    Member SparAPIAccess r,
    Member ConversationStore r,
    Member TeamJournal r,
    Member (Input FanoutLimit) r,
    Member (Error InternalError) r,
    Member (ErrorS 'TeamNotFound) r,
    Member (ErrorS 'TeamMemberNotFound) r,
    Member P.TinyLog r
  ) =>
  TeamSubsystemConfig ->
  InterpreterFor TeamSubsystem r
interpretTeamSubsystem config =
  runInputConst config . interpretTeamSubsystemWithInputConfig . raiseUnder

interpretTeamSubsystemWithInputConfig ::
  ( Member TeamStore r,
    Member LegalHoldStore r,
    Member BrigAPIAccess r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member (Input TeamSubsystemConfig) r,
    Member (Input FanoutLimit) r,
    Member Now r,
    Member SparAPIAccess r,
    Member ConversationStore r,
    Member TeamJournal r,
    Member (Error InternalError) r,
    Member (ErrorS 'TeamNotFound) r,
    Member (ErrorS 'TeamMemberNotFound) r,
    Member P.TinyLog r
  ) =>
  InterpreterFor TeamSubsystem r
interpretTeamSubsystemWithInputConfig =
  interpret $ \case
    InternalGetTeamMember uid tid ->
      internalGetTeamMemberImpl uid tid
    InternalGetTeamMembersWithLimit tid maxResults ->
      internalGetTeamMembersWithLimitImpl tid maxResults
    InternalSelectTeamMemberInfos tid uids -> TeamMemberInfoList <$> TeamStore.selectTeamMemberInfos tid uids
    InternalSelectTeamMembers tid uids -> do
      tms <- TeamStore.selectTeamMembers tid uids
      adjustMembersForImplicitConsent tid tms
    InternalGetTeamAdmins tid -> do
      admins <-
        TeamStore.getTeamAdmins tid
          >>= TeamStore.selectTeamMembers tid
          >>= adjustMembersForImplicitConsent tid
      pure $ newTeamMemberList admins ListComplete
    InternalGetOneUserTeam uid ->
      TeamStore.getOneUserTeam uid
    InternalFinalizeDeleteTeam luid mcon tid ->
      internalFinalizeDeleteTeamImpl luid mcon tid
    GetUserStatus lzusr tid uid ->
      getUserStatusImpl lzusr tid uid
    AssertTeamExists tid ->
      assertTeamExistsImpl tid
    GetTeamMembersForFanout tid ->
      getTeamMembersForFanoutImpl tid
    GetLHStatus teamOfUser other ->
      getLHStatusImpl teamOfUser other
    GetLHStatusForUsers uids ->
      getLHStatusForUsersImpl uids

adjustMembersForImplicitConsent :: (Member LegalHoldStore r) => TeamId -> [TeamMember] -> Sem r [TeamMember]
adjustMembersForImplicitConsent tid ms = do
  hasImplicitConsent <- LH.isTeamLegalholdWhitelisted tid
  pure $ if hasImplicitConsent then map grantImplicitConsent ms else ms

grantImplicitConsent :: TeamMember -> TeamMember
grantImplicitConsent =
  legalHoldStatus %~ \case
    UserLegalHoldNoConsent -> UserLegalHoldDisabled
    UserLegalHoldDisabled -> UserLegalHoldDisabled
    UserLegalHoldPending -> UserLegalHoldPending
    UserLegalHoldEnabled -> UserLegalHoldEnabled

internalGetTeamMemberImpl ::
  ( Member TeamStore r,
    Member LegalHoldStore r
  ) =>
  UserId ->
  TeamId ->
  Sem r (Maybe TeamMember)
internalGetTeamMemberImpl uid tid = do
  tms <- TeamStore.getTeamMember tid uid
  for tms $ \tm -> do
    hasImplicitConsent <- LH.isTeamLegalholdWhitelisted tid
    pure $ if hasImplicitConsent then grantImplicitConsent tm else tm

internalGetTeamMembersWithLimitImpl ::
  ( Member TeamStore r,
    Member LegalHoldStore r
  ) =>
  TeamId ->
  Maybe FanoutLimit ->
  Sem r TeamMemberList
internalGetTeamMembersWithLimitImpl tid maxResults = do
  tmList <- TeamStore.getTeamMembersWithLimit tid (fromMaybe hardTruncationLimitRange maxResults)
  ms <- adjustMembersForImplicitConsent tid (tmList ^. teamMembers)
  pure $ newTeamMemberList ms (tmList ^. teamMemberListType)

-- This function is "unchecked" because it does not validate that the user has the `DeleteTeam` permission.
internalFinalizeDeleteTeamImpl ::
  forall r.
  ( Member BrigAPIAccess r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member (Input TeamSubsystemConfig) r,
    Member Now r,
    Member LegalHoldStore r,
    Member SparAPIAccess r,
    Member TeamStore r,
    Member ConversationStore r,
    Member TeamJournal r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  TeamId ->
  Sem r ()
internalFinalizeDeleteTeamImpl lusr zcon tid = do
  team <- TeamStore.getTeam tid
  when (isJust team) $ do
    Spar.deleteTeam tid
    now <- Now.get
    convs <- ConvStore.getTeamConversations tid
    -- Even for LARGE TEAMS, we _DO_ want to fetch all team members here because we
    -- want to generate conversation deletion events for non-team users. This should
    -- be fine as it is done once during the life team of a team and we still do not
    -- fanout this particular event to all team members anyway. And this is anyway
    -- done asynchronously

    -- No need to adjust implicit consent here.
    membs <- TeamStore.getTeamMembers tid
    (ue, be) <- foldrM (createConvDeleteEvents now membs) ([], []) convs
    let e = newEvent tid now EdTeamDelete
    pushDeleteEvents membs e ue
    ExternalAccess.deliverAsync be
    -- TODO: we don't delete bots here, but we should do that, since
    -- every bot user can only be in a single conversation. Just
    -- deleting conversations from the database is not enough.
    mapM_ (Brig.deleteUser . view userId) membs
    Journal.teamDelete tid
    LH.unsetTeamLegalholdWhitelisted tid
    TeamStore.deleteTeam tid
  where
    pushDeleteEvents :: [TeamMember] -> Event -> [Push] -> Sem r ()
    pushDeleteEvents membs e ue = do
      let r = userRecipient (tUnqualified lusr) :| membersToRecipients (Just (tUnqualified lusr)) membs

      -- To avoid DoS on gundeck, send team deletion events in chunks
      chunkSize <- inputs (.concurrentDeletionEvents)
      let chunks = List.chunksOf chunkSize (toList r)
      forM_ chunks $ \chunk ->
        -- push TeamDelete events. Note that despite having a complete list, we are guaranteed in the
        -- push module to never fan this out to more than the limit
        pushNotifications [def {origin = Just (tUnqualified lusr), json = toJSONObject e, recipients = chunk, conn = zcon}]
      -- To avoid DoS on gundeck, send conversation deletion events slowly
      pushNotificationsSlowly ue
    createConvDeleteEvents ::
      UTCTime ->
      [TeamMember] ->
      ConvId ->
      ([Push], [(BotMember, Conv.Event)]) ->
      Sem r ([Push], [(BotMember, Conv.Event)])
    createConvDeleteEvents now teamMembs cid (pp, ee) = do
      let qconvId = tUntagged $ qualifyAs lusr cid
      (bots, convMembs) <- localBotsAndUsers <$> ConvStore.getLocalMembers cid
      -- Only nonTeamMembers need to get any events, since on team deletion,
      -- all team users are deleted immediately after these events are sent
      -- and will thus never be able to see these events in practice.
      let mm = nonTeamMembers convMembs teamMembs
      let e = Conv.Event qconvId Nothing (Conv.EventFromUser (tUntagged lusr)) now (Just tid) Conv.EdConvDelete
      -- This event always contains all the required recipients
      let p =
            def
              { origin = Just (tUnqualified lusr),
                json = toJSONObject e,
                recipients = map localMemberToRecipient mm
              }
      let ee' = map (,e) bots
      let pp' = (p {conn = zcon}) : pp
      pure (pp', ee' ++ ee)

-- | Learn whether a user has LH enabled and fetch pre-keys.
-- Note that this is accessible to ANY authenticated user, even ones outside the team
getUserStatusImpl ::
  forall r.
  ( Member (Error InternalError) r,
    Member (ErrorS 'TeamMemberNotFound) r,
    Member LegalHoldData.LegalHoldStore r,
    Member TeamStore r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  TeamId ->
  UserId ->
  Sem r Public.UserLegalHoldStatusResponse
getUserStatusImpl _lzusr tid uid = do
  teamMember <- noteS @'TeamMemberNotFound =<< internalGetTeamMemberImpl uid tid
  let status = view legalHoldStatus teamMember
  (mlk, lcid) <- case status of
    UserLegalHoldNoConsent -> pure (Nothing, Nothing)
    UserLegalHoldDisabled -> pure (Nothing, Nothing)
    UserLegalHoldPending -> makeResponseDetails
    UserLegalHoldEnabled -> makeResponseDetails
  pure $ UserLegalHoldStatusResponse status mlk lcid
  where
    makeResponseDetails :: Sem r (Maybe LastPrekey, Maybe ClientId)
    makeResponseDetails = do
      mLastKey <- fmap snd <$> LegalHoldData.selectPendingPrekeys uid
      lastKey <- case mLastKey of
        Nothing -> do
          P.err
            . Log.msg
            $ "expected to find a prekey for user: "
              <> toByteString' uid
              <> " but none was found"
          throw NoPrekeyForUser
        Just lstKey -> pure lstKey
      let clientId = clientIdFromPrekey . unpackLastPrekey $ lastKey
      pure (Just lastKey, Just clientId)

assertTeamExistsImpl ::
  ( Member (ErrorS 'TeamNotFound) r,
    Member TeamStore r
  ) =>
  TeamId ->
  Sem r ()
assertTeamExistsImpl tid = do
  teamExists <- isJust <$> TeamStore.getTeam tid
  if teamExists
    then pure ()
    else throwS @'TeamNotFound

getTeamMembersForFanoutImpl ::
  ( Member TeamStore r,
    Member LegalHoldStore r,
    Member (Input FanoutLimit) r
  ) =>
  TeamId ->
  Sem r TeamMemberList
getTeamMembersForFanoutImpl tid = do
  lim <- input
  internalGetTeamMembersWithLimitImpl tid (Just lim)

-- Get legalhold status of user. Defaults to 'defUserLegalHoldStatus' if user
-- doesn't belong to a team.
getLHStatusImpl ::
  ( Member TeamStore r,
    Member LegalHoldStore r
  ) =>
  Maybe TeamId ->
  UserId ->
  Sem r UserLegalHoldStatus
getLHStatusImpl teamOfUser other = do
  case teamOfUser of
    Nothing -> pure defUserLegalHoldStatus
    Just team -> do
      mMember <- internalGetTeamMemberImpl other team
      pure $ maybe defUserLegalHoldStatus (view legalHoldStatus) mMember

-- | Add to every uid the legalhold status
getLHStatusForUsersImpl ::
  ( Member TeamStore r,
    Member LegalHoldStore r
  ) =>
  [UserId] ->
  Sem r [(UserId, UserLegalHoldStatus)]
getLHStatusForUsersImpl uids =
  mconcat
    <$> for
      (List.chunksOf 32 uids)
      ( \uidsChunk -> do
          teamsOfUsers <- TeamStore.getUsersTeams uidsChunk
          for uidsChunk $ \uid -> do
            (uid,) <$> getLHStatusImpl (Map.lookup uid teamsOfUsers) uid
      )
