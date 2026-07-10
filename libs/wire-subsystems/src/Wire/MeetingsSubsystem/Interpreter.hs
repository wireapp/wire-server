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

module Wire.MeetingsSubsystem.Interpreter
  ( interpretMeetingsSubsystem,
    startTimeTolerance,
    MeetingError (..),
  )
where

import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.ByteString.Conversion (toByteString')
import Data.Default (def)
import Data.Domain (Domain)
import Data.Id
import Data.Json.Util (toJSONObject)
import Data.Map qualified as Map
import Data.Qualified (Local, Qualified (..), inputQualifyLocal, qualifyAs, tDomain, tUnqualified)
import Data.Range (Range, unsafeRange)
import Data.Set qualified as Set
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime)
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input (Input)
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as TinyLog
import System.Logger qualified as Log
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Role (roleNameWireAdmin)
import Wire.API.Event.Conversation qualified as ConvEvent
import Wire.API.Meeting qualified as API
import Wire.API.Push.V2 qualified as PushV2
import Wire.API.Routes.MultiTablePaging qualified as MultiTablePaging
import Wire.API.Team.Feature (FeatureStatus (..), LockableFeature (..), MeetingsConfig)
import Wire.API.User (BaseProtocolTag (BaseProtocolMLSTag), EmailAddress)
import Wire.ConversationSubsystem (ConversationSubsystem)
import Wire.ConversationSubsystem qualified as ConversationSubsystem
import Wire.FeaturesConfigSubsystem (FeaturesConfigSubsystem, getFeatureForTeam)
import Wire.MeetingsStore qualified as Store
import Wire.MeetingsSubsystem
import Wire.NotificationSubsystem
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now
import Wire.StoredConversation
import Wire.TeamSubsystem (TeamSubsystem)
import Wire.TeamSubsystem qualified as TeamSubsystem

data MeetingError = InvalidTimes | EmptyUpdate | MeetingsFeatureDisabled
  deriving stock (Eq, Show)

-- | Tolerance applied when validating that a meeting's start time is not in
-- the past. The check always uses the server's clock ('Now.get') as the
-- reference; the client's clock is never trusted. The tolerance only absorbs
-- minor clock skew between client and server and the network/processing delay
-- between the client sending the request and the server observing it (matches
-- the 60s precedent used by SAML2).
startTimeTolerance :: NominalDiffTime
startTimeTolerance = 60

-- | Whether a meeting is still alive at the given cutoff. A meeting is alive
-- when its 'Store.effectiveEndTime' is at or after the cutoff, or 'Nothing'
-- (open-ended recurrence, which never expires).
isAlive :: UTCTime -> Store.StoredMeeting -> Bool
isAlive cutoff = maybe True (>= cutoff) . Store.effectiveEndTime

checkMeetingsEnabled ::
  ( Member FeaturesConfigSubsystem r,
    Member (Error MeetingError) r
  ) =>
  Maybe TeamId ->
  Sem r ()
checkMeetingsEnabled maybeTeamId = do
  case maybeTeamId of
    Nothing -> pure ()
    Just teamId -> do
      meetingFeature <- getFeatureForTeam @_ @MeetingsConfig teamId
      unless (meetingFeature.status == FeatureStatusEnabled) $
        throw MeetingsFeatureDisabled

interpretMeetingsSubsystem ::
  ( Member Store.MeetingsStore r,
    Member ConversationSubsystem r,
    Member TeamSubsystem r,
    Member FeaturesConfigSubsystem r,
    Member NotificationSubsystem r,
    Member Now r,
    Member TinyLog r,
    Member (Error MeetingError) r,
    Member (Input (Local ())) r
  ) =>
  NominalDiffTime ->
  InterpreterFor MeetingsSubsystem r
interpretMeetingsSubsystem validityPeriod = interpret $ \case
  CreateMeeting zUser newMeeting ->
    createMeetingImpl zUser newMeeting
  UpdateMeeting zUser meetingId update ->
    updateMeetingImpl zUser meetingId update validityPeriod
  DeleteMeeting zUser connId meetingId ->
    deleteMeetingImpl zUser connId meetingId validityPeriod
  GetMeeting zUser meetingId ->
    getMeetingImpl zUser meetingId validityPeriod
  ListMeetings zUser ->
    listMeetingsImpl zUser validityPeriod
  AddInvitedEmails zUser meetingId emails ->
    addInvitedEmailsImpl zUser meetingId emails validityPeriod
  RemoveInvitedEmails zUser meetingId emails ->
    removeInvitedEmailsImpl zUser meetingId emails validityPeriod
  ReplaceInvitedEmails zUser meetingId emails ->
    replaceInvitedEmailsImpl zUser meetingId emails validityPeriod
  CleanupOldMeetings cutoffTime batchSize ->
    cleanupOldMeetingsImpl cutoffTime batchSize

createMeetingImpl ::
  ( Member Store.MeetingsStore r,
    Member ConversationSubsystem r,
    Member TeamSubsystem r,
    Member FeaturesConfigSubsystem r,
    Member NotificationSubsystem r,
    Member Now r,
    Member (Error MeetingError) r
  ) =>
  Local UserId ->
  API.NewMeeting ->
  Sem r API.MeetingWithConversation
createMeetingImpl zUser newMeeting = do
  -- Look up user's team once and reuse for both checks
  conversationTeamId <- TeamSubsystem.internalGetOneUserTeam (tUnqualified zUser)
  checkMeetingsEnabled conversationTeamId
  -- Validate that endTime > startTime
  when (newMeeting.endTime <= newMeeting.startTime) $
    throw InvalidTimes
  -- Validate that startTime is not in the past (within tolerance)
  now <- Now.get
  when (newMeeting.startTime < addUTCTime (negate startTimeTolerance) now) $
    throw InvalidTimes

  -- Determine trial status: personal users (no team) create trial meetings.
  -- The deprecated meetingsPremium feature flag no longer affects this; team
  -- meetings are always non-trial (see WPB-26771).
  let trial = isNothing conversationTeamId

  -- Create conversation with the meeting creator as the only member (admin role)
  let newConv =
        NewConv
          { newConvUsers = [],
            newConvQualifiedUsers = [],
            newConvName = Just newMeeting.title,
            -- InviteAccess is required so MLS commits can add participants via
            -- performConversationJoin (ensureAccess conv InviteAccess).
            newConvAccess = Set.fromList [PrivateAccess, InviteAccess],
            newConvAccessRoles = Nothing,
            newConvTeam = ConvTeamInfo <$> conversationTeamId,
            newConvMessageTimer = Nothing,
            newConvReceiptMode = Nothing,
            newConvUsersRole = roleNameWireAdmin,
            newConvProtocol = BaseProtocolMLSTag,
            newConvGroupConvType = MeetingConversation,
            newConvCells = True,
            newConvChannelAddPermission = Nothing,
            newConvSkipCreator = False,
            newConvParent = Nothing,
            newConvHistory = def
          }

  -- Create and store the conversation via ConversationSubsystem
  storedConv <- ConversationSubsystem.internalCreateGroupConversation zUser Nothing newConv

  -- Store meeting (trial status is provided by caller)
  storedMeeting <-
    Store.createMeeting
      newMeeting.title
      (tUnqualified zUser)
      newMeeting.startTime
      newMeeting.endTime
      newMeeting.recurrence
      storedConv.id_
      newMeeting.invitedEmails
      trial

  let qMeetingId = Qualified storedMeeting.id (tDomain zUser)
  pushMeetingEvent zUser Nothing storedConv.localMembers (Qualified storedConv.id_ (tDomain zUser)) conversationTeamId (ConvEvent.EdMeetingCreate qMeetingId)

  pure $ storedMeetingToMeetingWithConversation zUser storedConv storedMeeting

updateMeetingImpl ::
  ( Member Store.MeetingsStore r,
    Member ConversationSubsystem r,
    Member TeamSubsystem r,
    Member FeaturesConfigSubsystem r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Error MeetingError) r,
    Member Now r
  ) =>
  Local UserId ->
  Qualified MeetingId ->
  API.UpdateMeeting ->
  NominalDiffTime ->
  Sem r (Maybe API.MeetingWithConversation)
updateMeetingImpl zUser meetingId update validityPeriod = do
  maybeTeamId <- TeamSubsystem.internalGetOneUserTeam (tUnqualified zUser)
  checkMeetingsEnabled maybeTeamId
  when (isNothing update.title && isNothing update.startTime && isNothing update.endTime && isNothing update.recurrence) $
    throw EmptyUpdate

  runMaybeT $ do
    meeting <- MaybeT $ Store.getMeeting (qUnqualified meetingId)
    now <- lift Now.get
    let cutoff = addUTCTime (negate validityPeriod) now
    guard $ isAlive cutoff meeting
    guard $ qDomain meetingId == tDomain zUser
    when (fromMaybe meeting.startTime update.startTime >= fromMaybe meeting.endTime update.endTime) $
      lift $
        throw InvalidTimes
    -- Validate that the updated start time (if provided) is not in the past
    for_ update.startTime $ \t ->
      when (t < addUTCTime (negate startTimeTolerance) now) $
        lift $
          throw InvalidTimes

    guard $ meeting.creator == tUnqualified zUser
    updatedMeeting <-
      MaybeT $
        Store.updateMeeting
          (qUnqualified meetingId)
          update.title
          update.startTime
          update.endTime
          update.recurrence
    conv <- MaybeT $ getMeetingConversationOrFail meetingId updatedMeeting.conversationId
    lift $ pushMeetingEvent zUser Nothing conv.localMembers (Qualified conv.id_ (tDomain zUser)) maybeTeamId (ConvEvent.EdMeetingUpdate meetingId)
    pure $ storedMeetingToMeetingWithConversation zUser conv updatedMeeting

deleteMeetingImpl ::
  ( Member Store.MeetingsStore r,
    Member ConversationSubsystem r,
    Member TeamSubsystem r,
    Member FeaturesConfigSubsystem r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Error MeetingError) r,
    Member Now r
  ) =>
  Local UserId ->
  ConnId ->
  Qualified MeetingId ->
  NominalDiffTime ->
  Sem r Bool
deleteMeetingImpl zUser connId meetingId validityPeriod = do
  maybeTeamId <- TeamSubsystem.internalGetOneUserTeam (tUnqualified zUser)
  checkMeetingsEnabled maybeTeamId
  result <-
    runMaybeT $ do
      meeting <- MaybeT $ Store.getMeeting (qUnqualified meetingId)
      now <- lift Now.get
      let cutoff = addUTCTime (negate validityPeriod) now
      guard $ isAlive cutoff meeting
      guard $ qDomain meetingId == tDomain zUser
      guard $ meeting.creator == tUnqualified zUser
      let convId = meeting.conversationId
          lConvId = qualifyAs zUser convId
      conv <- MaybeT $ getMeetingConversationOrFail meetingId convId
      when (conv.metadata.cnvmGroupConvType == Just MeetingConversation) $
        lift $
          void $
            ConversationSubsystem.deleteLocalConversation zUser connId lConvId
      lift $ Store.deleteMeeting (qUnqualified meetingId)
      lift $ pushMeetingEvent zUser (Just connId) conv.localMembers (Qualified conv.id_ (tDomain zUser)) maybeTeamId (ConvEvent.EdMeetingDelete meetingId)
  pure $ isJust result

getMeetingImpl ::
  ( Member Store.MeetingsStore r,
    Member ConversationSubsystem r,
    Member TeamSubsystem r,
    Member FeaturesConfigSubsystem r,
    Member (Error MeetingError) r,
    Member Now r
  ) =>
  Local UserId ->
  Qualified MeetingId ->
  NominalDiffTime ->
  Sem r (Maybe API.Meeting)
getMeetingImpl zUser meetingId validityPeriod = do
  maybeTeamId <- TeamSubsystem.internalGetOneUserTeam (tUnqualified zUser)
  checkMeetingsEnabled maybeTeamId
  -- Get meeting from store
  runMaybeT $ do
    storedMeeting <- MaybeT $ Store.getMeeting (qUnqualified meetingId)
    now <- lift Now.get
    let cutoff = addUTCTime (negate validityPeriod) now
    guard $ isAlive cutoff storedMeeting
    guard $ qDomain meetingId == tDomain zUser
    -- Check authorization: user must be creator OR member of the associated conversation
    let isCreator = storedMeeting.creator == tUnqualified zUser
    if isCreator
      then pure $ storedMeetingToMeeting (tDomain zUser) storedMeeting
      else do
        -- Check if user is a member of the conversation
        let convId = storedMeeting.conversationId
        void $ MaybeT $ ConversationSubsystem.internalGetLocalMember convId (tUnqualified zUser)
        pure $ storedMeetingToMeeting (tDomain zUser) storedMeeting -- User is a member, authorized

-- | Look up the 'StoredConversation' associated with a meeting. When the
-- conversation cannot be found (a data-integrity anomaly), a warning is logged
-- before failing: otherwise the missing conversation is indistinguishable from
-- a missing meeting for callers.
getMeetingConversationOrFail ::
  ( Member ConversationSubsystem r,
    Member TinyLog r
  ) =>
  Qualified MeetingId ->
  ConvId ->
  Sem r (Maybe StoredConversation)
getMeetingConversationOrFail meetingId convId = do
  mConv <- ConversationSubsystem.internalGetConversation convId
  case mConv of
    Just conv -> pure (Just conv)
    Nothing -> do
      TinyLog.warn $
        Log.msg ("conversation not found for meeting" :: ByteString)
          . Log.field "conversationId" (toByteString' convId)
          . Log.field "meetingId" (toByteString' (qUnqualified meetingId))
      pure Nothing

-- | Push a meeting lifecycle event to all local members of the meeting's
-- conversation via the 'NotificationSubsystem'. Meetings are not federated, so
-- only local members are notified.
pushMeetingEvent ::
  ( Member NotificationSubsystem r,
    Member Now r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  [LocalMember] ->
  Qualified ConvId ->
  Maybe TeamId ->
  ConvEvent.EventData ->
  Sem r ()
pushMeetingEvent lUser conn members qConvId mTeamId edata = do
  now <- Now.get
  let evt =
        ConvEvent.Event
          { evtConv = qConvId,
            evtSubConv = Nothing,
            evtFrom =
              ConvEvent.EventFromUser
                (Qualified (tUnqualified lUser) (tDomain lUser)),
            evtTime = now,
            evtTeam = mTeamId,
            evtData = edata
          }
  pushNotifications
    [ def
        { origin = Just (tUnqualified lUser),
          json = toJSONObject evt,
          recipients = map localMemberToRecipient members,
          route = PushV2.RouteDirect,
          conn
        }
    ]

-- Helper function to convert StoredMeeting to API.Meeting
storedMeetingToMeeting :: Domain -> Store.StoredMeeting -> API.Meeting
storedMeetingToMeeting domain sm =
  API.Meeting
    { API.id = Qualified sm.id domain,
      API.title = sm.title,
      API.creator = Qualified sm.creator domain,
      API.startTime = sm.startTime,
      API.endTime = sm.endTime,
      API.recurrence = sm.recurrence,
      API.conversationId = Qualified sm.conversationId domain,
      API.invitedEmails = sm.invitedEmails,
      API.trial = sm.trial,
      API.createdAt = sm.createdAt,
      API.updatedAt = sm.updatedAt
    }

-- | Like 'storedMeetingToMeeting', but additionally carries the full
-- 'API.Conversation' associated with the meeting.
--
-- The local user's domain ('tDomain lUser') is used to qualify the meeting,
-- its creator and its conversation: meetings are not federated, and every
-- meeting operation guards @qDomain meetingId == tDomain zUser@. The
-- conversation itself is always created locally.
storedMeetingToMeetingWithConversation ::
  Local UserId ->
  StoredConversation ->
  Store.StoredMeeting ->
  API.MeetingWithConversation
storedMeetingToMeetingWithConversation lUser conv sm =
  API.MeetingWithConversation
    { API.meeting = storedMeetingToMeeting (tDomain lUser) sm,
      API.conversation = conversationView lUser (Just lUser) conv
    }

listMeetingsImpl ::
  ( Member Store.MeetingsStore r,
    Member ConversationSubsystem r,
    Member TeamSubsystem r,
    Member FeaturesConfigSubsystem r,
    Member (Error MeetingError) r,
    Member Now r
  ) =>
  Local UserId ->
  NominalDiffTime ->
  Sem r [API.Meeting]
listMeetingsImpl zUser validityPeriod = do
  maybeTeamId <- TeamSubsystem.internalGetOneUserTeam (tUnqualified zUser)
  checkMeetingsEnabled maybeTeamId
  now <- Now.get
  let cutoff = addUTCTime (negate validityPeriod) now
  -- List all meetings created by the user
  createdMeetings <- Store.listMeetingsByUser (tUnqualified zUser) cutoff
  -- Loop over local conversations accessible by the user, then filter to only keep meetings.
  memberMeetings <- getAllMemberMeetings zUser cutoff
  -- Combine and deduplicate
  let allMeetings = map (storedMeetingToMeeting (tDomain zUser)) createdMeetings <> memberMeetings
      uniqueMeetings = Map.elems $ Map.fromList [(m.id, m) | m <- allMeetings]
  pure uniqueMeetings

getAllMemberMeetings ::
  ( Member Store.MeetingsStore r,
    Member ConversationSubsystem r
  ) =>
  Local UserId ->
  UTCTime ->
  Sem r [API.Meeting]
getAllMemberMeetings zUser cutoff = do
  -- We process conversations in pages
  processPage Nothing
  where
    processPage ::
      ( Member Store.MeetingsStore r,
        Member ConversationSubsystem r
      ) =>
      Maybe ConversationPagingState -> Sem r [API.Meeting]
    processPage pagingState = do
      let range = unsafeRange 1000 :: Range 1 1000 Int32
      page <- ConversationSubsystem.getConversationIds zUser range pagingState
      case page of
        MultiTablePaging.MultiTablePage uConvIds hasMore _ ->
          if null uConvIds
            then pure []
            else do
              convs <- ConversationSubsystem.getConversations (map qUnqualified uConvIds)
              let meetingConvs = filter isMeetingConv convs
                  meetingConvIds = Set.fromList $ map (.id_) meetingConvs
              -- Identify which Qualified ConvIds correspond to meeting conversations
              -- We use the original Qualified IDs to query the meeting store
              let targetQConvIds = filter (\qId -> qUnqualified qId `Set.member` meetingConvIds) uConvIds
              -- Fetch meetings for these conversations
              pageMeetings <- forM targetQConvIds $ \qConvId -> do
                Store.listMeetingsByConversation (qUnqualified qConvId) cutoff
              let currentMeetings = storedMeetingToMeeting (tDomain zUser) <$> concat pageMeetings
              -- Check if there are more pages
              if hasMore
                then do
                  -- Recurse with paging state from the page
                  let nextPageState = Just page.mtpPagingState
                  rest <- processPage nextPageState
                  pure (currentMeetings <> rest)
                else pure currentMeetings
    isMeetingConv :: StoredConversation -> Bool
    isMeetingConv conv = conv.metadata.cnvmGroupConvType == Just MeetingConversation

addInvitedEmailsImpl ::
  ( Member Store.MeetingsStore r,
    Member TeamSubsystem r,
    Member FeaturesConfigSubsystem r,
    Member (Error MeetingError) r,
    Member Now r
  ) =>
  Local UserId ->
  Qualified MeetingId ->
  [EmailAddress] ->
  NominalDiffTime ->
  Sem r Bool
addInvitedEmailsImpl zUser meetingId emails validityPeriod = do
  maybeTeamId <- TeamSubsystem.internalGetOneUserTeam (tUnqualified zUser)
  checkMeetingsEnabled maybeTeamId
  result <-
    runMaybeT $ do
      storedMeeting <- MaybeT $ Store.getMeeting (qUnqualified meetingId)
      now <- lift Now.get
      let cutoff = addUTCTime (negate validityPeriod) now
      guard $ isAlive cutoff storedMeeting
      guard $ storedMeeting.creator == tUnqualified zUser
      guard $ qDomain meetingId == tDomain zUser
      lift $ Store.addInvitedEmails (qUnqualified meetingId) emails

  pure $ isJust result

removeInvitedEmailsImpl ::
  ( Member Store.MeetingsStore r,
    Member TeamSubsystem r,
    Member FeaturesConfigSubsystem r,
    Member (Error MeetingError) r,
    Member Now r
  ) =>
  Local UserId ->
  Qualified MeetingId ->
  [EmailAddress] ->
  NominalDiffTime ->
  Sem r Bool
removeInvitedEmailsImpl zUser meetingId emails validityPeriod = do
  maybeTeamId <- TeamSubsystem.internalGetOneUserTeam (tUnqualified zUser)
  checkMeetingsEnabled maybeTeamId
  result <-
    runMaybeT $ do
      storedMeeting <- MaybeT $ Store.getMeeting (qUnqualified meetingId)
      now <- lift Now.get
      let cutoff = addUTCTime (negate validityPeriod) now
      guard $ isAlive cutoff storedMeeting
      guard $ storedMeeting.creator == tUnqualified zUser
      guard $ qDomain meetingId == tDomain zUser
      lift $ Store.removeInvitedEmails (qUnqualified meetingId) emails

  pure $ isJust result

replaceInvitedEmailsImpl ::
  ( Member Store.MeetingsStore r,
    Member TeamSubsystem r,
    Member FeaturesConfigSubsystem r,
    Member (Error MeetingError) r,
    Member Now r
  ) =>
  Local UserId ->
  Qualified MeetingId ->
  [EmailAddress] ->
  NominalDiffTime ->
  Sem r Bool
replaceInvitedEmailsImpl zUser meetingId emails validityPeriod = do
  maybeTeamId <- TeamSubsystem.internalGetOneUserTeam (tUnqualified zUser)
  checkMeetingsEnabled maybeTeamId
  result <-
    runMaybeT $ do
      storedMeeting <- MaybeT $ Store.getMeeting (qUnqualified meetingId)
      now <- lift Now.get
      let cutoff = addUTCTime (negate validityPeriod) now
      guard $ isAlive cutoff storedMeeting
      guard $ storedMeeting.creator == tUnqualified zUser
      guard $ qDomain meetingId == tDomain zUser
      lift $ Store.replaceInvitedEmails (qUnqualified meetingId) emails

  pure $ isJust result

cleanupOldMeetingsImpl ::
  ( Member Store.MeetingsStore r,
    Member ConversationSubsystem r,
    Member (Input (Local ())) r
  ) =>
  UTCTime ->
  Int ->
  Sem r Int64
cleanupOldMeetingsImpl cutoffTime batchSize = do
  oldMeetings <- Store.getOldMeetings cutoffTime batchSize
  if null oldMeetings
    then pure 0
    else do
      for_ oldMeetings forceDeleteMeeting
      pure $ fromIntegral $ length oldMeetings

forceDeleteMeeting ::
  ( Member Store.MeetingsStore r,
    Member ConversationSubsystem r,
    Member (Input (Local ())) r
  ) =>
  Store.StoredMeeting ->
  Sem r ()
forceDeleteMeeting meeting = do
  maybeConv <- ConversationSubsystem.internalGetConversation meeting.conversationId
  case maybeConv of
    Just conv
      | conv.metadata.cnvmGroupConvType == Just MeetingConversation,
        conv.id_ == meeting.conversationId ->
          ConversationSubsystem.internalDeleteLocalConversation =<< inputQualifyLocal meeting.conversationId
    _ -> pure ()
  Store.deleteMeeting meeting.id
