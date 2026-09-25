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
  ( MeetingSystemConfig (..),
    interpretMeetingsSubsystem,
    startTimeTolerance,
  )
where

import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.ByteString.Conversion (toByteString')
import Data.Code (Timeout (..))
import Data.Default (def)
import Data.Domain (Domain, domainText)
import Data.Id
import Data.Map qualified as Map
import Data.Misc (HttpsUrl, httpsUrlFromText)
import Data.Qualified (Local, Qualified (..), inputQualifyLocal, qualifyAs, tDomain, tUnqualified)
import Data.Range (Range, unsafeRange)
import Data.Set qualified as Set
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime)
import Data.UUID (nil)
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input (Input)
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as TinyLog
import System.Logger qualified as Log
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Role (roleNameWireAdmin)
import Wire.API.Error.Galley (InvalidTimesReason (..), MeetingError (..))
import Wire.API.Event.Meeting qualified as MeetingEvent
import Wire.API.Meeting qualified as API
import Wire.API.Routes.MultiTablePaging qualified as MultiTablePaging
import Wire.API.Team.Feature (FeatureStatus (..), LockableFeature (..), MeetingsConfig)
import Wire.API.User (BaseProtocolTag (BaseProtocolMLSTag), EmailAddress)
import Wire.CodeStore (CodeStore)
import Wire.CodeStore qualified as CodeStore
import Wire.ConversationSubsystem (ConversationSubsystem)
import Wire.ConversationSubsystem qualified as ConversationSubsystem
import Wire.FeaturesConfigSubsystem (FeaturesConfigSubsystem, getFeatureForTeam)
import Wire.MeetingNotifier (MeetingNotifier, notifyMeetingEvent)
import Wire.MeetingsStore qualified as Store
import Wire.MeetingsSubsystem
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now
import Wire.Sem.Random qualified as Random
import Wire.StoredConversation
import Wire.TeamSubsystem (TeamSubsystem)
import Wire.TeamSubsystem qualified as TeamSubsystem

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

-- | Meeting join codes never expire while the meeting lives, and
-- open-ended recurrences never expire at all, so use the largest TTL
-- expressible in the code store's int32-seconds schema (~68 years)
-- instead of a renewal mechanism. Known trade-off: if the meeting-row
-- insert fails after the code was created, the orphaned code row is
-- unreachable forever (its key derives from the unpersisted id) and
-- will live out this TTL.
meetingCodeTimeout :: Timeout
meetingCodeTimeout = Timeout (fromIntegral (maxBound @Int32))

-- | Resolve the join-link base for a domain. 'Nothing' from the code store
-- only happens in multi-domain mode when the user's domain has no configured
-- URI (a misconfiguration); degrade to an https URL derived from the user's
-- own domain so the nil-uuid placeholder stays recognizable.
codeURIBase :: (Member CodeStore r) => Domain -> Sem r HttpsUrl
codeURIBase dom =
  fromMaybe (domainFallback dom) <$> CodeStore.getConversationCodeURI (Just dom)

domainFallback :: Domain -> HttpsUrl
domainFallback dom =
  -- A 'Domain' is a validated host name, so this URI always parses.
  fromRight' (httpsUrlFromText ("https://" <> domainText dom <> "/"))

checkMeetingsEnabled maybeTeamId =
  unlessM (meetingsFeatureEnabled maybeTeamId) $
    throw MeetingsFeatureDisabled

-- | Like 'checkMeetingsEnabled' but returns the resolved status instead of
-- throwing. Used by read paths (list, get) that treat a disabled feature as
-- "no meetings" rather than as a forbidden operation.
meetingsFeatureEnabled ::
  (Member FeaturesConfigSubsystem r) =>
  Maybe TeamId ->
  Sem r Bool
meetingsFeatureEnabled maybeTeamId =
  case maybeTeamId of
    Nothing -> pure True
    Just teamId -> do
      meetingFeature <- getFeatureForTeam @_ @MeetingsConfig teamId
      pure (meetingFeature.status == FeatureStatusEnabled)

-- | System-wide meeting configuration: the legacy time zone used for V16
-- meetings, how long meetings stay alive after their effective end time, and
-- how far into the past an update may move a meeting's times. Invariant
-- (checked by callers at startup where configurable): @pastEditPeriod <=
-- validityPeriod@, so an edited meeting stays within the validity window.
data MeetingSystemConfig = MeetingSystemConfig
  { legacyTimeZone :: !API.TimeZone,
    validityPeriod :: !NominalDiffTime,
    pastEditPeriod :: !NominalDiffTime
  }

interpretMeetingsSubsystem ::
  ( Member Store.MeetingsStore r,
    Member ConversationSubsystem r,
    Member TeamSubsystem r,
    Member FeaturesConfigSubsystem r,
    Member MeetingNotifier r,
    Member Now r,
    Member TinyLog r,
    Member (Error MeetingError) r,
    Member (Input (Local ())) r,
    Member CodeStore r,
    Member Random.Random r
  ) =>
  -- | System-wide meeting configuration.
  MeetingSystemConfig ->
  InterpreterFor MeetingsSubsystem r
interpretMeetingsSubsystem cfg = interpret $ \case
  CreateMeeting zUser connId newMeeting ->
    createMeetingImpl zUser connId newMeeting
  UpdateMeeting zUser connId meetingId update ->
    updateMeetingImpl zUser connId meetingId update cfg.validityPeriod cfg.pastEditPeriod
  DeleteMeeting zUser connId meetingId ->
    deleteMeetingImpl zUser connId meetingId cfg.validityPeriod
  GetMeeting zUser meetingId ->
    getMeetingImpl zUser meetingId cfg.validityPeriod
  ListMeetings zUser ->
    listMeetingsImpl zUser cfg.validityPeriod
  CreateMeetingV16 zUser connId newMeeting ->
    API.toLegacyWithConv <$> createMeetingImpl zUser connId (API.fromLegacyNewMeeting cfg.legacyTimeZone newMeeting)
  UpdateMeetingV16 zUser connId meetingId update ->
    updateMeetingV16Impl zUser connId meetingId update cfg.validityPeriod cfg.pastEditPeriod
  GetMeetingV16 zUser meetingId ->
    fmap API.toLegacy <$> getMeetingImpl zUser meetingId cfg.validityPeriod
  ListMeetingsV16 zUser ->
    map API.toLegacy <$> listMeetingsImpl zUser cfg.validityPeriod
  CreateMeetingV18 zUser connId nm ->
    API.toLegacyWithConvV18 <$> createMeetingImpl zUser connId (API.fromLegacyNewMeetingV18 nm)
  UpdateMeetingV18 zUser connId meetingId update ->
    updateMeetingV18Impl zUser connId meetingId update cfg.validityPeriod cfg.pastEditPeriod
  GetMeetingV18 zUser meetingId ->
    fmap API.toLegacyV18 <$> getMeetingImpl zUser meetingId cfg.validityPeriod
  ListMeetingsV18 zUser ->
    map API.toLegacyV18 <$> listMeetingsImpl zUser cfg.validityPeriod
  AddInvitedEmails zUser meetingId emails ->
    addInvitedEmailsImpl zUser meetingId emails cfg.validityPeriod
  RemoveInvitedEmails zUser meetingId emails ->
    removeInvitedEmailsImpl zUser meetingId emails cfg.validityPeriod
  ReplaceInvitedEmails zUser meetingId emails ->
    replaceInvitedEmailsImpl zUser meetingId emails cfg.validityPeriod
  CleanupOldMeetings cutoffTime batchSize ->
    cleanupOldMeetingsImpl cutoffTime batchSize

createMeetingImpl ::
  ( Member Store.MeetingsStore r,
    Member ConversationSubsystem r,
    Member TeamSubsystem r,
    Member FeaturesConfigSubsystem r,
    Member MeetingNotifier r,
    Member Now r,
    Member TinyLog r,
    Member (Error MeetingError) r,
    Member CodeStore r,
    Member Random.Random r
  ) =>
  Local UserId ->
  ConnId ->
  API.NewMeeting ->
  Sem r API.MeetingWithConversation
createMeetingImpl zUser connId newMeeting = do
  -- Look up user's team once and reuse for both checks
  conversationTeamId <- TeamSubsystem.internalGetOneUserTeam (tUnqualified zUser)
  checkMeetingsEnabled conversationTeamId
  -- Validate that the meeting ends after it starts (end_time is the source of
  -- truth; positivity was previously checked on the derived duration).
  when (newMeeting.endTime <= newMeeting.startTime) $
    throw (InvalidTimes EndBeforeStart)
  -- Validate that startTime is not in the past (within tolerance)
  now <- Now.get
  when (newMeeting.startTime < addUTCTime (negate startTimeTolerance) now) $
    throw (InvalidTimes StartTimeTooFarInPast)
  mid <- Random.newId

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
            -- CodeAccess allows joining the meeting conversation by code.
            newConvAccess = Set.fromList [InviteAccess, CodeAccess],
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

  -- Create the join code BEFORE the meeting row: a row without a code would
  -- serve a dead link, while an unreferenced code is harmless. Code-store
  -- modes that cannot hold meeting codes (Cassandra-only) return False;
  -- degrade to the placeholder link instead of failing the request.
  hasJoinCode <- CodeStore.createMeetingCode mid meetingCodeTimeout
  unless hasJoinCode $
    TinyLog.warn $
      Log.msg ("meeting created without join link" :: ByteString)
        . Log.field "meetingId" (toByteString' mid)
  -- Store meeting (trial status provided by caller)
  storedMeeting <-
    Store.createMeeting
      mid
      newMeeting.title
      (tUnqualified zUser)
      newMeeting.startTime
      newMeeting.endTime
      newMeeting.tzid
      newMeeting.mtype
      newMeeting.recurrence
      storedConv.id_
      newMeeting.invitedEmails
      trial

  let qMeetingId = Qualified storedMeeting.id (tDomain zUser)
  notifyMeetingEvent zUser (Just connId) storedConv.localMembers (Qualified storedConv.id_ (tDomain zUser)) conversationTeamId MeetingEvent.Create qMeetingId

  base <- codeURIBase (tDomain zUser)
  storedMeetingToMeetingWithConversation base zUser storedConv storedMeeting

updateMeetingImpl ::
  ( Member Store.MeetingsStore r,
    Member ConversationSubsystem r,
    Member TeamSubsystem r,
    Member FeaturesConfigSubsystem r,
    Member MeetingNotifier r,
    Member TinyLog r,
    Member (Error MeetingError) r,
    Member Now r,
    Member CodeStore r
  ) =>
  Local UserId ->
  ConnId ->
  Qualified MeetingId ->
  API.UpdateMeeting ->
  NominalDiffTime ->
  NominalDiffTime ->
  Sem r (Maybe API.MeetingWithConversation)
updateMeetingImpl zUser connId meetingId update validityPeriod pastEditPeriod = do
  maybeTeamId <- TeamSubsystem.internalGetOneUserTeam (tUnqualified zUser)
  checkMeetingsEnabled maybeTeamId
  when (isNothing update.title && isNothing update.startTime && isNothing update.endTime && isNothing update.recurrence && isNothing update.tzid && isNothing update.mtype) $
    throw EmptyUpdate
  base <- codeURIBase (tDomain zUser)

  runMaybeT $ do
    meeting <- MaybeT $ Store.getMeeting (qUnqualified meetingId)
    now <- lift Now.get
    let cutoff = addUTCTime (negate validityPeriod) now
    guard $ isAlive cutoff meeting
    guard $ qDomain meetingId == tDomain zUser
    guard $ meeting.creator == tUnqualified zUser
    -- Creation enforces endTime > startTime (createMeetingImpl); re-establish
    -- the invariant on the *effective* times after this update, since either
    -- bound may change independently.
    when (fromMaybe meeting.startTime update.startTime >= fromMaybe meeting.endTime update.endTime) $
      lift $
        throw (InvalidTimes EndBeforeStart)
    -- New time values may be moved into the past so that past/ongoing
    -- meetings can be corrected to what actually happened, but no further
    -- than `pastEditPeriod` (WPB-28080). Only provided values are checked;
    -- unchanged stored times are not re-validated.
    let pastEditCutoff = addUTCTime (negate pastEditPeriod) now
    for_ update.startTime $ \t -> when (t < pastEditCutoff) $ lift $ throw (InvalidTimes TimesBeyondPastEditWindow)
    for_ update.endTime $ \t -> when (t < pastEditCutoff) $ lift $ throw (InvalidTimes TimesBeyondPastEditWindow)

    updatedMeeting <-
      MaybeT $
        Store.updateMeeting
          (qUnqualified meetingId)
          update.title
          update.startTime
          update.endTime
          update.tzid
          update.mtype
          update.recurrence
    conv <- MaybeT $ getMeetingConversationOrFail meetingId updatedMeeting.conversationId
    lift $ notifyMeetingEvent zUser (Just connId) conv.localMembers (Qualified conv.id_ (tDomain zUser)) maybeTeamId MeetingEvent.Update meetingId
    lift $ storedMeetingToMeetingWithConversation base zUser conv updatedMeeting

-- | V16 update path: 'API.UpdateMeetingV16' carries no @type@ field, so
-- legacy clients cannot change the stored meeting type. The request is mapped
-- onto 'API.UpdateMeeting' with @mtype = Nothing@ and delegated to the shared
-- update implementation; the result is re-shaped to the legacy form.
updateMeetingV16Impl ::
  ( Member Store.MeetingsStore r,
    Member ConversationSubsystem r,
    Member TeamSubsystem r,
    Member FeaturesConfigSubsystem r,
    Member MeetingNotifier r,
    Member TinyLog r,
    Member (Error MeetingError) r,
    Member Now r,
    Member CodeStore r
  ) =>
  Local UserId ->
  ConnId ->
  Qualified MeetingId ->
  API.UpdateMeetingV16 ->
  NominalDiffTime ->
  NominalDiffTime ->
  Sem r (Maybe API.MeetingWithConversationV16)
updateMeetingV16Impl zUser connId meetingId updateL validityPeriod pastEditPeriod =
  fmap API.toLegacyWithConv
    <$> updateMeetingImpl zUser connId meetingId (API.legacyUpdateToMeeting updateL) validityPeriod pastEditPeriod

-- | V18 update path: 'API.UpdateMeetingV18' carries no @type@ field, so
-- legacy clients cannot change the stored meeting type. The request is mapped
-- onto 'API.UpdateMeeting' with @mtype = Nothing@ and delegated to the shared
-- update implementation; the result is re-shaped to the legacy form.
updateMeetingV18Impl ::
  ( Member Store.MeetingsStore r,
    Member ConversationSubsystem r,
    Member TeamSubsystem r,
    Member FeaturesConfigSubsystem r,
    Member MeetingNotifier r,
    Member TinyLog r,
    Member (Error MeetingError) r,
    Member Now r,
    Member CodeStore r
  ) =>
  Local UserId ->
  ConnId ->
  Qualified MeetingId ->
  API.UpdateMeetingV18 ->
  NominalDiffTime ->
  NominalDiffTime ->
  Sem r (Maybe API.MeetingWithConversationV18)
updateMeetingV18Impl zUser connId meetingId updateL validityPeriod pastEditPeriod =
  fmap API.toLegacyWithConvV18
    <$> updateMeetingImpl zUser connId meetingId (API.legacyUpdateToMeeting updateL) validityPeriod pastEditPeriod

deleteMeetingImpl ::
  ( Member Store.MeetingsStore r,
    Member ConversationSubsystem r,
    Member TeamSubsystem r,
    Member FeaturesConfigSubsystem r,
    Member MeetingNotifier r,
    Member TinyLog r,
    Member (Error MeetingError) r,
    Member Now r,
    Member CodeStore r
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
      lift $ CodeStore.deleteMeetingCode (qUnqualified meetingId)
      lift $ Store.deleteMeeting (qUnqualified meetingId)
      lift $ notifyMeetingEvent zUser (Just connId) conv.localMembers (Qualified conv.id_ (tDomain zUser)) maybeTeamId MeetingEvent.Delete meetingId
  pure $ isJust result

getMeetingImpl ::
  ( Member Store.MeetingsStore r,
    Member ConversationSubsystem r,
    Member TeamSubsystem r,
    Member FeaturesConfigSubsystem r,
    Member Now r,
    Member CodeStore r
  ) =>
  Local UserId ->
  Qualified MeetingId ->
  NominalDiffTime ->
  Sem r (Maybe API.Meeting)
getMeetingImpl zUser meetingId validityPeriod = do
  maybeTeamId <- TeamSubsystem.internalGetOneUserTeam (tUnqualified zUser)
  enabled <- meetingsFeatureEnabled maybeTeamId
  base <- codeURIBase (tDomain zUser)
  if enabled
    then runMaybeT $ do
      storedMeeting <- MaybeT $ Store.getMeeting (qUnqualified meetingId)
      now <- lift Now.get
      let cutoff = addUTCTime (negate validityPeriod) now
      guard $ isAlive cutoff storedMeeting
      guard $ qDomain meetingId == tDomain zUser
      -- Check authorization: user must be creator OR member of the associated conversation
      let isCreator = storedMeeting.creator == tUnqualified zUser
      if isCreator
        then lift $ storedMeetingToMeeting base (tDomain zUser) storedMeeting
        else do
          -- Check if user is a member of the conversation
          let convId = storedMeeting.conversationId
          void $ MaybeT $ ConversationSubsystem.internalGetLocalMember convId (tUnqualified zUser)
          lift $ storedMeetingToMeeting base (tDomain zUser) storedMeeting -- User is a member, authorized
    else pure Nothing

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

-- | Convert a 'Store.StoredMeeting' to an 'API.Meeting'. Meetings without a
-- join-code row in the code store serve the nil-uuid placeholder link;
-- code presence is derived at read time.
storedMeetingToMeeting ::
  (Member CodeStore r) =>
  HttpsUrl ->
  Domain ->
  Store.StoredMeeting ->
  Sem r API.Meeting
storedMeetingToMeeting base domain sm = do
  hasCode <- isJust <$> CodeStore.getMeetingCode sm.id
  pure $
    API.Meeting
      { API.id = Qualified sm.id domain,
        API.title = sm.title,
        API.creator = Qualified sm.creator domain,
        API.startTime = sm.startTime,
        API.endTime = sm.endTime,
        API.tzid = sm.tzid,
        API.mtype = sm.meetingType,
        API.recurrence = sm.recurrence,
        API.conversationId = Qualified sm.conversationId domain,
        API.invitedEmails = sm.invitedEmails,
        API.createdAt = sm.createdAt,
        API.updatedAt = sm.updatedAt,
        API.link = API.mkMeetingLink base (if hasCode then sm.id else Id nil)
      }

-- | Like 'storedMeetingToMeeting', but additionally carries the full
-- 'API.Conversation' associated with the meeting.
--
-- The local user's domain ('tDomain lUser') is used to qualify the meeting,
-- its creator and its conversation: meetings are not federated, and every
-- meeting operation guards @qDomain meetingId == tDomain zUser@. The
-- conversation itself is always created locally.
storedMeetingToMeetingWithConversation ::
  (Member CodeStore r) =>
  HttpsUrl ->
  Local UserId ->
  StoredConversation ->
  Store.StoredMeeting ->
  Sem r API.MeetingWithConversation
storedMeetingToMeetingWithConversation base lUser conv sm = do
  meeting <- storedMeetingToMeeting base (tDomain lUser) sm
  pure $
    API.MeetingWithConversation
      { API.meeting = meeting,
        API.conversation = conversationView lUser (Just lUser) conv
      }

listMeetingsImpl ::
  ( Member Store.MeetingsStore r,
    Member ConversationSubsystem r,
    Member TeamSubsystem r,
    Member FeaturesConfigSubsystem r,
    Member Now r,
    Member CodeStore r
  ) =>
  Local UserId ->
  NominalDiffTime ->
  Sem r [API.Meeting]
listMeetingsImpl zUser validityPeriod = do
  maybeTeamId <- TeamSubsystem.internalGetOneUserTeam (tUnqualified zUser)
  enabled <- meetingsFeatureEnabled maybeTeamId
  base <- codeURIBase (tDomain zUser)
  if enabled
    then do
      now <- Now.get
      let cutoff = addUTCTime (negate validityPeriod) now
      -- List all meetings created by the user
      createdMeetings <- Store.listMeetingsByUser (tUnqualified zUser) cutoff
      -- Loop over local conversations accessible by the user, then filter to only keep meetings.
      memberMeetings <- getAllMemberMeetings zUser base cutoff
      -- Combine and deduplicate
      allMeetings <-
        (<> memberMeetings)
          <$> traverse (storedMeetingToMeeting base (tDomain zUser)) createdMeetings
      let uniqueMeetings = Map.elems $ Map.fromList [(m.id, m) | m <- allMeetings]
      pure uniqueMeetings
    else pure []

getAllMemberMeetings ::
  ( Member Store.MeetingsStore r,
    Member ConversationSubsystem r,
    Member CodeStore r
  ) =>
  Local UserId ->
  HttpsUrl ->
  UTCTime ->
  Sem r [API.Meeting]
getAllMemberMeetings zUser base cutoff = do
  -- We process conversations in pages
  processPage Nothing
  where
    processPage ::
      ( Member Store.MeetingsStore r,
        Member ConversationSubsystem r,
        Member CodeStore r
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
              currentMeetings <- traverse (storedMeetingToMeeting base (tDomain zUser)) (concat pageMeetings)
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
    Member (Input (Local ())) r,
    Member CodeStore r
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
    Member (Input (Local ())) r,
    Member CodeStore r
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
  CodeStore.deleteMeetingCode meeting.id
  Store.deleteMeeting meeting.id
