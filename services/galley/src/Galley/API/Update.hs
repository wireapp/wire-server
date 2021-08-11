-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.API.Update
  ( -- * Managing Conversations
    acceptConvH,
    blockConvH,
    unblockConvH,
    checkReusableCodeH,
    joinConversationByIdH,
    joinConversationByReusableCodeH,
    addCodeH,
    rmCodeH,
    getCodeH,
    updateConversationDeprecatedH,
    updateConversationNameH,
    updateConversationAccessH,
    updateConversationReceiptModeH,
    updateConversationMessageTimerH,

    -- * Managing Members
    addMembersH,
    addMembers,
    updateSelfMemberH,
    updateOtherMemberH,
    removeMember,
    removeMemberQualified,
    removeMemberUnqualified,

    -- * Talking
    postProteusMessage,
    postOtrMessageUnqualified,
    postOtrBroadcastH,
    postProtoOtrBroadcastH,
    isTypingH,
    postRemoteToLocal,

    -- * External Services
    addServiceH,
    rmServiceH,
    Galley.API.Update.addBotH,
    rmBotH,
    postBotMessageH,
  )
where

import qualified Brig.Types.User as User
import Control.Lens
import Control.Monad.Catch
import Control.Monad.State
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.ByteString.Conversion (toByteString')
import Data.Code
import Data.Id
import Data.Json.Util (fromBase64TextLenient, toUTCTimeMillis)
import Data.List.Extra (nubOrd)
import Data.List1
import qualified Data.Map.Strict as Map
import Data.Misc (FutureWork (FutureWork))
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
import Data.Tagged
import Data.Time
import Galley.API.Error
import Galley.API.LegalHold.Conflicts (guardLegalholdPolicyConflicts)
import Galley.API.Mapping
import Galley.API.Message
import Galley.API.Util
import Galley.App
import qualified Galley.Data as Data
import Galley.Data.Services as Data
import Galley.Data.Types hiding (Conversation)
import qualified Galley.External as External
import qualified Galley.Intra.Client as Intra
import Galley.Intra.Push
import Galley.Intra.User (deleteBot, getContactList, lookupActivatedUsers)
import Galley.Options
import Galley.Types
import Galley.Types.Bot hiding (addBot)
import Galley.Types.Clients (Clients)
import qualified Galley.Types.Clients as Clients
import Galley.Types.Conversations.Members (RemoteMember (rmId))
import Galley.Types.Conversations.Roles (Action (..), RoleName, roleNameWireMember)
import Galley.Types.Teams hiding (Event, EventData (..), EventType (..), self)
import Galley.Validation
import Gundeck.Types.Push.V2 (RecipientClients (..))
import Imports
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (and, failure, setStatus, _1, _2)
import Network.Wai.Utilities
import qualified System.Logger.Class as Log
import Wire.API.Conversation (InviteQualified (invQRoleName))
import qualified Wire.API.Conversation as Public
import qualified Wire.API.Conversation.Code as Public
import Wire.API.Conversation.Role (roleNameWireAdmin)
import Wire.API.ErrorDescription
  ( codeNotFound,
    convNotFound,
    missingLegalholdConsent,
    unknownClient,
  )
import qualified Wire.API.ErrorDescription as Public
import qualified Wire.API.Event.Conversation as Public
import Wire.API.Federation.API.Galley (RemoteMessage (..))
import qualified Wire.API.Message as Public
import Wire.API.Routes.Public.Galley (UpdateResult (..))
import Wire.API.Routes.Public.Galley.Responses
import Wire.API.ServantProto (RawProto (..))
import Wire.API.Team.LegalHold (LegalholdProtectee (..))
import Wire.API.User.Client

acceptConvH :: UserId ::: Maybe ConnId ::: ConvId -> Galley Response
acceptConvH (usr ::: conn ::: cnv) =
  setStatus status200 . json <$> acceptConv usr conn cnv

acceptConv :: UserId -> Maybe ConnId -> ConvId -> Galley Conversation
acceptConv usr conn cnv = do
  conv <- Data.conversation cnv >>= ifNothing (errorDescriptionToWai convNotFound)
  conv' <- acceptOne2One usr conv conn
  conversationView usr conv'

blockConvH :: UserId ::: ConvId -> Galley Response
blockConvH (zusr ::: cnv) =
  empty <$ blockConv zusr cnv

blockConv :: UserId -> ConvId -> Galley ()
blockConv zusr cnv = do
  conv <- Data.conversation cnv >>= ifNothing (errorDescriptionToWai convNotFound)
  unless (Data.convType conv `elem` [ConnectConv, One2OneConv]) $
    throwM $
      invalidOp "block: invalid conversation type"
  let mems = Data.convLocalMembers conv
  when (zusr `isMember` mems) $ Data.removeMember zusr cnv

unblockConvH :: UserId ::: Maybe ConnId ::: ConvId -> Galley Response
unblockConvH (usr ::: conn ::: cnv) =
  setStatus status200 . json <$> unblockConv usr conn cnv

unblockConv :: UserId -> Maybe ConnId -> ConvId -> Galley Conversation
unblockConv usr conn cnv = do
  conv <- Data.conversation cnv >>= ifNothing (errorDescriptionToWai convNotFound)
  unless (Data.convType conv `elem` [ConnectConv, One2OneConv]) $
    throwM $
      invalidOp "unblock: invalid conversation type"
  conv' <- acceptOne2One usr conv conn
  conversationView usr conv'

-- conversation updates

handleUpdateResult :: UpdateResult -> Response
handleUpdateResult = \case
  Updated ev -> json ev & setStatus status200
  Unchanged -> empty & setStatus status204

updateConversationAccessH :: UserId ::: ConnId ::: ConvId ::: JsonRequest Public.ConversationAccessUpdate -> Galley Response
updateConversationAccessH (usr ::: zcon ::: cnv ::: req) = do
  update <- fromJsonBody req
  handleUpdateResult <$> updateConversationAccess usr zcon cnv update

updateConversationAccess :: UserId -> ConnId -> ConvId -> Public.ConversationAccessUpdate -> Galley UpdateResult
updateConversationAccess usr zcon cnv update = do
  let targetAccess = Set.fromList (toList (cupAccess update))
      targetRole = cupAccessRole update
  -- 'PrivateAccessRole' is for self-conversations, 1:1 conversations and
  -- so on; users are not supposed to be able to make other conversations
  -- have 'PrivateAccessRole'
  when (PrivateAccess `elem` targetAccess || PrivateAccessRole == targetRole) $
    throwM invalidTargetAccess
  -- The user who initiated access change has to be a conversation member
  (bots, users) <- localBotsAndUsers <$> Data.members cnv
  ensureConvMember users usr
  conv <- Data.conversation cnv >>= ifNothing (errorDescriptionToWai convNotFound)
  -- The conversation has to be a group conversation
  ensureGroupConvThrowing conv
  self <- getSelfMember usr users
  ensureActionAllowedThrowing ModifyConversationAccess self
  -- Team conversations incur another round of checks
  case Data.convTeam conv of
    Just tid -> checkTeamConv tid self
    Nothing -> when (targetRole == TeamAccessRole) $ throwM invalidTargetAccess
  -- When there is no update to be done, we return 204; otherwise we go
  -- with 'uncheckedUpdateConversationAccess', which will potentially kick
  -- out some users and do DB updates.
  let currentAccess = Set.fromList (toList $ Data.convAccess conv)
      currentRole = Data.convAccessRole conv
  if currentAccess == targetAccess && currentRole == targetRole
    then pure Unchanged
    else
      Updated
        <$> uncheckedUpdateConversationAccess
          update
          usr
          zcon
          conv
          (currentAccess, targetAccess)
          (currentRole, targetRole)
          users
          bots
  where
    checkTeamConv tid self = do
      -- Access mode change for managed conversation is not allowed
      tcv <- Data.teamConversation tid cnv
      when (maybe False (view managedConversation) tcv) $
        throwM invalidManagedConvOp
      -- Access mode change might result in members being removed from the
      -- conversation, so the user must have the necessary permission flag
      ensureActionAllowedThrowing RemoveConversationMember self

uncheckedUpdateConversationAccess ::
  ConversationAccessUpdate ->
  UserId ->
  ConnId ->
  Data.Conversation ->
  (Set Access, Set Access) ->
  (AccessRole, AccessRole) ->
  [LocalMember] ->
  [BotMember] ->
  Galley Event
uncheckedUpdateConversationAccess body usr zcon conv (currentAccess, targetAccess) (currentRole, targetRole) users bots = do
  localDomain <- viewFederationDomain
  let cnv = convId conv
      qcnv = Qualified cnv localDomain
      qusr = Qualified usr localDomain
  -- Remove conversation codes if CodeAccess is revoked
  when (CodeAccess `elem` currentAccess && CodeAccess `notElem` targetAccess) $ do
    key <- mkKey cnv
    Data.deleteCode key ReusableCode
  -- Depending on a variety of things, some bots and users have to be
  -- removed from the conversation. We keep track of them using 'State'.
  (newUsers, newBots) <- flip execStateT (users, bots) $ do
    -- We might have to remove non-activated members
    -- TODO(akshay): Remove Ord instance for AccessRole. It is dangerous
    -- to make assumption about the order of roles and implement policy
    -- based on those assumptions.
    when (currentRole > ActivatedAccessRole && targetRole <= ActivatedAccessRole) $ do
      mIds <- map memId <$> use usersL
      activated <- fmap User.userId <$> lift (lookupActivatedUsers mIds)
      let isActivated user = memId user `elem` activated
      usersL %= filter isActivated
    -- In a team-only conversation we also want to remove bots and guests
    case (targetRole, Data.convTeam conv) of
      (TeamAccessRole, Just tid) -> do
        currentUsers <- use usersL
        onlyTeamUsers <- flip filterM currentUsers $ \user ->
          lift $ isJust <$> Data.teamMember tid (memId user)
        assign usersL onlyTeamUsers
        botsL .= []
      _ -> return ()
  -- Update Cassandra & send an event
  now <- liftIO getCurrentTime
  let accessEvent = Event ConvAccessUpdate qcnv qusr now (EdConvAccessUpdate body)
  Data.updateConversationAccess cnv targetAccess targetRole
  pushConversationEvent (Just zcon) accessEvent (map memId users) bots
  -- Remove users and bots
  let removedUsers = map memId users \\ map memId newUsers
      removedBots = map botMemId bots \\ map botMemId newBots
  mapM_ (deleteBot cnv) removedBots
  case removedUsers of
    [] -> return ()
    x : xs -> do
      -- FUTUREWORK: deal with remote members, too, see removeMembers
      e <- Data.removeLocalMembersFromLocalConv EventBackwardsCompatibilityUnqualified localDomain conv usr (list1 x xs)
      -- push event to all clients, including zconn
      -- since updateConversationAccess generates a second (member removal) event here
      for_ (newPushLocal ListComplete usr (ConvEvent e) (recipient <$> users)) $ \p -> push1 p
      void . forkIO $ void $ External.deliver (newBots `zip` repeat e)
  -- Return the event
  pure accessEvent
  where
    usersL :: Lens' ([LocalMember], [BotMember]) [LocalMember]
    usersL = _1
    botsL :: Lens' ([LocalMember], [BotMember]) [BotMember]
    botsL = _2

updateConversationReceiptModeH :: UserId ::: ConnId ::: ConvId ::: JsonRequest Public.ConversationReceiptModeUpdate ::: JSON -> Galley Response
updateConversationReceiptModeH (usr ::: zcon ::: cnv ::: req ::: _) = do
  update <- fromJsonBody req
  handleUpdateResult <$> updateConversationReceiptMode usr zcon cnv update

updateConversationReceiptMode :: UserId -> ConnId -> ConvId -> Public.ConversationReceiptModeUpdate -> Galley UpdateResult
updateConversationReceiptMode usr zcon cnv receiptModeUpdate@(Public.ConversationReceiptModeUpdate target) = do
  localDomain <- viewFederationDomain
  let qcnv = Qualified cnv localDomain
      qusr = Qualified usr localDomain
  (bots, users) <- localBotsAndUsers <$> Data.members cnv
  ensureActionAllowedThrowing ModifyConversationReceiptMode =<< getSelfMember usr users
  current <- Data.lookupReceiptMode cnv
  if current == Just target
    then pure Unchanged
    else Updated <$> update qcnv qusr users bots
  where
    update qcnv qusr users bots = do
      -- Update Cassandra & send an event
      Data.updateConversationReceiptMode cnv target
      now <- liftIO getCurrentTime
      let receiptEvent = Event ConvReceiptModeUpdate qcnv qusr now (EdConvReceiptModeUpdate receiptModeUpdate)
      pushConversationEvent (Just zcon) receiptEvent (map memId users) bots
      pure receiptEvent

updateConversationMessageTimerH :: UserId ::: ConnId ::: ConvId ::: JsonRequest Public.ConversationMessageTimerUpdate -> Galley Response
updateConversationMessageTimerH (usr ::: zcon ::: cnv ::: req) = do
  timerUpdate <- fromJsonBody req
  handleUpdateResult <$> updateConversationMessageTimer usr zcon cnv timerUpdate

updateConversationMessageTimer :: UserId -> ConnId -> ConvId -> Public.ConversationMessageTimerUpdate -> Galley UpdateResult
updateConversationMessageTimer usr zcon cnv timerUpdate@(Public.ConversationMessageTimerUpdate target) = do
  localDomain <- viewFederationDomain
  let qcnv = Qualified cnv localDomain
      qusr = Qualified usr localDomain
  -- checks and balances
  (bots, users) <- localBotsAndUsers <$> Data.members cnv
  ensureActionAllowedThrowing ModifyConversationMessageTimer =<< getSelfMember usr users
  conv <- Data.conversation cnv >>= ifNothing (errorDescriptionToWai convNotFound)
  ensureGroupConvThrowing conv
  let currentTimer = Data.convMessageTimer conv
  if currentTimer == target
    then pure Unchanged
    else Updated <$> update qcnv qusr users bots
  where
    update qcnv qusr users bots = do
      -- update cassandra & send event
      now <- liftIO getCurrentTime
      let timerEvent = Event ConvMessageTimerUpdate qcnv qusr now (EdConvMessageTimerUpdate timerUpdate)
      Data.updateConversationMessageTimer cnv target
      pushConversationEvent (Just zcon) timerEvent (map memId users) bots
      pure timerEvent

addCodeH :: UserId ::: ConnId ::: ConvId -> Galley Response
addCodeH (usr ::: zcon ::: cnv) =
  addCode usr zcon cnv <&> \case
    CodeAdded event -> json event & setStatus status201
    CodeAlreadyExisted conversationCode -> json conversationCode & setStatus status200

data AddCodeResult
  = CodeAdded Public.Event
  | CodeAlreadyExisted Public.ConversationCode

addCode :: UserId -> ConnId -> ConvId -> Galley AddCodeResult
addCode usr zcon cnv = do
  localDomain <- viewFederationDomain
  let qcnv = Qualified cnv localDomain
      qusr = Qualified usr localDomain
  conv <- Data.conversation cnv >>= ifNothing (errorDescriptionToWai convNotFound)
  ensureConvMember (Data.convLocalMembers conv) usr
  ensureAccess conv CodeAccess
  let (bots, users) = localBotsAndUsers $ Data.convLocalMembers conv
  key <- mkKey cnv
  mCode <- Data.lookupCode key ReusableCode
  case mCode of
    Nothing -> do
      code <- generate cnv ReusableCode (Timeout 3600 * 24 * 365) -- one year TODO: configurable
      Data.insertCode code
      now <- liftIO getCurrentTime
      conversationCode <- createCode code
      let event = Event ConvCodeUpdate qcnv qusr now (EdConvCodeUpdate conversationCode)
      pushConversationEvent (Just zcon) event (map memId users) bots
      pure $ CodeAdded event
    Just code -> do
      conversationCode <- createCode code
      pure $ CodeAlreadyExisted conversationCode
  where
    createCode :: Code -> Galley ConversationCode
    createCode code = do
      urlPrefix <- view $ options . optSettings . setConversationCodeURI
      return $ mkConversationCode (codeKey code) (codeValue code) urlPrefix

rmCodeH :: UserId ::: ConnId ::: ConvId -> Galley Response
rmCodeH (usr ::: zcon ::: cnv) =
  setStatus status200 . json <$> rmCode usr zcon cnv

rmCode :: UserId -> ConnId -> ConvId -> Galley Public.Event
rmCode usr zcon cnv = do
  localDomain <- viewFederationDomain
  let qcnv = Qualified cnv localDomain
      qusr = Qualified usr localDomain
  conv <- Data.conversation cnv >>= ifNothing (errorDescriptionToWai convNotFound)
  ensureConvMember (Data.convLocalMembers conv) usr
  ensureAccess conv CodeAccess
  let (bots, users) = localBotsAndUsers $ Data.convLocalMembers conv
  key <- mkKey cnv
  Data.deleteCode key ReusableCode
  now <- liftIO getCurrentTime
  let event = Event ConvCodeDelete qcnv qusr now EdConvCodeDelete
  pushConversationEvent (Just zcon) event (map memId users) bots
  pure event

getCodeH :: UserId ::: ConvId -> Galley Response
getCodeH (usr ::: cnv) =
  setStatus status200 . json <$> getCode usr cnv

getCode :: UserId -> ConvId -> Galley Public.ConversationCode
getCode usr cnv = do
  conv <- Data.conversation cnv >>= ifNothing (errorDescriptionToWai convNotFound)
  ensureAccess conv CodeAccess
  ensureConvMember (Data.convLocalMembers conv) usr
  key <- mkKey cnv
  c <-
    Data.lookupCode key ReusableCode
      >>= ifNothing (errorDescriptionToWai codeNotFound)
  returnCode c

returnCode :: Code -> Galley Public.ConversationCode
returnCode c = do
  urlPrefix <- view $ options . optSettings . setConversationCodeURI
  pure $ Public.mkConversationCode (codeKey c) (codeValue c) urlPrefix

checkReusableCodeH :: JsonRequest Public.ConversationCode -> Galley Response
checkReusableCodeH req = do
  convCode <- fromJsonBody req
  checkReusableCode convCode
  pure empty

checkReusableCode :: Public.ConversationCode -> Galley ()
checkReusableCode convCode =
  void $ verifyReusableCode convCode

joinConversationByReusableCodeH :: UserId ::: ConnId ::: JsonRequest Public.ConversationCode -> Galley Response
joinConversationByReusableCodeH (zusr ::: zcon ::: req) = do
  convCode <- fromJsonBody req
  handleUpdateResult <$> joinConversationByReusableCode zusr zcon convCode

joinConversationByReusableCode :: UserId -> ConnId -> Public.ConversationCode -> Galley UpdateResult
joinConversationByReusableCode zusr zcon convCode = do
  c <- verifyReusableCode convCode
  joinConversation zusr zcon (codeConversation c) CodeAccess

joinConversationByIdH :: UserId ::: ConnId ::: ConvId ::: JSON -> Galley Response
joinConversationByIdH (zusr ::: zcon ::: cnv ::: _) =
  handleUpdateResult <$> joinConversationById zusr zcon cnv

joinConversationById :: UserId -> ConnId -> ConvId -> Galley UpdateResult
joinConversationById zusr zcon cnv =
  joinConversation zusr zcon cnv LinkAccess

joinConversation :: UserId -> ConnId -> ConvId -> Access -> Galley UpdateResult
joinConversation zusr zcon cnv access = do
  conv <- ensureConversationAccess zusr cnv access
  let newUsers = filter (notIsMember conv) [zusr]
  -- FUTUREWORK: remote users?
  ensureMemberLimit (toList $ Data.convLocalMembers conv) newUsers []
  -- NOTE: When joining conversations, all users become members
  -- as this is our desired behavior for these types of conversations
  -- where there is no way to control who joins, etc.
  let mems = localBotsAndUsers (Data.convLocalMembers conv)
  let rMems = Data.convRemoteMembers conv
  addToConversation mems rMems (zusr, roleNameWireMember) zcon (mkList1WithOrigin ((,roleNameWireMember) <$> newUsers) []) conv

addMembersH :: UserId ::: ConnId ::: ConvId ::: JsonRequest Public.Invite -> Galley Response
addMembersH (zusr ::: zcon ::: cid ::: req) = do
  (Invite u r) <- fromJsonBody req
  domain <- viewFederationDomain
  let qInvite = Public.InviteQualified (flip Qualified domain <$> toNonEmpty u) r
  handleUpdateResult <$> addMembers zusr zcon cid qInvite

-- FUTUREWORK(federation): we need the following checks/implementation:
--  - (1) [DONE] Remote qualified users must exist before they can be added (a
--  call to the respective backend should be made): Avoid clients making up random
--  Ids, and increase the chances that the updateConversationMemberships call
--  suceeds
--  - (2) [DONE] A call must be made to the remote backend informing it that this user is
--  now part of that conversation. Use and implement 'updateConversationMemberships'.
--    - that call should probably be made *after* inserting the conversation membership
--    happens in this backend.
--    - 'updateConversationMemberships' should send an event to the affected
--    users informing them they have joined a remote conversation.
--  - (3) Events should support remote / qualified users, too.
--  These checks need tests :)
addMembers :: UserId -> ConnId -> ConvId -> Public.InviteQualified -> Galley UpdateResult
addMembers zusr zcon convId invite = do
  conv <- Data.conversation convId >>= ifNothing (errorDescriptionToWai convNotFound)
  let mems = localBotsAndUsers (Data.convLocalMembers conv)
  let rMems = Data.convRemoteMembers conv
  self <- getSelfMember zusr (snd mems)
  ensureActionAllowedThrowing AddConversationMember self
  let invitedUsers = toList $ Public.invQUsers invite
  domain <- viewFederationDomain
  let (invitedRemotes, invitedLocals) = partitionRemoteOrLocalIds' domain invitedUsers
  let newLocals = filter (notIsMember conv) invitedLocals
  let newRemotes = filter (notIsMember' conv) invitedRemotes
  ensureMemberLimit (toList $ Data.convLocalMembers conv) newLocals newRemotes
  ensureAccess conv InviteAccess
  ensureConvRoleNotElevated self (invQRoleName invite)
  checkLocals conv (Data.convTeam conv) newLocals
  checkRemoteUsersExist newRemotes
  checkLHPolicyConflictsLocal conv newLocals
  checkLHPolicyConflictsRemote (FutureWork newRemotes)
  addToConversation mems rMems (zusr, memConvRoleName self) zcon (combineUsers newLocals newRemotes) conv
  where
    userIsMember u = (^. userId . to (== u))

    checkLocals :: Data.Conversation -> Maybe TeamId -> [UserId] -> Galley ()
    checkLocals conv (Just tid) newUsers = do
      tms <- Data.teamMembersLimited tid newUsers
      let userMembershipMap = map (\u -> (u, find (userIsMember u) tms)) newUsers
      ensureAccessRole (Data.convAccessRole conv) userMembershipMap
      tcv <- Data.teamConversation tid convId
      when (maybe True (view managedConversation) tcv) $
        throwM noAddToManaged
      ensureConnectedOrSameTeam zusr newUsers
    checkLocals conv Nothing newUsers = do
      ensureAccessRole (Data.convAccessRole conv) (zip newUsers $ repeat Nothing)
      ensureConnectedOrSameTeam zusr newUsers

    checkLHPolicyConflictsLocal :: Data.Conversation -> [UserId] -> Galley ()
    checkLHPolicyConflictsLocal conv newUsers = do
      let convUsers = Data.convLocalMembers conv

      allNewUsersGaveConsent <- allLegalholdConsentGiven newUsers

      whenM (anyLegalholdActivated (memId <$> convUsers)) $
        unless allNewUsersGaveConsent $
          throwErrorDescription missingLegalholdConsent

      whenM (anyLegalholdActivated newUsers) $ do
        unless allNewUsersGaveConsent $
          throwErrorDescription missingLegalholdConsent

        convUsersLHStatus <- do
          uidsStatus <- getLHStatusForUsers (memId <$> convUsers)
          pure $ zipWith (\mem (_, status) -> (mem, status)) convUsers uidsStatus

        if any
          ( \(mem, status) ->
              memConvRoleName mem == roleNameWireAdmin
                && consentGiven status == ConsentGiven
          )
          convUsersLHStatus
          then do
            localDomain <- viewFederationDomain
            for_ convUsersLHStatus $ \(mem, status) ->
              when (consentGiven status == ConsentNotGiven) $
                let qvictim = Qualified (memId mem) localDomain
                 in void $ removeMember EventBackwardsCompatibilityUnqualified (memId mem) Nothing (Data.convId conv) qvictim
          else throwErrorDescription missingLegalholdConsent

    checkLHPolicyConflictsRemote :: FutureWork 'LegalholdPlusFederationNotImplemented [Remote UserId] -> Galley ()
    checkLHPolicyConflictsRemote _remotes = pure ()

    combineUsers ::
      [UserId] ->
      [Remote UserId] ->
      Maybe (List1WithOrigin (UserId, RoleName) (Remote UserId, RoleName))
    combineUsers nls nrs =
      mkList1WithOrigin ((,invQRoleName invite) <$> nls) ((,invQRoleName invite) <$> nrs)

updateSelfMemberH :: UserId ::: ConnId ::: ConvId ::: JsonRequest Public.MemberUpdate -> Galley Response
updateSelfMemberH (zusr ::: zcon ::: cid ::: req) = do
  update <- fromJsonBody req
  updateSelfMember zusr zcon cid update
  return empty

updateSelfMember :: UserId -> ConnId -> ConvId -> Public.MemberUpdate -> Galley ()
updateSelfMember zusr zcon cid update = do
  conv <- getConversationAndCheckMembership zusr cid
  m <- getSelfMember zusr (Data.convLocalMembers conv)
  -- Ensure no self role upgrades
  for_ (mupConvRoleName update) $ ensureConvRoleNotElevated m
  void $ processUpdateMemberEvent zusr zcon cid [m] m update

updateOtherMemberH :: UserId ::: ConnId ::: ConvId ::: UserId ::: JsonRequest Public.OtherMemberUpdate -> Galley Response
updateOtherMemberH (zusr ::: zcon ::: cid ::: victim ::: req) = do
  update <- fromJsonBody req
  updateOtherMember zusr zcon cid victim update
  return empty

updateOtherMember :: UserId -> ConnId -> ConvId -> UserId -> Public.OtherMemberUpdate -> Galley ()
updateOtherMember zusr zcon cid victim update = do
  when (zusr == victim) $
    throwM invalidTargetUserOp
  conv <- getConversationAndCheckMembership zusr cid
  let (bots, users) = localBotsAndUsers (Data.convLocalMembers conv)
  ensureActionAllowedThrowing ModifyOtherConversationMember =<< getSelfMember zusr users
  memTarget <- getOtherMember victim users
  e <- processUpdateMemberEvent zusr zcon cid users memTarget (memberUpdate {mupConvRoleName = omuConvRoleName update})
  void . forkIO $ void $ External.deliver (bots `zip` repeat e)

removeMember :: EventBackwardsCompatibility -> UserId -> Maybe ConnId -> ConvId -> Qualified UserId -> Galley RemoveFromConversation
removeMember compatibility zusr zcon convId victim =
  fmap (either mapError RemoveFromConversationUpdated) . runExceptT $
    removeMemberFromLocalConv compatibility zusr zcon convId victim
  where
    mapError :: RemoveFromConversationError -> RemoveFromConversation
    mapError = \case
      RemoveFromConversationErrorNotAllowed a -> RemoveFromConversationNotAllowed a
      RemoveFromConversationErrorManagedConvNotAllowed -> RemoveFromConversationManagedConvNotAllowed
      RemoveFromConversationErrorNotFound -> RemoveFromConversationNotFound
      RemoveFromConversationErrorCustomRolesNotSupported -> RemoveFromConversationCustomRolesNotSupported
      RemoveFromConversationErrorSelfConv -> RemoveFromConversationSelfConv
      RemoveFromConversationErrorOne2OneConv -> RemoveFromConversationOne2OneConv
      RemoveFromConversationErrorConnectConv -> RemoveFromConversationConnectConv
      RemoveFromConversationErrorUnchanged -> RemoveFromConversationUnchanged

removeMemberUnqualified :: UserId -> ConnId -> ConvId -> UserId -> Galley RemoveFromConversation
removeMemberUnqualified zusr zcon conv victim = do
  localDomain <- viewFederationDomain
  removeMember EventBackwardsCompatibilityUnqualified zusr (Just zcon) conv (Qualified victim localDomain)

removeMemberQualified :: UserId -> ConnId -> ConvId -> Qualified UserId -> Galley RemoveFromConversation
removeMemberQualified zusr zcon = removeMember EventBackwardsCompatibilityQualified zusr (Just zcon)

removeMemberFromLocalConv ::
  EventBackwardsCompatibility ->
  UserId ->
  Maybe ConnId ->
  ConvId ->
  Qualified UserId ->
  ExceptT RemoveFromConversationError Galley Public.Event
removeMemberFromLocalConv compatibility zusr zcon convId qvictim@(Qualified victim victimDomain) = do
  localDomain <- viewFederationDomain
  conv <-
    lift (Data.conversation convId)
      >>= maybe (throwE RemoveFromConversationErrorNotFound) pure
  let (bots, locals) = localBotsAndUsers (Data.convLocalMembers conv)
  genConvChecks conv locals
  for_ (Data.convTeam conv) teamConvChecks

  if victimDomain == localDomain && victim `isMember` locals
    || toRemote qvictim `isRemoteMember` Data.convRemoteMembers conv
    then lift $ do
      let (remoteVictim, _localVictim) = partitionRemoteOrLocalIds' localDomain (singleton qvictim)
      event <-
        if victimDomain == localDomain
          then Data.removeLocalMembersFromLocalConv compatibility localDomain conv zusr (singleton victim)
          else Data.removeRemoteMembersFromLocalConv localDomain conv zusr (singleton . toRemote $ qvictim)
      -- Notify local users
      for_ (newPushLocal ListComplete zusr (ConvEvent event) (recipient <$> locals)) $ \p ->
        push1 $ p & pushConn .~ zcon

      void . forkIO $ void $ External.deliver (bots `zip` repeat event)

      -- Notify remote backends
      let stayingRemotes = (rmId <$> Data.convRemoteMembers conv) \\ remoteVictim
          victimAsList
            | victimDomain == localDomain = OnlyFirstList . singleton $ victim
            | otherwise = OnlySecondList . singleton . toRemote $ qvictim
      notifyRemoteOfRemovedConvMembers stayingRemotes zusr (evtTime event) conv victimAsList

      pure event
    else throwE RemoveFromConversationErrorUnchanged
  where
    genConvChecks ::
      Data.Conversation ->
      [LocalMember] ->
      ExceptT RemoveFromConversationError Galley ()
    genConvChecks conv usrs = do
      localDomain <- viewFederationDomain
      case ensureGroupConv (Data.convType conv) of
        Left GroupConvInvalidOpSelfConv -> throwE RemoveFromConversationErrorSelfConv
        Left GroupConvInvalidOpOne2OneConv -> throwE RemoveFromConversationErrorOne2OneConv
        Left GroupConvInvalidOpConnectConv -> throwE RemoveFromConversationErrorConnectConv
        Right () -> pure ()
      selfMember <- lift $ getSelfMember zusr usrs
      let action
            | Qualified zusr localDomain == qvictim = LeaveConversation
            | otherwise = RemoveConversationMember
      case ensureActionAllowed action selfMember of
        ACOAllowed -> pure ()
        ACOActionDenied a -> throwE . RemoveFromConversationErrorNotAllowed $ a
        ACOCustomRolesNotSupported -> throwE RemoveFromConversationErrorCustomRolesNotSupported
    teamConvChecks :: TeamId -> ExceptT RemoveFromConversationError Galley ()
    teamConvChecks tid = do
      tcv <- Data.teamConversation tid convId
      when (maybe False (view managedConversation) tcv) $
        throwE RemoveFromConversationErrorManagedConvNotAllowed

-- OTR

data OtrResult
  = OtrSent !Public.ClientMismatch
  | OtrMissingRecipients !Public.ClientMismatch
  | OtrUnknownClient !Public.UnknownClient
  | OtrConversationNotFound !Public.ConvNotFound

handleOtrResult :: OtrResult -> Galley Response
handleOtrResult = \case
  OtrSent m -> pure $ json m & setStatus status201
  OtrMissingRecipients m -> pure $ json m & setStatus status412
  OtrUnknownClient _ -> throwErrorDescription unknownClient
  OtrConversationNotFound _ -> throwErrorDescription convNotFound

postBotMessageH :: BotId ::: ConvId ::: Public.OtrFilterMissing ::: JsonRequest Public.NewOtrMessage ::: JSON -> Galley Response
postBotMessageH (zbot ::: zcnv ::: val ::: req ::: _) = do
  message <- fromJsonBody req
  let val' = allowOtrFilterMissingInBody val message
  handleOtrResult =<< postBotMessage zbot zcnv val' message

postBotMessage :: BotId -> ConvId -> Public.OtrFilterMissing -> Public.NewOtrMessage -> Galley OtrResult
postBotMessage zbot zcnv val message =
  postNewOtrMessage Bot (botUserId zbot) Nothing zcnv val message

-- | FUTUREWORK: Send message to remote users, as of now this function fails if
-- the conversation is not hosted on current backend. If the conversation is
-- hosted on current backend, it completely ignores remote users.
postProteusMessage :: UserId -> ConnId -> Qualified ConvId -> RawProto Public.QualifiedNewOtrMessage -> Galley (Public.PostOtrResponse Public.MessageSendingStatus)
postProteusMessage zusr zcon conv msg = do
  localDomain <- viewFederationDomain
  let sender = Qualified zusr localDomain
  if localDomain /= qDomain conv
    then postRemoteOtrMessage sender conv (rpRaw msg)
    else postQualifiedOtrMessage User sender (Just zcon) (qUnqualified conv) (rpValue msg)

postOtrMessageUnqualified :: UserId -> ConnId -> ConvId -> Maybe Public.IgnoreMissing -> Maybe Public.ReportMissing -> Public.NewOtrMessage -> Galley (Public.PostOtrResponse Public.ClientMismatch)
postOtrMessageUnqualified zusr zcon cnv ignoreMissing reportMissing message = do
  localDomain <- viewFederationDomain
  let sender = Qualified zusr localDomain
      qualifiedRecipients =
        Public.QualifiedOtrRecipients
          . QualifiedUserClientMap
          . Map.singleton localDomain
          . userClientMap
          . fmap fromBase64TextLenient
          . Public.otrRecipientsMap
          . Public.newOtrRecipients
          $ message
      clientMismatchStrategy = legacyClientMismatchStrategy localDomain (newOtrReportMissing message) ignoreMissing reportMissing
      qualifiedMessage =
        Public.QualifiedNewOtrMessage
          { Public.qualifiedNewOtrSender = newOtrSender message,
            Public.qualifiedNewOtrRecipients = qualifiedRecipients,
            Public.qualifiedNewOtrNativePush = newOtrNativePush message,
            Public.qualifiedNewOtrTransient = newOtrTransient message,
            Public.qualifiedNewOtrNativePriority = newOtrNativePriority message,
            Public.qualifiedNewOtrData = maybe mempty fromBase64TextLenient (newOtrData message),
            Public.qualifiedNewOtrClientMismatchStrategy = clientMismatchStrategy
          }
  unqualify localDomain
    <$> postQualifiedOtrMessage User sender (Just zcon) cnv qualifiedMessage

postProtoOtrBroadcastH :: UserId ::: ConnId ::: Public.OtrFilterMissing ::: Request ::: JSON -> Galley Response
postProtoOtrBroadcastH (zusr ::: zcon ::: val ::: req ::: _) = do
  message <- Public.protoToNewOtrMessage <$> fromProtoBody req
  let val' = allowOtrFilterMissingInBody val message
  handleOtrResult =<< postOtrBroadcast zusr zcon val' message

postOtrBroadcastH :: UserId ::: ConnId ::: Public.OtrFilterMissing ::: JsonRequest Public.NewOtrMessage -> Galley Response
postOtrBroadcastH (zusr ::: zcon ::: val ::: req) = do
  message <- fromJsonBody req
  let val' = allowOtrFilterMissingInBody val message
  handleOtrResult =<< postOtrBroadcast zusr zcon val' message

postOtrBroadcast :: UserId -> ConnId -> Public.OtrFilterMissing -> Public.NewOtrMessage -> Galley OtrResult
postOtrBroadcast zusr zcon = postNewOtrBroadcast zusr (Just zcon)

-- internal OTR helpers

-- This is a work-around for the fact that we sometimes want to send larger lists of user ids
-- in the filter query than fits the url length limit.  for details, see
-- https://github.com/zinfra/backend-issues/issues/1248
allowOtrFilterMissingInBody :: OtrFilterMissing -> NewOtrMessage -> OtrFilterMissing
allowOtrFilterMissingInBody val (NewOtrMessage _ _ _ _ _ _ mrepmiss) = case mrepmiss of
  Nothing -> val
  Just uids -> OtrReportMissing $ Set.fromList uids

-- | bots are not supported on broadcast
postNewOtrBroadcast :: UserId -> Maybe ConnId -> OtrFilterMissing -> NewOtrMessage -> Galley OtrResult
postNewOtrBroadcast usr con val msg = do
  localDomain <- viewFederationDomain
  let qusr = Qualified usr localDomain
      sender = newOtrSender msg
      recvrs = newOtrRecipients msg
  now <- liftIO getCurrentTime
  withValidOtrBroadcastRecipients usr sender recvrs val now $ \rs -> do
    let (_, toUsers) = foldr (newMessage qusr con Nothing msg now) ([], []) rs
    pushSome (catMaybes toUsers)

postNewOtrMessage :: UserType -> UserId -> Maybe ConnId -> ConvId -> OtrFilterMissing -> NewOtrMessage -> Galley OtrResult
postNewOtrMessage utype usr con cnv val msg = do
  localDomain <- viewFederationDomain
  let qusr = Qualified usr localDomain
      qcnv = Qualified cnv localDomain
      sender = newOtrSender msg
      recvrs = newOtrRecipients msg
  now <- liftIO getCurrentTime
  withValidOtrRecipients utype usr sender cnv recvrs val now $ \rs -> do
    let (toBots, toUsers) = foldr (newMessage qusr con (Just qcnv) msg now) ([], []) rs
    pushSome (catMaybes toUsers)
    void . forkIO $ do
      gone <- External.deliver toBots
      mapM_ (deleteBot cnv . botMemId) gone

-- | Locally post a message originating from a remote conversation
-- FUTUREWORK: error handling for missing / mismatched clients
postRemoteToLocal :: RemoteMessage (Remote ConvId) -> Galley ()
postRemoteToLocal rm = do
  localDomain <- viewFederationDomain
  let UserClientMap rcpts = rmRecipients rm
      Tagged conv = rmConversation rm
  -- FUTUREWORK(authorization) review whether filtering members is appropriate
  -- at this stage
  (members, allMembers) <- Data.filterRemoteConvMembers (Map.keys rcpts) conv
  unless allMembers $
    Log.warn $
      Log.field "conversation" (toByteString' (qUnqualified conv))
        Log.~~ Log.field "domain" (toByteString' (qDomain conv))
        Log.~~ Log.msg
          ( "Attempt to send remote message to local\
            \ users not in the conversation" ::
              Text
          )
  let rcpts' = do
        m <- members
        (c, t) <- maybe [] Map.assocs (rcpts ^? ix m)
        pure (m, c, t)
  let remoteToLocalPush (rcpt, rcptc, ciphertext) =
        newPush1
          ListComplete
          (guard (localDomain == qDomain (rmSender rm)) $> qUnqualified (rmSender rm))
          ( ConvEvent
              ( Event
                  OtrMessageAdd
                  conv
                  (rmSender rm)
                  (rmTime rm)
                  ( EdOtrMessage
                      ( OtrMessage
                          { otrSender = rmSenderClient rm,
                            otrRecipient = rcptc,
                            otrCiphertext = ciphertext,
                            otrData = rmData rm
                          }
                      )
                  )
              )
          )
          (singleton (userRecipient rcpt))
          -- FUTUREWORK: unify event creation logic after #1634 is merged
          & pushNativePriority .~ rmPriority rm
          & pushRoute .~ bool RouteDirect RouteAny (rmPush rm)
          & pushTransient .~ rmTransient rm
  pushSome (map remoteToLocalPush rcpts')

newMessage ::
  Qualified UserId ->
  Maybe ConnId ->
  -- | Conversation Id (if Nothing, recipient's self conversation is used)
  Maybe (Qualified ConvId) ->
  NewOtrMessage ->
  UTCTime ->
  (LocalMember, ClientId, Text) ->
  ([(BotMember, Event)], [Maybe Push]) ->
  ([(BotMember, Event)], [Maybe Push])
newMessage qusr con qcnv msg now (m, c, t) ~(toBots, toUsers) =
  let o =
        OtrMessage
          { otrSender = newOtrSender msg,
            otrRecipient = c,
            otrCiphertext = t,
            otrData = newOtrData msg
          }
      -- use recipient's client's self conversation on broadcast
      -- (with federation, this might not work for remote members)
      -- FUTUREWORK: for remote recipients, set the domain correctly here
      qconv = fromMaybe ((`Qualified` qDomain qusr) . selfConv $ memId m) qcnv
      e = Event OtrMessageAdd qconv qusr now (EdOtrMessage o)
      r = recipient m & recipientClients .~ RecipientClientsSome (singleton c)
   in case newBotMember m of
        Just b -> ((b, e) : toBots, toUsers)
        Nothing ->
          let p =
                newPushLocal ListComplete (qUnqualified (evtFrom e)) (ConvEvent e) [r]
                  <&> set pushConn con
                    . set pushNativePriority (newOtrNativePriority msg)
                    . set pushRoute (bool RouteDirect RouteAny (newOtrNativePush msg))
                    . set pushTransient (newOtrTransient msg)
           in (toBots, p : toUsers)

updateConversationDeprecatedH :: UserId ::: ConnId ::: ConvId ::: JsonRequest Public.ConversationRename -> Galley Response
updateConversationDeprecatedH (zusr ::: zcon ::: cnv ::: req) = do
  convRename <- fromJsonBody req
  setStatus status200 . json <$> updateConversationName zusr zcon cnv convRename

updateConversationNameH :: UserId ::: ConnId ::: ConvId ::: JsonRequest Public.ConversationRename -> Galley Response
updateConversationNameH (zusr ::: zcon ::: cnv ::: req) = do
  convRename <- fromJsonBody req
  setStatus status200 . json <$> updateConversationName zusr zcon cnv convRename

updateConversationName :: UserId -> ConnId -> ConvId -> Public.ConversationRename -> Galley Public.Event
updateConversationName zusr zcon cnv convRename = do
  localDomain <- viewFederationDomain
  let qcnv = Qualified cnv localDomain
      qusr = Qualified zusr localDomain
  alive <- Data.isConvAlive cnv
  unless alive $ do
    Data.deleteConversation cnv
    throwErrorDescription convNotFound
  (bots, users) <- localBotsAndUsers <$> Data.members cnv
  ensureActionAllowedThrowing ModifyConversationName =<< getSelfMember zusr users
  now <- liftIO getCurrentTime
  cn <- rangeChecked (cupName convRename)
  Data.updateConversation cnv cn
  let e = Event ConvRename qcnv qusr now (EdConvRename convRename)
  for_ (newPushLocal ListComplete zusr (ConvEvent e) (recipient <$> users)) $ \p ->
    push1 $ p & pushConn ?~ zcon
  void . forkIO $ void $ External.deliver (bots `zip` repeat e)
  return e

isTypingH :: UserId ::: ConnId ::: ConvId ::: JsonRequest Public.TypingData -> Galley Response
isTypingH (zusr ::: zcon ::: cnv ::: req) = do
  typingData <- fromJsonBody req
  isTyping zusr zcon cnv typingData
  pure empty

isTyping :: UserId -> ConnId -> ConvId -> Public.TypingData -> Galley ()
isTyping zusr zcon cnv typingData = do
  localDomain <- viewFederationDomain
  let qcnv = Qualified cnv localDomain
      qusr = Qualified zusr localDomain
  mm <- Data.members cnv
  unless (zusr `isMember` mm) $
    throwErrorDescription convNotFound
  now <- liftIO getCurrentTime
  let e = Event Typing qcnv qusr now (EdTyping typingData)
  for_ (newPushLocal ListComplete zusr (ConvEvent e) (recipient <$> mm)) $ \p ->
    push1 $
      p
        & pushConn ?~ zcon
        & pushRoute .~ RouteDirect
        & pushTransient .~ True

addServiceH :: JsonRequest Service -> Galley Response
addServiceH req = do
  Data.insertService =<< fromJsonBody req
  return empty

rmServiceH :: JsonRequest ServiceRef -> Galley Response
rmServiceH req = do
  Data.deleteService =<< fromJsonBody req
  return empty

addBotH :: UserId ::: ConnId ::: JsonRequest AddBot -> Galley Response
addBotH (zusr ::: zcon ::: req) = do
  bot <- fromJsonBody req
  json <$> addBot zusr zcon bot

addBot :: UserId -> ConnId -> AddBot -> Galley Event
addBot zusr zcon b = do
  localDomain <- viewFederationDomain
  let qusr = Qualified zusr localDomain
  c <- Data.conversation (b ^. addBotConv) >>= ifNothing (errorDescriptionToWai convNotFound)
  -- Check some preconditions on adding bots to a conversation
  for_ (Data.convTeam c) $ teamConvChecks (b ^. addBotConv)
  (bots, users) <- regularConvChecks c
  t <- liftIO getCurrentTime
  Data.updateClient True (botUserId (b ^. addBotId)) (b ^. addBotClient)
  (e, bm) <- Data.addBotMember qusr (b ^. addBotService) (b ^. addBotId) (b ^. addBotConv) t
  for_ (newPushLocal ListComplete zusr (ConvEvent e) (recipient <$> users)) $ \p ->
    push1 $ p & pushConn ?~ zcon
  void . forkIO $ void $ External.deliver ((bm : bots) `zip` repeat e)
  pure e
  where
    regularConvChecks c = do
      let (bots, users) = localBotsAndUsers (Data.convLocalMembers c)
      unless (zusr `isMember` users) $
        throwErrorDescription convNotFound
      ensureGroupConvThrowing c
      ensureActionAllowedThrowing AddConversationMember =<< getSelfMember zusr users
      unless (any ((== b ^. addBotId) . botMemId) bots) $
        ensureMemberLimit (toList $ Data.convLocalMembers c) [botUserId (b ^. addBotId)] []
      return (bots, users)
    teamConvChecks cid tid = do
      tcv <- Data.teamConversation tid cid
      when (maybe True (view managedConversation) tcv) $
        throwM noAddToManaged

rmBotH :: UserId ::: Maybe ConnId ::: JsonRequest RemoveBot -> Galley Response
rmBotH (zusr ::: zcon ::: req) = do
  bot <- fromJsonBody req
  handleUpdateResult <$> rmBot zusr zcon bot

rmBot :: UserId -> Maybe ConnId -> RemoveBot -> Galley UpdateResult
rmBot zusr zcon b = do
  c <- Data.conversation (b ^. rmBotConv) >>= ifNothing (errorDescriptionToWai convNotFound)
  localDomain <- viewFederationDomain
  let qcnv = Qualified (Data.convId c) localDomain
      qusr = Qualified zusr localDomain
  unless (zusr `isMember` Data.convLocalMembers c) $
    throwErrorDescription convNotFound
  let (bots, users) = localBotsAndUsers (Data.convLocalMembers c)
  if not (any ((== b ^. rmBotId) . botMemId) bots)
    then pure Unchanged
    else do
      t <- liftIO getCurrentTime
      let evd = EdMembersLeave (UserIdList [Qualified (botUserId (b ^. rmBotId)) localDomain])
      let e = Event MemberLeave qcnv qusr t evd
      for_ (newPushLocal ListComplete zusr (ConvEvent e) (recipient <$> users)) $ \p ->
        push1 $ p & pushConn .~ zcon
      Data.removeMember (botUserId (b ^. rmBotId)) (Data.convId c)
      Data.eraseClients (botUserId (b ^. rmBotId))
      void . forkIO $ void $ External.deliver (bots `zip` repeat e)
      pure $ Updated e

-------------------------------------------------------------------------------
-- Helpers

addToConversation ::
  -- | The existing bots and local users in the conversation
  ([BotMember], [LocalMember]) ->
  -- | The existing remote users
  [RemoteMember] ->
  -- | The originating user and their role
  (UserId, RoleName) ->
  -- | The connection ID of the originating user
  ConnId ->
  -- | New users to be added and their roles
  Maybe (List1WithOrigin (UserId, RoleName) (Remote UserId, RoleName)) ->
  -- | The conversation to modify
  Data.Conversation ->
  Galley UpdateResult
addToConversation _ _ _ _ Nothing _ = pure Unchanged
addToConversation (bots, existingLocals) existingRemotes (usr, usrRole) conn (Just newUsers) c = do
  ensureGroupConvThrowing c
  let (newLocals, newRemotes) = splitList1WithOrigin newUsers
  mems <- checkedMemberAddSize newLocals newRemotes
  now <- liftIO getCurrentTime
  localDomain <- viewFederationDomain
  (e, lmm, rmm) <- Data.addMembersWithRole localDomain now (Data.convId c) (usr, usrRole) mems
  for_ (mkList1WithOrigin lmm rmm) $
    notifyRemoteOfNewConvMembers existingRemotes usr now c
  let localsToNotify = nubOrd . fmap memId $ existingLocals <> lmm
  pushConversationEvent (Just conn) e localsToNotify bots
  pure $ Updated e

data GroupConvInvalidOp
  = GroupConvInvalidOpSelfConv
  | GroupConvInvalidOpOne2OneConv
  | GroupConvInvalidOpConnectConv

ensureGroupConv :: ConvType -> Either GroupConvInvalidOp ()
ensureGroupConv = \case
  SelfConv -> Left GroupConvInvalidOpSelfConv
  One2OneConv -> Left GroupConvInvalidOpOne2OneConv
  ConnectConv -> Left GroupConvInvalidOpConnectConv
  _ -> Right ()

ensureGroupConvThrowing :: MonadThrow m => Data.Conversation -> m ()
ensureGroupConvThrowing c = case ensureGroupConv (Data.convType c) of
  Left GroupConvInvalidOpSelfConv -> throwM invalidSelfOp
  Left GroupConvInvalidOpOne2OneConv -> throwM invalidOne2OneOp
  Left GroupConvInvalidOpConnectConv -> throwM invalidConnectOp
  Right () -> return ()

ensureMemberLimit :: [LocalMember] -> [UserId] -> [Remote UserId] -> Galley ()
ensureMemberLimit old newLocals newRemotes = do
  o <- view options
  let maxSize = fromIntegral (o ^. optSettings . setMaxConvSize)
  when (length old + length newLocals + length newRemotes > maxSize) $
    throwM tooManyMembers

notIsMember :: Data.Conversation -> UserId -> Bool
notIsMember cc u = not $ isMember u (Data.convLocalMembers cc)

notIsMember' :: Data.Conversation -> Remote UserId -> Bool
notIsMember' cc u = not $ isRemoteMember u (Data.convRemoteMembers cc)

ensureConvMember :: [LocalMember] -> UserId -> Galley ()
ensureConvMember users usr =
  unless (usr `isMember` users) $
    throwErrorDescription convNotFound

processUpdateMemberEvent ::
  UserId ->
  ConnId ->
  ConvId ->
  [LocalMember] ->
  LocalMember ->
  MemberUpdate ->
  Galley Event
processUpdateMemberEvent zusr zcon cid users target update = do
  localDomain <- viewFederationDomain
  let qcnv = Qualified cid localDomain
      qusr = Qualified zusr localDomain
  up <- Data.updateMember cid (memId target) update
  now <- liftIO getCurrentTime
  let e = Event MemberStateUpdate qcnv qusr now (EdMemberUpdate up)
  let recipients = fmap recipient (target : filter ((/= memId target) . memId) users)
  for_ (newPushLocal ListComplete zusr (ConvEvent e) recipients) $ \p ->
    push1 $
      p
        & pushConn ?~ zcon
        & pushRoute .~ RouteDirect
  return e

-------------------------------------------------------------------------------
-- OtrRecipients Validation

data CheckedOtrRecipients
  = -- | Valid sender (user and client) and no missing recipients,
    -- or missing recipients have been willfully ignored.
    ValidOtrRecipients !ClientMismatch [(LocalMember, ClientId, Text)]
  | -- | Missing recipients.
    MissingOtrRecipients !ClientMismatch
  | -- | Invalid sender (user).
    InvalidOtrSenderUser
  | -- | Invalid sender (client).
    InvalidOtrSenderClient

-- | bots are not supported on broadcast
withValidOtrBroadcastRecipients ::
  UserId ->
  ClientId ->
  OtrRecipients ->
  OtrFilterMissing ->
  UTCTime ->
  ([(LocalMember, ClientId, Text)] -> Galley ()) ->
  Galley OtrResult
withValidOtrBroadcastRecipients usr clt rcps val now go = withBindingTeam usr $ \tid -> do
  limit <- fromIntegral . fromRange <$> fanoutLimit
  -- If we are going to fan this out to more than limit, we want to fail early
  unless (Map.size (userClientMap (otrRecipientsMap rcps)) <= limit) $
    throwM broadcastLimitExceeded
  -- In large teams, we may still use the broadcast endpoint but only if `report_missing`
  -- is used and length `report_missing` < limit since we cannot fetch larger teams than
  -- that.
  tMembers <-
    fmap (view userId) <$> case val of
      OtrReportMissing us -> maybeFetchLimitedTeamMemberList limit tid us
      _ -> maybeFetchAllMembersInTeam tid
  contacts <- getContactList usr
  let users = Set.toList $ Set.union (Set.fromList tMembers) (Set.fromList contacts)
  isInternal <- view $ options . optSettings . setIntraListing
  clts <-
    if isInternal
      then Clients.fromUserClients <$> Intra.lookupClients users
      else Data.lookupClients users
  let membs = Data.newMember <$> users
  handleOtrResponse User usr clt rcps membs clts val now go
  where
    maybeFetchLimitedTeamMemberList limit tid uListInFilter = do
      -- Get the users in the filter (remote ids are not in a local team)
      let localUserIdsInFilter = toList uListInFilter
      let localUserIdsInRcps = Map.keys $ userClientMap (otrRecipientsMap rcps)
      let localUserIdsToLookup = Set.toList $ Set.union (Set.fromList localUserIdsInFilter) (Set.fromList localUserIdsInRcps)
      unless (length localUserIdsToLookup <= limit) $
        throwM broadcastLimitExceeded
      Data.teamMembersLimited tid localUserIdsToLookup
    maybeFetchAllMembersInTeam tid = do
      mems <- Data.teamMembersForFanout tid
      when (mems ^. teamMemberListType == ListTruncated) $
        throwM broadcastLimitExceeded
      pure (mems ^. teamMembers)

withValidOtrRecipients ::
  UserType ->
  UserId ->
  ClientId ->
  ConvId ->
  OtrRecipients ->
  OtrFilterMissing ->
  UTCTime ->
  ([(LocalMember, ClientId, Text)] -> Galley ()) ->
  Galley OtrResult
withValidOtrRecipients utype usr clt cnv rcps val now go = do
  alive <- Data.isConvAlive cnv
  if not alive
    then do
      Data.deleteConversation cnv
      pure $ OtrConversationNotFound convNotFound
    else do
      localMembers <- Data.members cnv
      let localMemberIds = memId <$> localMembers
      isInternal <- view $ options . optSettings . setIntraListing
      clts <-
        if isInternal
          then Clients.fromUserClients <$> Intra.lookupClients localMemberIds
          else Data.lookupClients localMemberIds
      handleOtrResponse utype usr clt rcps localMembers clts val now go

handleOtrResponse ::
  -- | Type of proposed sender (user / bot)
  UserType ->
  -- | Proposed sender (user)
  UserId ->
  -- | Proposed sender (client)
  ClientId ->
  -- | Proposed recipients (users & clients).
  OtrRecipients ->
  -- | Members to consider as valid recipients.
  [LocalMember] ->
  -- | Clients to consider as valid recipients.
  Clients ->
  -- | How to filter missing clients.
  OtrFilterMissing ->
  -- | The current timestamp.
  UTCTime ->
  -- | Callback if OtrRecipients are valid
  ([(LocalMember, ClientId, Text)] -> Galley ()) ->
  Galley OtrResult
handleOtrResponse utype usr clt rcps membs clts val now go = case checkOtrRecipients usr clt rcps membs clts val now of
  ValidOtrRecipients m r -> go r >> pure (OtrSent m)
  MissingOtrRecipients m -> do
    guardLegalholdPolicyConflicts (userToProtectee utype usr) (missingClients m)
      >>= either (const (throwErrorDescription missingLegalholdConsent)) pure
    pure (OtrMissingRecipients m)
  InvalidOtrSenderUser -> pure $ OtrConversationNotFound convNotFound
  InvalidOtrSenderClient -> pure $ OtrUnknownClient unknownClient

-- | Check OTR sender and recipients for validity and completeness
-- against a given list of valid members and clients, optionally
-- ignoring missing clients. Returns 'ValidOtrRecipients' on success
-- for further processing.
checkOtrRecipients ::
  -- | Proposed sender (user)
  UserId ->
  -- | Proposed sender (client)
  ClientId ->
  -- | Proposed recipients (users & clients).
  OtrRecipients ->
  -- | Members to consider as valid recipients.
  [LocalMember] ->
  -- | Clients to consider as valid recipients.
  Clients ->
  -- | How to filter missing clients.
  OtrFilterMissing ->
  -- | The current timestamp.
  UTCTime ->
  CheckedOtrRecipients
checkOtrRecipients usr sid prs vms vcs val now
  | not (Map.member usr vmembers) = InvalidOtrSenderUser
  | not (Clients.contains usr sid vcs) = InvalidOtrSenderClient
  | not (Clients.null missing) = MissingOtrRecipients mismatch
  | otherwise = ValidOtrRecipients mismatch yield
  where
    yield :: [(LocalMember, ClientId, Text)]
    yield = foldrOtrRecipients next [] prs

    next :: r ~ [(LocalMember, ClientId, c)] => UserId -> ClientId -> c -> r -> r
    next u c t rs
      | Just m <- member u c = (m, c, t) : rs
      | otherwise = rs

    member :: UserId -> ClientId -> Maybe LocalMember
    member u c
      | Just m <- Map.lookup u vmembers,
        Clients.contains u c vclients =
        Just m
      | otherwise = Nothing

    -- Valid recipient members & clients
    vmembers :: Map UserId (InternalMember UserId)
    vmembers = Map.fromList $ map (\m -> (memId m, m)) vms

    vclients :: Clients
    vclients = Clients.rmClient usr sid vcs

    -- Proposed (given) recipients
    recipients :: Map UserId (Map ClientId Text)
    recipients = userClientMap (otrRecipientsMap prs)

    given :: Clients
    given = Clients.fromMap (Map.map Map.keysSet recipients)

    -- Differences between valid and proposed recipients
    missing, unknown, deleted, redundant :: Clients
    missing = filterMissing (Clients.diff vclients given)
    unknown = Clients.diff given vcs
    deleted = Clients.filter (`Map.member` vmembers) unknown
    redundant =
      Clients.diff unknown deleted
        & if Clients.contains usr sid given
          then Clients.insert usr sid
          else id

    mismatch :: ClientMismatch
    mismatch =
      ClientMismatch
        { cmismatchTime = toUTCTimeMillis now,
          missingClients = UserClients (Clients.toMap missing),
          redundantClients = UserClients (Clients.toMap redundant),
          deletedClients = UserClients (Clients.toMap deleted)
        }

    filterMissing :: Clients -> Clients
    filterMissing miss = case val of
      OtrReportAllMissing -> miss
      OtrIgnoreAllMissing -> Clients.nil
      OtrReportMissing us -> Clients.filter (`Set.member` us) miss
      OtrIgnoreMissing us -> Clients.filter (`Set.notMember` us) miss

-- Copied from 'Galley.API.Team' to break import cycles
withBindingTeam :: UserId -> (TeamId -> Galley b) -> Galley b
withBindingTeam zusr callback = do
  tid <- Data.oneUserTeam zusr >>= ifNothing teamNotFound
  binding <- Data.teamBinding tid >>= ifNothing teamNotFound
  case binding of
    Binding -> callback tid
    NonBinding -> throwM nonBindingTeam
