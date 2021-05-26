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
    Galley.API.Update.addMembersH,
    Galley.API.Update.addMembersQH,
    updateSelfMemberH,
    updateOtherMemberH,
    removeMemberH,

    -- * Servant
    UpdateResponses,

    -- * Talking
    postOtrMessageH,
    postProtoOtrMessageH,
    postOtrBroadcastH,
    postProtoOtrBroadcastH,
    isTypingH,

    -- * External Services
    addServiceH,
    rmServiceH,
    Galley.API.Update.addBotH,
    rmBotH,
    postBotMessageH,
  )
where

import Brig.Types.Intra (accountUser)
import qualified Brig.Types.User as User
import Control.Lens
import Control.Monad.Catch
import Control.Monad.State
import Data.Code
import Data.Id
import Data.LegalHold (UserLegalHoldStatus (UserLegalHoldNoConsent), defUserLegalHoldStatus)
import Data.List.Extra (nubOrdOn)
import Data.List1
import qualified Data.Map.Strict as Map
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
import Data.Time
import Galley.API.Error
import Galley.API.Mapping
import qualified Galley.API.Teams as Teams
import Galley.API.Util
import Galley.App
import Galley.Data (teamMember)
import qualified Galley.Data as Data
import Galley.Data.Services as Data
import Galley.Data.Types hiding (Conversation)
import qualified Galley.External as External
import qualified Galley.Intra.Client as Intra
import Galley.Intra.Push
import Galley.Intra.User (deleteBot, getContactList, getUser, lookupActivatedUsers)
import Galley.Options
import Galley.Types
import Galley.Types.Bot hiding (addBot)
import Galley.Types.Clients (Clients)
import qualified Galley.Types.Clients as Clients
import Galley.Types.Conversations.Roles (Action (..), RoleName, roleNameWireMember)
import Galley.Types.Teams hiding (Event, EventData (..), EventType (..), self)
import Galley.Validation
import Gundeck.Types.Push.V2 (RecipientClients (..))
import Imports
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (failure, setStatus, _1, _2)
import Network.Wai.Utilities
import Servant (respond)
import Servant.API (NoContent (NoContent))
import Servant.API.UVerb
import Wire.API.Conversation (InviteQualified (invQRoleName))
import qualified Wire.API.Conversation as Public
import qualified Wire.API.Conversation.Code as Public
import qualified Wire.API.Event.Conversation as Public
import qualified Wire.API.Message as Public
import qualified Wire.API.Message.Proto as Proto
import Wire.API.Routes.Public.Galley (UpdateResponses)
import Wire.API.User (userTeam)
import Wire.API.User.Client (UserClientsFull)
import qualified Wire.API.User.Client as Client

acceptConvH :: UserId ::: Maybe ConnId ::: ConvId -> Galley Response
acceptConvH (usr ::: conn ::: cnv) = do
  setStatus status200 . json <$> acceptConv usr conn cnv

acceptConv :: UserId -> Maybe ConnId -> ConvId -> Galley Conversation
acceptConv usr conn cnv = do
  conv <- Data.conversation cnv >>= ifNothing convNotFound
  conv' <- acceptOne2One usr conv conn
  conversationView usr conv'

blockConvH :: UserId ::: ConvId -> Galley Response
blockConvH (zusr ::: cnv) = do
  empty <$ blockConv zusr cnv

blockConv :: UserId -> ConvId -> Galley ()
blockConv zusr cnv = do
  conv <- Data.conversation cnv >>= ifNothing convNotFound
  unless (Data.convType conv `elem` [ConnectConv, One2OneConv]) $
    throwM $
      invalidOp "block: invalid conversation type"
  let mems = Data.convMembers conv
  when (zusr `isMember` mems) $ Data.removeMember zusr cnv

unblockConvH :: UserId ::: Maybe ConnId ::: ConvId -> Galley Response
unblockConvH (usr ::: conn ::: cnv) = do
  setStatus status200 . json <$> unblockConv usr conn cnv

unblockConv :: UserId -> Maybe ConnId -> ConvId -> Galley Conversation
unblockConv usr conn cnv = do
  conv <- Data.conversation cnv >>= ifNothing convNotFound
  unless (Data.convType conv `elem` [ConnectConv, One2OneConv]) $
    throwM $
      invalidOp "unblock: invalid conversation type"
  conv' <- acceptOne2One usr conv conn
  conversationView usr conv'

-- conversation updates

data UpdateResult
  = Updated Public.Event
  | Unchanged

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
  (bots, users) <- botsAndUsers =<< Data.members cnv
  ensureConvMember users usr
  conv <- Data.conversation cnv >>= ifNothing convNotFound
  -- The conversation has to be a group conversation
  ensureGroupConv conv
  self <- getSelfMember usr users
  ensureActionAllowed ModifyConversationAccess self
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
      ensureActionAllowed RemoveConversationMember self

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
  let cnv = convId conv
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
  let accessEvent = Event ConvAccessUpdate cnv usr now (EdConvAccessUpdate body)
  Data.updateConversationAccess cnv targetAccess targetRole
  pushEvent accessEvent users bots zcon
  -- Remove users and bots
  let removedUsers = map memId users \\ map memId newUsers
      removedBots = map botMemId bots \\ map botMemId newBots
  mapM_ (deleteBot cnv) removedBots
  case removedUsers of
    [] -> return ()
    x : xs -> do
      -- FUTUREWORK: deal with remote members, too, see removeMembers
      e <- Data.removeLocalMembers conv usr (list1 x xs)
      -- push event to all clients, including zconn
      -- since updateConversationAccess generates a second (member removal) event here
      for_ (newPush ListComplete (evtFrom e) (ConvEvent e) (recipient <$> users)) $ \p -> push1 p
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
  (bots, users) <- botsAndUsers =<< Data.members cnv
  ensureActionAllowed ModifyConversationReceiptMode =<< getSelfMember usr users
  current <- Data.lookupReceiptMode cnv
  if current == Just target
    then pure Unchanged
    else Updated <$> update users bots
  where
    update users bots = do
      -- Update Cassandra & send an event
      Data.updateConversationReceiptMode cnv target
      now <- liftIO getCurrentTime
      let receiptEvent = Event ConvReceiptModeUpdate cnv usr now (EdConvReceiptModeUpdate receiptModeUpdate)
      pushEvent receiptEvent users bots zcon
      pure receiptEvent

updateConversationMessageTimerH :: UserId ::: ConnId ::: ConvId ::: JsonRequest Public.ConversationMessageTimerUpdate -> Galley Response
updateConversationMessageTimerH (usr ::: zcon ::: cnv ::: req) = do
  timerUpdate <- fromJsonBody req
  handleUpdateResult <$> updateConversationMessageTimer usr zcon cnv timerUpdate

updateConversationMessageTimer :: UserId -> ConnId -> ConvId -> Public.ConversationMessageTimerUpdate -> Galley UpdateResult
updateConversationMessageTimer usr zcon cnv timerUpdate@(Public.ConversationMessageTimerUpdate target) = do
  -- checks and balances
  (bots, users) <- botsAndUsers =<< Data.members cnv
  ensureActionAllowed ModifyConversationMessageTimer =<< getSelfMember usr users
  conv <- Data.conversation cnv >>= ifNothing convNotFound
  ensureGroupConv conv
  let currentTimer = Data.convMessageTimer conv
  if currentTimer == target
    then pure Unchanged
    else Updated <$> update users bots
  where
    update users bots = do
      -- update cassandra & send event
      now <- liftIO getCurrentTime
      let timerEvent = Event ConvMessageTimerUpdate cnv usr now (EdConvMessageTimerUpdate timerUpdate)
      Data.updateConversationMessageTimer cnv target
      pushEvent timerEvent users bots zcon
      pure timerEvent

pushEvent :: Event -> [LocalMember] -> [BotMember] -> ConnId -> Galley ()
pushEvent e users bots zcon = do
  for_ (newPush ListComplete (evtFrom e) (ConvEvent e) (recipient <$> users)) $ \p ->
    push1 $ p & pushConn ?~ zcon
  void . forkIO $ void $ External.deliver (bots `zip` repeat e)

addCodeH :: UserId ::: ConnId ::: ConvId -> Galley Response
addCodeH (usr ::: zcon ::: cnv) = do
  addCode usr zcon cnv <&> \case
    CodeAdded event -> json event & setStatus status201
    CodeAlreadyExisted conversationCode -> json conversationCode & setStatus status200

data AddCodeResult
  = CodeAdded Public.Event
  | CodeAlreadyExisted Public.ConversationCode

addCode :: UserId -> ConnId -> ConvId -> Galley AddCodeResult
addCode usr zcon cnv = do
  conv <- Data.conversation cnv >>= ifNothing convNotFound
  ensureConvMember (Data.convMembers conv) usr
  ensureAccess conv CodeAccess
  (bots, users) <- botsAndUsers $ Data.convMembers conv
  key <- mkKey cnv
  mCode <- Data.lookupCode key ReusableCode
  case mCode of
    Nothing -> do
      code <- generate cnv ReusableCode (Timeout 3600 * 24 * 365) -- one year TODO: configurable
      Data.insertCode code
      now <- liftIO getCurrentTime
      conversationCode <- createCode code
      let event = Event ConvCodeUpdate cnv usr now (EdConvCodeUpdate conversationCode)
      pushEvent event users bots zcon
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
rmCodeH (usr ::: zcon ::: cnv) = do
  setStatus status200 . json <$> rmCode usr zcon cnv

rmCode :: UserId -> ConnId -> ConvId -> Galley Public.Event
rmCode usr zcon cnv = do
  conv <- Data.conversation cnv >>= ifNothing convNotFound
  ensureConvMember (Data.convMembers conv) usr
  ensureAccess conv CodeAccess
  (bots, users) <- botsAndUsers $ Data.convMembers conv
  key <- mkKey cnv
  Data.deleteCode key ReusableCode
  now <- liftIO getCurrentTime
  let event = Event ConvCodeDelete cnv usr now EdConvCodeDelete
  pushEvent event users bots zcon
  pure event

getCodeH :: UserId ::: ConvId -> Galley Response
getCodeH (usr ::: cnv) = do
  setStatus status200 . json <$> getCode usr cnv

getCode :: UserId -> ConvId -> Galley Public.ConversationCode
getCode usr cnv = do
  conv <- Data.conversation cnv >>= ifNothing convNotFound
  ensureAccess conv CodeAccess
  ensureConvMember (Data.convMembers conv) usr
  key <- mkKey cnv
  c <- Data.lookupCode key ReusableCode >>= ifNothing codeNotFound
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
checkReusableCode convCode = do
  void $ verifyReusableCode convCode

verifyReusableCode :: ConversationCode -> Galley Code
verifyReusableCode convCode = do
  c <- Data.lookupCode (conversationKey convCode) ReusableCode >>= ifNothing codeNotFound
  unless (codeValue c == conversationCode convCode) $
    throwM codeNotFound
  return c

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
  conv <- Data.conversation cnv >>= ifNothing convNotFound
  ensureAccess conv access
  zusrMembership <- maybe (pure Nothing) (`Data.teamMember` zusr) (Data.convTeam conv)
  ensureAccessRole (Data.convAccessRole conv) [(zusr, zusrMembership)]
  let newUsers = filter (notIsMember conv) [zusr]
  -- FUTUREWORK: remote users?
  ensureMemberLimit (toList $ Data.convMembers conv) newUsers []
  -- NOTE: When joining conversations, all users become members
  -- as this is our desired behavior for these types of conversations
  -- where there is no way to control who joins, etc.
  mems <- botsAndUsers (Data.convMembers conv)
  addToConversation mems (zusr, roleNameWireMember) zcon ((,roleNameWireMember) <$> newUsers) [] conv

addMembersH :: UserId ::: ConnId ::: ConvId ::: JsonRequest Public.Invite -> Galley Response
addMembersH (zusr ::: zcon ::: cid ::: req) = do
  (Invite u r) <- fromJsonBody req
  domain <- viewFederationDomain
  let qInvite = Public.InviteQualified (flip Qualified domain <$> toNonEmpty u) r
  handleUpdateResult <$> addMembers zusr zcon cid qInvite

addMembersQH :: UserId -> ConnId -> ConvId -> Public.InviteQualified -> Galley (Union UpdateResponses)
addMembersQH zusr zcon convId invite = mapUpdateToServant =<< addMembers zusr zcon convId invite

mapUpdateToServant :: UpdateResult -> Galley (Union UpdateResponses)
mapUpdateToServant (Updated e) = Servant.respond $ WithStatus @200 e
mapUpdateToServant Unchanged = Servant.respond NoContent

-- FUTUREWORK(federation): Before 'addMembers' in its current form can be made
-- public by exposing 'addMembersToConversationV2' (currently hidden using /i/
-- prefix), i.e. by allowing remote members to be actually added in any environment,
-- we need the following checks/implementation:
--  - (1) Remote qualified users must exist before they can be added (a call to the
--  respective backend should be made): Avoid clients making up random Ids, and
--  increase the chances that the updateConversationMembership call suceeds
--  - (2) A call must be made to the remote backend informing it that this user is
--  now part of that conversation. Use and implement 'updateConversationMemberships'.
--    - that call should probably be made *after* inserting the conversation membership
--    happens in this backend.
--    - 'updateConversationMemberships' should send an event to the affected
--    users informing them they have joined a remote conversation.
--  - (3) Events should support remote / qualified users, too.
--  These checks need tests :)
addMembers :: UserId -> ConnId -> ConvId -> Public.InviteQualified -> Galley UpdateResult
addMembers zusr zcon convId invite = do
  conv <- Data.conversation convId >>= ifNothing convNotFound
  mems <- botsAndUsers (Data.convMembers conv)
  self <- getSelfMember zusr (snd mems)
  ensureActionAllowed AddConversationMember self
  let invitedUsers = toList $ Public.invQUsers invite
  domain <- viewFederationDomain
  let (invitedRemotes, invitedLocals) = partitionRemoteOrLocalIds' domain invitedUsers
  let newLocals = filter (notIsMember conv) invitedLocals
  let newRemotes = filter (notIsMember' conv) invitedRemotes
  ensureMemberLimit (toList $ Data.convMembers conv) newLocals newRemotes
  ensureAccess conv InviteAccess
  ensureConvRoleNotElevated self (invQRoleName invite)
  case Data.convTeam conv of
    Nothing -> do
      ensureAccessRole (Data.convAccessRole conv) (zip newLocals $ repeat Nothing)
      ensureConnectedOrSameTeam zusr newLocals
    Just ti -> teamConvChecks ti newLocals conv
  addToConversation mems (zusr, memConvRoleName self) zcon ((,invQRoleName invite) <$> newLocals) ((,invQRoleName invite) <$> newRemotes) conv
  where
    userIsMember u = (^. userId . to (== u))
    teamConvChecks tid newUsers conv = do
      tms <- Data.teamMembersLimited tid newUsers
      let userMembershipMap = map (\u -> (u, find (userIsMember u) tms)) newUsers
      ensureAccessRole (Data.convAccessRole conv) userMembershipMap
      tcv <- Data.teamConversation tid convId
      when (maybe True (view managedConversation) tcv) $
        throwM noAddToManaged
      ensureConnectedOrSameTeam zusr newUsers

updateSelfMemberH :: UserId ::: ConnId ::: ConvId ::: JsonRequest Public.MemberUpdate -> Galley Response
updateSelfMemberH (zusr ::: zcon ::: cid ::: req) = do
  update <- fromJsonBody req
  updateSelfMember zusr zcon cid update
  return empty

updateSelfMember :: UserId -> ConnId -> ConvId -> Public.MemberUpdate -> Galley ()
updateSelfMember zusr zcon cid update = do
  conv <- getConversationAndCheckMembership zusr cid
  m <- getSelfMember zusr (Data.convMembers conv)
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
  (bots, users) <- botsAndUsers (Data.convMembers conv)
  ensureActionAllowed ModifyOtherConversationMember =<< getSelfMember zusr users
  memTarget <- getOtherMember victim users
  e <- processUpdateMemberEvent zusr zcon cid users memTarget (memberUpdate {mupConvRoleName = omuConvRoleName update})
  void . forkIO $ void $ External.deliver (bots `zip` repeat e)

removeMemberH :: UserId ::: ConnId ::: ConvId ::: UserId -> Galley Response
removeMemberH (zusr ::: zcon ::: cid ::: victim) = do
  handleUpdateResult <$> removeMember zusr zcon cid victim

removeMember :: UserId -> ConnId -> ConvId -> UserId -> Galley UpdateResult
removeMember zusr zcon convId victim = do
  -- FUTUREWORK(federation, #1274): forward request to conversation's backend.
  conv <- Data.conversation convId >>= ifNothing convNotFound
  (bots, users) <- botsAndUsers (Data.convMembers conv)
  genConvChecks conv users
  case Data.convTeam conv of
    Nothing -> pure ()
    Just ti -> teamConvChecks ti
  if victim `isMember` users
    then do
      -- FUTUREWORK: deal with remote members, too, see removeMembers
      event <- Data.removeLocalMembers conv zusr (singleton victim)
      -- FUTUREWORK(federation, #1274): users can be on other backend, how to notify it?
      for_ (newPush ListComplete (evtFrom event) (ConvEvent event) (recipient <$> users)) $ \p ->
        push1 $ p & pushConn ?~ zcon
      void . forkIO $ void $ External.deliver (bots `zip` repeat event)
      pure $ Updated event
    else pure Unchanged
  where
    genConvChecks conv usrs = do
      ensureGroupConv conv
      if zusr == victim
        then ensureActionAllowed LeaveConversation =<< getSelfMember zusr usrs
        else ensureActionAllowed RemoveConversationMember =<< getSelfMember zusr usrs
    teamConvChecks tid = do
      tcv <- Data.teamConversation tid convId
      when (maybe False (view managedConversation) tcv) $
        throwM (invalidOp "Users can not be removed from managed conversations.")

-- OTR

data OtrResult
  = OtrSent !Public.ClientMismatch
  | OtrMissingRecipients !Public.ClientMismatch

handleOtrResult :: OtrResult -> Response
handleOtrResult = \case
  OtrSent m -> json m & setStatus status201
  OtrMissingRecipients m -> json m & setStatus status412

postBotMessageH :: BotId ::: ConvId ::: Public.OtrFilterMissing ::: JsonRequest Public.NewOtrMessage ::: JSON -> Galley Response
postBotMessageH (zbot ::: zcnv ::: val ::: req ::: _) = do
  message <- fromJsonBody req
  let val' = allowOtrFilterMissingInBody val message
  handleOtrResult <$> postBotMessage zbot zcnv val' message

postBotMessage :: BotId -> ConvId -> Public.OtrFilterMissing -> Public.NewOtrMessage -> Galley OtrResult
postBotMessage zbot zcnv val message = do
  postNewOtrMessage (botUserId zbot) Nothing zcnv val message

postProtoOtrMessageH :: UserId ::: ConnId ::: ConvId ::: Public.OtrFilterMissing ::: Request ::: Media "application" "x-protobuf" -> Galley Response
postProtoOtrMessageH (zusr ::: zcon ::: cnv ::: val ::: req ::: _) = do
  message <- Proto.toNewOtrMessage <$> fromProtoBody req
  let val' = allowOtrFilterMissingInBody val message
  handleOtrResult <$> postOtrMessage zusr zcon cnv val' message

postOtrMessageH :: UserId ::: ConnId ::: ConvId ::: Public.OtrFilterMissing ::: JsonRequest Public.NewOtrMessage -> Galley Response
postOtrMessageH (zusr ::: zcon ::: cnv ::: val ::: req) = do
  message <- fromJsonBody req
  let val' = allowOtrFilterMissingInBody val message
  handleOtrResult <$> postOtrMessage zusr zcon cnv val' message

postOtrMessage :: UserId -> ConnId -> ConvId -> Public.OtrFilterMissing -> Public.NewOtrMessage -> Galley OtrResult
postOtrMessage zusr zcon cnv val message =
  postNewOtrMessage zusr (Just zcon) cnv val message

postProtoOtrBroadcastH :: UserId ::: ConnId ::: Public.OtrFilterMissing ::: Request ::: JSON -> Galley Response
postProtoOtrBroadcastH (zusr ::: zcon ::: val ::: req ::: _) = do
  message <- Proto.toNewOtrMessage <$> fromProtoBody req
  let val' = allowOtrFilterMissingInBody val message
  handleOtrResult <$> postOtrBroadcast zusr zcon val' message

postOtrBroadcastH :: UserId ::: ConnId ::: Public.OtrFilterMissing ::: JsonRequest Public.NewOtrMessage -> Galley Response
postOtrBroadcastH (zusr ::: zcon ::: val ::: req) = do
  message <- fromJsonBody req
  let val' = allowOtrFilterMissingInBody val message
  handleOtrResult <$> postOtrBroadcast zusr zcon val' message

postOtrBroadcast :: UserId -> ConnId -> Public.OtrFilterMissing -> Public.NewOtrMessage -> Galley OtrResult
postOtrBroadcast zusr zcon val message =
  postNewOtrBroadcast zusr (Just zcon) val message

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
  let sender = newOtrSender msg
  let recvrs = newOtrRecipients msg
  now <- liftIO getCurrentTime
  withValidOtrBroadcastRecipients usr sender recvrs val now $ \rs -> do
    let (_, toUsers) = foldr (newMessage usr con Nothing msg now) ([], []) rs
    pushSome (catMaybes toUsers)

postNewOtrMessage :: UserId -> Maybe ConnId -> ConvId -> OtrFilterMissing -> NewOtrMessage -> Galley OtrResult
postNewOtrMessage usr con cnv val msg = do
  let sender = newOtrSender msg
  let recvrs = newOtrRecipients msg
  now <- liftIO getCurrentTime
  withValidOtrRecipients usr sender cnv recvrs val now $ \rs -> do
    let (toBots, toUsers) = foldr (newMessage usr con (Just cnv) msg now) ([], []) rs
    pushSome (catMaybes toUsers)
    void . forkIO $ do
      gone <- External.deliver toBots
      mapM_ (deleteBot cnv . botMemId) gone

newMessage ::
  UserId ->
  Maybe ConnId ->
  -- | Conversation Id (if Nothing, recipient's self conversation is used)
  Maybe ConvId ->
  NewOtrMessage ->
  UTCTime ->
  (LocalMember, ClientId, Text) ->
  ([(BotMember, Event)], [Maybe Push]) ->
  ([(BotMember, Event)], [Maybe Push])
newMessage usr con cnv msg now (m, c, t) ~(toBots, toUsers) =
  let o =
        OtrMessage
          { otrSender = newOtrSender msg,
            otrRecipient = c,
            otrCiphertext = t,
            otrData = newOtrData msg
          }
      -- use recipient's client's self conversation on broadcast
      -- (with federation, this might not work for remote members)
      conv = fromMaybe (selfConv $ memId m) cnv
      e = Event OtrMessageAdd conv usr now (EdOtrMessage o)
      r = recipient m & recipientClients .~ RecipientClientsSome (singleton c)
   in case newBotMember m of
        Just b -> ((b, e) : toBots, toUsers)
        Nothing ->
          let p =
                newPush ListComplete (evtFrom e) (ConvEvent e) [r]
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
  alive <- Data.isConvAlive cnv
  unless alive $ do
    Data.deleteConversation cnv
    throwM convNotFound
  (bots, users) <- botsAndUsers =<< Data.members cnv
  ensureActionAllowed ModifyConversationName =<< getSelfMember zusr users
  now <- liftIO getCurrentTime
  cn <- rangeChecked (cupName convRename)
  Data.updateConversation cnv cn
  let e = Event ConvRename cnv zusr now (EdConvRename convRename)
  for_ (newPush ListComplete (evtFrom e) (ConvEvent e) (recipient <$> users)) $ \p ->
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
  mm <- Data.members cnv
  unless (zusr `isMember` mm) $
    throwM convNotFound
  now <- liftIO getCurrentTime
  let e = Event Typing cnv zusr now (EdTyping typingData)
  for_ (newPush ListComplete (evtFrom e) (ConvEvent e) (recipient <$> mm)) $ \p ->
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
  c <- Data.conversation (b ^. addBotConv) >>= ifNothing convNotFound
  -- Check some preconditions on adding bots to a conversation
  for_ (Data.convTeam c) $ teamConvChecks (b ^. addBotConv)
  (bots, users) <- regularConvChecks c
  t <- liftIO getCurrentTime
  Data.updateClient True (botUserId (b ^. addBotId)) (b ^. addBotClient)
  (e, bm) <- Data.addBotMember zusr (b ^. addBotService) (b ^. addBotId) (b ^. addBotConv) t
  for_ (newPush ListComplete (evtFrom e) (ConvEvent e) (recipient <$> users)) $ \p ->
    push1 $ p & pushConn ?~ zcon
  void . forkIO $ void $ External.deliver ((bm : bots) `zip` repeat e)
  pure e
  where
    regularConvChecks c = do
      (bots, users) <- botsAndUsers (Data.convMembers c)
      unless (zusr `isMember` users) $
        throwM convNotFound
      ensureGroupConv c
      ensureActionAllowed AddConversationMember =<< getSelfMember zusr users
      unless (any ((== b ^. addBotId) . botMemId) bots) $
        ensureMemberLimit (toList $ Data.convMembers c) [botUserId (b ^. addBotId)] []
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
  c <- Data.conversation (b ^. rmBotConv) >>= ifNothing convNotFound
  unless (zusr `isMember` Data.convMembers c) $
    throwM convNotFound
  (bots, users) <- botsAndUsers (Data.convMembers c)
  if not (any ((== b ^. rmBotId) . botMemId) bots)
    then pure Unchanged
    else do
      t <- liftIO getCurrentTime
      let evd = EdMembersLeave (UserIdList [botUserId (b ^. rmBotId)])
      let e = Event MemberLeave (Data.convId c) zusr t evd
      for_ (newPush ListComplete (evtFrom e) (ConvEvent e) (recipient <$> users)) $ \p ->
        push1 $ p & pushConn .~ zcon
      Data.removeMember (botUserId (b ^. rmBotId)) (Data.convId c)
      Data.eraseClients (botUserId (b ^. rmBotId))
      void . forkIO $ void $ External.deliver (bots `zip` repeat e)
      pure $ Updated e

-------------------------------------------------------------------------------
-- Helpers

addToConversation :: ([BotMember], [LocalMember]) -> (UserId, RoleName) -> ConnId -> [(UserId, RoleName)] -> [(Remote UserId, RoleName)] -> Data.Conversation -> Galley UpdateResult
addToConversation _ _ _ [] [] _ = pure Unchanged
addToConversation (bots, others) (usr, usrRole) conn locals remotes c = do
  ensureGroupConv c
  mems <- checkedMemberAddSize locals remotes
  now <- liftIO getCurrentTime
  (e, mm, _remotes) <- Data.addMembersWithRole now (Data.convId c) (usr, usrRole) mems
  -- FUTUREWORK: send events to '_remotes' users here, too
  let allMembers = nubOrdOn memId (toList mm <> others)
  for_ (newPush ListComplete (evtFrom e) (ConvEvent e) (recipient <$> allMembers)) $ \p ->
    push1 $ p & pushConn ?~ conn
  void . forkIO $ void $ External.deliver (bots `zip` repeat e)
  pure $ Updated e

ensureGroupConv :: MonadThrow m => Data.Conversation -> m ()
ensureGroupConv c = case Data.convType c of
  SelfConv -> throwM invalidSelfOp
  One2OneConv -> throwM invalidOne2OneOp
  ConnectConv -> throwM invalidConnectOp
  _ -> return ()

ensureMemberLimit :: [LocalMember] -> [UserId] -> [Remote UserId] -> Galley ()
ensureMemberLimit old newLocals newRemotes = do
  o <- view options
  let maxSize = fromIntegral (o ^. optSettings . setMaxConvSize)
  when (length old + length newLocals + length newRemotes > maxSize) $
    throwM tooManyMembers

notIsMember :: Data.Conversation -> UserId -> Bool
notIsMember cc u = not $ isMember u (Data.convMembers cc)

notIsMember' :: Data.Conversation -> Remote UserId -> Bool
notIsMember' cc u = not $ isRemoteMember u (Data.convRemoteMembers cc)

ensureConvMember :: [LocalMember] -> UserId -> Galley ()
ensureConvMember users usr =
  unless (usr `isMember` users) $
    throwM convNotFound

ensureAccess :: Data.Conversation -> Access -> Galley ()
ensureAccess conv access =
  unless (access `elem` Data.convAccess conv) $
    throwM convAccessDenied

processUpdateMemberEvent ::
  UserId ->
  ConnId ->
  ConvId ->
  [LocalMember] ->
  LocalMember ->
  MemberUpdate ->
  Galley Event
processUpdateMemberEvent zusr zcon cid users target update = do
  up <- Data.updateMember cid (memId target) update
  now <- liftIO getCurrentTime
  let e = Event MemberStateUpdate cid zusr now (EdMemberUpdate up)
  let recipients = fmap recipient (target : filter ((/= memId target) . memId) users)
  for_ (newPush ListComplete (evtFrom e) (ConvEvent e) recipients) $ \p ->
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

withValidOtrBroadcastRecipients ::
  UserId ->
  ClientId ->
  OtrRecipients ->
  OtrFilterMissing ->
  UTCTime ->
  ([(LocalMember, ClientId, Text)] -> Galley ()) ->
  Galley OtrResult
withValidOtrBroadcastRecipients usr clt rcps val now go = Teams.withBindingTeam usr $ \tid -> do
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
  handleOtrResponse usr clt rcps membs clts val now go
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
  UserId ->
  ClientId ->
  ConvId ->
  OtrRecipients ->
  OtrFilterMissing ->
  UTCTime ->
  ([(LocalMember, ClientId, Text)] -> Galley ()) ->
  Galley OtrResult
withValidOtrRecipients usr clt cnv rcps val now go = do
  alive <- Data.isConvAlive cnv
  unless alive $ do
    Data.deleteConversation cnv
    throwM convNotFound
  -- FUTUREWORK(federation): also handle remote members
  localMembers <- Data.members cnv
  let localMemberIds = memId <$> localMembers
  isInternal <- view $ options . optSettings . setIntraListing
  clts <-
    if isInternal
      then Clients.fromUserClients <$> Intra.lookupClients localMemberIds
      else Data.lookupClients localMemberIds
  handleOtrResponse usr clt rcps localMembers clts val now go

handleOtrResponse ::
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
handleOtrResponse usr clt rcps membs clts val now go = case checkOtrRecipients usr clt rcps membs clts val now of
  ValidOtrRecipients m r -> go r >> pure (OtrSent m)
  MissingOtrRecipients m -> do
    guardLegalholdPolicyConflicts usr (missingClients m)
    pure (OtrMissingRecipients m)
  InvalidOtrSenderUser -> throwM convNotFound
  InvalidOtrSenderClient -> throwM unknownClient

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
        { cmismatchTime = now,
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

-- | If user has legalhold status `no_consent` or has client devices that have no legalhold
-- capability, and some of the clients she is about to get connected are LH devices, respond
-- with 412 and do not process notification.
--
-- This is a fallback safeguard that shouldn't get triggered if backend and clients work as
-- intended.
guardLegalholdPolicyConflicts :: UserId -> UserClients -> Galley ()
guardLegalholdPolicyConflicts self otherClients = do
  let otherCids :: [ClientId]
      otherCids = Set.toList . Set.unions . Map.elems . userClients $ otherClients

      otherUids :: [UserId]
      otherUids = nub $ Map.keys . userClients $ otherClients

  when (nub otherUids /= [self {- if all other clients belong to us, there can be no conflict -}]) $ do
    allClients :: UserClientsFull <- Intra.lookupClientsFull (nub $ self : otherUids)

    let selfClients :: [Client.Client] =
          allClients
            & Client.userClientsFull
            & Map.lookup self
            & fromMaybe Set.empty
            & Set.toList

        otherClientHasLH :: Bool
        otherClientHasLH =
          let clients =
                allClients
                  & Client.userClientsFull
                  & Map.delete self
                  & Map.elems
                  & Set.unions
                  & Set.toList
                  & filter ((`elem` otherCids) . Client.clientId)
           in Client.LegalHoldClientType `elem` (Client.clientType <$> clients)

        checkSelfHasLHClients :: Bool
        checkSelfHasLHClients =
          any ((== Client.LegalHoldClientType) . Client.clientType) selfClients

        checkSelfHasOldClients :: Bool
        checkSelfHasOldClients =
          any isOld selfClients
          where
            isOld :: Client.Client -> Bool
            isOld =
              (Client.ClientSupportsLegalholdImplicitConsent `Set.notMember`)
                . Client.fromClientCapabilityList
                . Client.clientCapabilities

        checkConsentMissing :: Galley Bool
        checkConsentMissing = do
          -- (we could also get the profile from brig.  would make the code slightly more
          -- concise, but not really help with the rpc back-and-forth, so, like, why?)
          mbUser <- accountUser <$$> getUser self
          mbTeamMember <- join <$> for (mbUser >>= userTeam) (`teamMember` self)
          let lhStatus = maybe defUserLegalHoldStatus (view legalHoldStatus) mbTeamMember
          pure (lhStatus == UserLegalHoldNoConsent)

    -- (I've tried to order the following checks for minimum IO; did it work?  ~~fisx)
    when otherClientHasLH $ do
      when checkSelfHasOldClients $
        throwM userLegalHoldNotSupported

      unless checkSelfHasLHClients {- carrying a LH device implies having granted LH consent -} $ do
        whenM checkConsentMissing $
          -- We assume this is impossible, since conversations are automatically
          -- blocked if LH consent is missing of any participant.
          -- We add this check here as an extra failsafe.
          throwM userLegalHoldNotSupported
