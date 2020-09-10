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
    updateSelfMemberH,
    updateOtherMemberH,
    removeMemberH,

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

import qualified Brig.Types.User as User
import Control.Lens
import Control.Monad.Catch
import Control.Monad.State
import Data.Code
import Data.Id
import qualified Data.Id as Id
import Data.IdMapping
import Data.List.Extra (nubOrdOn)
import Data.List.NonEmpty (nonEmpty)
import Data.List1
import qualified Data.Map.Strict as Map
import Data.Range
import qualified Data.Set as Set
import Data.Time
import Galley.API.Error
import qualified Galley.API.IdMapping as IdMapping
import Galley.API.Mapping
import qualified Galley.API.Teams as Teams
import Galley.API.Util
import Galley.App
import qualified Galley.Data as Data
import Galley.Data.Services as Data
import Galley.Data.Types hiding (Conversation)
import qualified Galley.External as External
import qualified Galley.Intra.Client as Intra
import Galley.Intra.Push
import Galley.Intra.User
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
import qualified Wire.API.Conversation as Public
import qualified Wire.API.Conversation.Code as Public
import qualified Wire.API.Event.Conversation as Public
import qualified Wire.API.Message as Public
import qualified Wire.API.Message.Proto as Proto

acceptConvH :: UserId ::: Maybe ConnId ::: ConvId -> Galley Response
acceptConvH (usr ::: conn ::: cnv) = do
  setStatus status200 . json <$> acceptConv usr conn cnv

acceptConv :: UserId -> Maybe ConnId -> ConvId -> Galley Conversation
acceptConv usr conn cnv = do
  conv <- Data.conversation cnv >>= ifNothing convNotFound
  conv' <- acceptOne2One usr conv conn
  conversationView (Local usr) conv'

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
  when (Local zusr `isMember` mems) $ Data.removeMember (Local zusr) cnv

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
  conversationView (Local usr) conv'

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
  [Member] ->
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
      let (localMemberIds, _) = partitionMappedOrLocalIds mIds
      activated <- fmap User.userId <$> lift (lookupActivatedUsers localMemberIds)
      let isActivatedOrRemote user = case memId user of
            Local l -> l `elem` activated
            Mapped _ -> True -- remote users don't need to be activated (we can't enforce it anyways)
      usersL %= filter isActivatedOrRemote
    -- In a team-only conversation we also want to remove bots and guests
    case (targetRole, Data.convTeam conv) of
      (TeamAccessRole, Just tid) -> do
        currentUsers <- use usersL
        onlyTeamUsers <- flip filterM currentUsers $ \user ->
          case memId user of
            Mapped _ -> pure False -- remote users can't be team members
            Local localId -> lift $ isJust <$> Data.teamMember tid localId
        assign usersL onlyTeamUsers
        botsL .= []
      _ -> return ()
  -- Update Cassandra & send an event
  now <- liftIO getCurrentTime
  let accessEvent = Event ConvAccessUpdate cnv usr now (Just $ EdConvAccessUpdate body)
  Data.updateConversationAccess cnv targetAccess targetRole
  pushEvent accessEvent users bots zcon
  -- Remove users and bots
  let removedUsers = map memId users \\ map memId newUsers
      removedBots = map botMemId bots \\ map botMemId newBots
  mapM_ (deleteBot cnv) removedBots
  case removedUsers of
    [] -> return ()
    x : xs -> do
      e <- Data.removeMembers conv usr (list1 x xs)
      -- push event to all clients, including zconn
      -- since updateConversationAccess generates a second (member removal) event here
      for_ (newPush ListComplete (evtFrom e) (ConvEvent e) (recipient <$> users)) $ \p -> push1 p
      void . forkIO $ void $ External.deliver (newBots `zip` repeat e)
  -- Return the event
  pure accessEvent
  where
    usersL :: Lens' ([Member], [BotMember]) [Member]
    usersL = _1
    botsL :: Lens' ([Member], [BotMember]) [BotMember]
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
      let receiptEvent = Event ConvReceiptModeUpdate cnv usr now (Just $ EdConvReceiptModeUpdate receiptModeUpdate)
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
      let timerEvent = Event ConvMessageTimerUpdate cnv usr now (Just $ EdConvMessageTimerUpdate timerUpdate)
      Data.updateConversationMessageTimer cnv target
      pushEvent timerEvent users bots zcon
      pure timerEvent

pushEvent :: Event -> [Member] -> [BotMember] -> ConnId -> Galley ()
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
      let event = Event ConvCodeUpdate cnv usr now (Just $ EdConvCodeUpdate conversationCode)
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
  let event = Event ConvCodeDelete cnv usr now Nothing
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
  let newUsers = filter (notIsMember conv . Local) [zusr]
  ensureMemberLimit (toList $ Data.convMembers conv) (Local <$> newUsers)
  -- NOTE: When joining conversations, all users become members
  -- as this is our desired behavior for these types of conversations
  -- where there is no way to control who joins, etc.
  mems <- botsAndUsers (Data.convMembers conv)
  addToConversation mems (zusr, roleNameWireMember) zcon ((,roleNameWireMember) <$> newUsers) conv

addMembersH :: UserId ::: ConnId ::: OpaqueConvId ::: JsonRequest Public.Invite -> Galley Response
addMembersH (zusr ::: zcon ::: cid ::: req) = do
  invite <- fromJsonBody req
  handleUpdateResult <$> addMembers zusr zcon cid invite

addMembers :: UserId -> ConnId -> OpaqueConvId -> Public.Invite -> Galley UpdateResult
addMembers zusr zcon cid invite = do
  IdMapping.resolveOpaqueConvId cid >>= \case
    Mapped idMapping ->
      -- FUTUREWORK(federation): if the conversation is on another backend, send request there.
      -- in the case of a non-team conversation, we need to think about `ensureConnectedOrSameTeam`,
      -- specifically whether teams from another backend than the conversation should have any
      -- relevance here.
      throwM . federationNotImplemented $ pure idMapping
    Local localConvId ->
      addMembersToLocalConv localConvId
  where
    addMembersToLocalConv convId = do
      conv <- Data.conversation convId >>= ifNothing convNotFound
      mems <- botsAndUsers (Data.convMembers conv)
      self <- getSelfMember zusr (snd mems)
      ensureActionAllowed AddConversationMember self
      invitedUsers <- traverse IdMapping.resolveOpaqueUserId (toList $ invUsers invite)
      toAdd <- fromMemberSize <$> checkedMemberAddSize invitedUsers
      let newUsers = filter (notIsMember conv) (toList toAdd)
      ensureMemberLimit (toList $ Data.convMembers conv) newUsers
      ensureAccess conv InviteAccess
      ensureConvRoleNotElevated self (invRoleName invite)
      let (newLocalUsers, newQualifiedUsers) = partitionMappedOrLocalIds newUsers
      -- FUTUREWORK(federation): allow adding remote members
      -- this one is a bit tricky because all of the checks that need to be done,
      -- some of them on remote backends.
      for_ (nonEmpty newQualifiedUsers) $
        throwM . federationNotImplemented
      case Data.convTeam conv of
        Nothing -> do
          ensureAccessRole (Data.convAccessRole conv) (zip newLocalUsers $ repeat Nothing)
          ensureConnectedOrSameTeam zusr newLocalUsers
        Just ti -> teamConvChecks ti newLocalUsers convId conv
      addToConversation mems (zusr, memConvRoleName self) zcon ((,invRoleName invite) <$> newLocalUsers) conv
    userIsMember u = (^. userId . to (== u))
    teamConvChecks tid newLocalUsers convId conv = do
      tms <- Data.teamMembersLimited tid newLocalUsers
      let userMembershipMap = map (\u -> (u, find (userIsMember u) tms)) newLocalUsers
      ensureAccessRole (Data.convAccessRole conv) userMembershipMap
      tcv <- Data.teamConversation tid convId
      when (maybe True (view managedConversation) tcv) $
        throwM noAddToManaged
      ensureConnectedOrSameTeam zusr newLocalUsers

updateSelfMemberH :: UserId ::: ConnId ::: ConvId ::: JsonRequest Public.MemberUpdate -> Galley Response
updateSelfMemberH (zusr ::: zcon ::: cid ::: req) = do
  update <- fromJsonBody req
  updateSelfMember zusr zcon cid update
  return empty

updateSelfMember :: UserId -> ConnId -> ConvId -> Public.MemberUpdate -> Galley ()
updateSelfMember zusr zcon cid update = do
  conv <- getConversationAndCheckMembership zusr (Local cid)
  m <- getSelfMember zusr (Data.convMembers conv)
  -- Ensure no self role upgrades
  for_ (mupConvRoleName update) $ ensureConvRoleNotElevated m
  void $ processUpdateMemberEvent zusr zcon cid [Local <$> m] m update

updateOtherMemberH :: UserId ::: ConnId ::: ConvId ::: UserId ::: JsonRequest Public.OtherMemberUpdate -> Galley Response
updateOtherMemberH (zusr ::: zcon ::: cid ::: victim ::: req) = do
  update <- fromJsonBody req
  updateOtherMember zusr zcon cid victim update
  return empty

updateOtherMember :: UserId -> ConnId -> ConvId -> UserId -> Public.OtherMemberUpdate -> Galley ()
updateOtherMember zusr zcon cid victim update = do
  when (zusr == victim) $
    throwM invalidTargetUserOp
  conv <- getConversationAndCheckMembership zusr (Local cid)
  (bots, users) <- botsAndUsers (Data.convMembers conv)
  ensureActionAllowed ModifyOtherConversationMember =<< getSelfMember zusr users
  memTarget <- getOtherMember victim users
  e <- processUpdateMemberEvent zusr zcon cid users memTarget (memberUpdate {mupConvRoleName = omuConvRoleName update})
  void . forkIO $ void $ External.deliver (bots `zip` repeat e)

removeMemberH :: UserId ::: ConnId ::: OpaqueConvId ::: OpaqueUserId -> Galley Response
removeMemberH (zusr ::: zcon ::: cid ::: victim) = do
  handleUpdateResult <$> removeMember zusr zcon cid victim

removeMember :: UserId -> ConnId -> OpaqueConvId -> OpaqueUserId -> Galley UpdateResult
removeMember zusr zcon cid opaqueVictim = do
  IdMapping.resolveOpaqueConvId cid >>= \case
    Mapped idMapping ->
      -- FUTUREWORK(federation, #1274): forward request to conversation's backend.
      throwM . federationNotImplemented $ pure idMapping
    Local localConvId -> do
      victim <- IdMapping.resolveOpaqueUserId opaqueVictim
      removeMemberOfLocalConversation localConvId victim
  where
    removeMemberOfLocalConversation convId victim = do
      conv <- Data.conversation convId >>= ifNothing convNotFound
      (bots, users) <- botsAndUsers (Data.convMembers conv)
      genConvChecks conv users victim
      case Data.convTeam conv of
        Nothing -> pure ()
        Just ti -> teamConvChecks convId ti
      if victim `isMember` users
        then do
          event <- Data.removeMembers conv zusr (singleton victim)
          case victim of
            Local _ -> pure () -- nothing to do
            Mapped _ -> do
              -- FUTUREWORK(federation, #1274): users can be on other backend, how to notify it?
              pure ()
          for_ (newPush ListComplete (evtFrom event) (ConvEvent event) (recipient <$> users)) $ \p ->
            push1 $ p & pushConn ?~ zcon
          void . forkIO $ void $ External.deliver (bots `zip` repeat event)
          pure $ Updated event
        else pure Unchanged
    genConvChecks conv usrs victim = do
      ensureGroupConv conv
      if Local zusr == victim
        then ensureActionAllowed LeaveConversation =<< getSelfMember zusr usrs
        else ensureActionAllowed RemoveConversationMember =<< getSelfMember zusr usrs
    teamConvChecks convId tid = do
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
  postNewOtrMessage (botUserId zbot) Nothing (makeIdOpaque zcnv) val message

postProtoOtrMessageH :: UserId ::: ConnId ::: OpaqueConvId ::: Public.OtrFilterMissing ::: Request ::: Media "application" "x-protobuf" -> Galley Response
postProtoOtrMessageH (zusr ::: zcon ::: cnv ::: val ::: req ::: _) = do
  message <- Proto.toNewOtrMessage <$> fromProtoBody req
  let val' = allowOtrFilterMissingInBody val message
  handleOtrResult <$> postOtrMessage zusr zcon cnv val' message

postOtrMessageH :: UserId ::: ConnId ::: OpaqueConvId ::: Public.OtrFilterMissing ::: JsonRequest Public.NewOtrMessage -> Galley Response
postOtrMessageH (zusr ::: zcon ::: cnv ::: val ::: req) = do
  message <- fromJsonBody req
  let val' = allowOtrFilterMissingInBody val message
  handleOtrResult <$> postOtrMessage zusr zcon cnv val' message

postOtrMessage :: UserId -> ConnId -> OpaqueConvId -> Public.OtrFilterMissing -> Public.NewOtrMessage -> Galley OtrResult
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

postNewOtrMessage :: UserId -> Maybe ConnId -> OpaqueConvId -> OtrFilterMissing -> NewOtrMessage -> Galley OtrResult
postNewOtrMessage usr con cnv val msg = do
  IdMapping.resolveOpaqueConvId cnv >>= \case
    Mapped idMapping ->
      -- FUTUREWORK(federation, #1261): forward message to backend owning the conversation
      throwM . federationNotImplemented $ pure idMapping
    Local localConvId ->
      postToLocalConv localConvId
  where
    postToLocalConv localConvId = do
      let sender = newOtrSender msg
      let recvrs = newOtrRecipients msg
      now <- liftIO getCurrentTime
      withValidOtrRecipients usr sender localConvId recvrs val now $ \rs -> do
        let (toBots, toUsers) = foldr (newMessage usr con (Just localConvId) msg now) ([], []) rs
        pushSome (catMaybes toUsers)
        void . forkIO $ do
          gone <- External.deliver toBots
          mapM_ (deleteBot localConvId . botMemId) gone

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
      e = Event OtrMessageAdd conv usr now (Just $ EdOtrMessage o)
      r = recipient (Local <$> m) & recipientClients .~ (RecipientClientsSome $ singleton c)
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
  let e = Event ConvRename cnv zusr now (Just $ EdConvRename convRename)
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
  unless (Local zusr `isMember` mm) $
    throwM convNotFound
  now <- liftIO getCurrentTime
  let e = Event Typing cnv zusr now (Just $ EdTyping typingData)
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
      unless (Local zusr `isMember` users) $
        throwM convNotFound
      ensureGroupConv c
      ensureActionAllowed AddConversationMember =<< getSelfMember zusr users
      unless (any ((== b ^. addBotId) . botMemId) bots) $
        ensureMemberLimit (toList $ Data.convMembers c) [Local (botUserId (b ^. addBotId))]
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
  unless (Local zusr `isMember` Data.convMembers c) $
    throwM convNotFound
  (bots, users) <- botsAndUsers (Data.convMembers c)
  if not (any ((== b ^. rmBotId) . botMemId) bots)
    then pure Unchanged
    else do
      t <- liftIO getCurrentTime
      let evd = Just (EdMembersLeave (UserIdList [botUserId (b ^. rmBotId)]))
      let e = Event MemberLeave (Data.convId c) zusr t evd
      for_ (newPush ListComplete (evtFrom e) (ConvEvent e) (recipient <$> users)) $ \p ->
        push1 $ p & pushConn .~ zcon
      Data.removeMember (Local (botUserId (b ^. rmBotId))) (Data.convId c)
      Data.eraseClients (botUserId (b ^. rmBotId))
      void . forkIO $ void $ External.deliver (bots `zip` repeat e)
      pure $ Updated e

-------------------------------------------------------------------------------
-- Helpers

addToConversation :: ([BotMember], [Member]) -> (UserId, RoleName) -> ConnId -> [(UserId, RoleName)] -> Data.Conversation -> Galley UpdateResult
addToConversation _ _ _ [] _ = pure Unchanged
addToConversation (bots, others) (usr, usrRole) conn xs c = do
  ensureGroupConv c
  mems <- checkedMemberAddSize xs
  now <- liftIO getCurrentTime
  (e, mm) <- Data.addMembersWithRole now (Data.convId c) (usr, usrRole) mems
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

ensureMemberLimit :: [Member] -> [MappedOrLocalId Id.U] -> Galley ()
ensureMemberLimit old new = do
  o <- view options
  let maxSize = fromIntegral (o ^. optSettings . setMaxConvSize)
  when (length old + length new > maxSize) $
    throwM tooManyMembers

notIsMember :: Data.Conversation -> MappedOrLocalId Id.U -> Bool
notIsMember cc u = not $ isMember u (Data.convMembers cc)

ensureConvMember :: [Member] -> UserId -> Galley ()
ensureConvMember users usr =
  unless (Local usr `isMember` users) $
    throwM convNotFound

ensureAccess :: Data.Conversation -> Access -> Galley ()
ensureAccess conv access =
  unless (access `elem` Data.convAccess conv) $
    throwM convAccessDenied

processUpdateMemberEvent ::
  UserId ->
  ConnId ->
  ConvId ->
  [Member] ->
  LocalMember ->
  MemberUpdate ->
  Galley Event
processUpdateMemberEvent zusr zcon cid users target update = do
  up <- Data.updateMember cid (memId target) update
  now <- liftIO getCurrentTime
  let e = Event MemberStateUpdate cid zusr now (Just $ EdMemberUpdate up)
  let recipients = fmap recipient ((Local <$> target) : filter ((/= Local (memId target)) . memId) users)
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
  unless ((Map.size $ userClientMap (otrRecipientsMap rcps)) <= limit) $
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
      (localUserIdsInFilter, _remoteUserIdsInFilter) <- partitionMappedOrLocalIds <$> traverse IdMapping.resolveOpaqueUserId (toList uListInFilter)
      -- Get the users in the recipient list (remote ids are not in a local team)
      (localUserIdsInRcps, _remoteUserIdsInRcps) <- partitionMappedOrLocalIds <$> traverse IdMapping.resolveOpaqueUserId (Map.keys $ userClientMap (otrRecipientsMap rcps))
      -- Put them in a single list, and ensure it's smaller than the max size
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
  membs <- Data.members cnv
  let localMembers = flip mapMaybe membs $ \memb ->
        case memId memb of
          Local localId -> Just (memb {memId = localId} :: LocalMember)
          Mapped _ -> Nothing
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
  MissingOtrRecipients m -> pure (OtrMissingRecipients m)
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
checkOtrRecipients (makeIdOpaque -> usr) sid prs vms vcs val now
  | not (Map.member usr vmembers) = InvalidOtrSenderUser
  | not (Clients.contains usr sid vcs) = InvalidOtrSenderClient
  | not (Clients.null missing) = MissingOtrRecipients mismatch
  | otherwise = ValidOtrRecipients mismatch yield
  where
    yield = foldrOtrRecipients next [] prs
    next u c t rs
      | Just m <- member u c = (m, c, t) : rs
      | otherwise = rs
    member :: OpaqueUserId -> ClientId -> Maybe LocalMember
    member u c
      | Just m <- Map.lookup u vmembers,
        Clients.contains u c vclients =
        Just m
      | otherwise = Nothing
    -- Valid recipient members & clients
    vmembers = Map.fromList $ map (\m -> (makeIdOpaque (memId m), m)) vms
    vclients = Clients.rmClient usr sid vcs
    -- Proposed (given) recipients
    recipients = userClientMap (otrRecipientsMap prs)
    given = Clients.fromMap (Map.map Map.keysSet recipients)
    -- Differences between valid and proposed recipients
    missing = filterMissing (Clients.diff vclients given)
    unknown = Clients.diff given vcs
    deleted = Clients.filter (`Map.member` vmembers) unknown
    redundant =
      Clients.diff unknown deleted
        & if Clients.contains usr sid given
          then Clients.insert usr sid
          else id
    mismatch =
      ClientMismatch
        { cmismatchTime = now,
          missingClients = UserClients (Clients.toMap missing),
          redundantClients = UserClients (Clients.toMap redundant),
          deletedClients = UserClients (Clients.toMap deleted)
        }
    filterMissing miss = case val of
      OtrReportAllMissing -> miss
      OtrIgnoreAllMissing -> Clients.nil
      OtrReportMissing us -> Clients.filter (`Set.member` us) miss
      OtrIgnoreMissing us -> Clients.filter (`Set.notMember` us) miss
