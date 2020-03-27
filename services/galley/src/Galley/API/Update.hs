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
import Data.IdMapping
import Data.List (delete)
import Data.List.NonEmpty (nonEmpty)
import Data.List1
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time
import Galley.API.Error
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
import qualified Galley.Types.Proto as Proto
import Galley.Types.Teams hiding (Event, EventData (..), EventType (..), self)
import Galley.Validation
import Gundeck.Types.Push.V2 (RecipientClients (..))
import Imports
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (_1, _2, failure, setStatus)
import Network.Wai.Utilities

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
  unless (Data.convType conv `elem` [ConnectConv, One2OneConv])
    $ throwM
    $ invalidOp "block: invalid conversation type"
  let mems = Data.convMembers conv
  when (makeIdOpaque zusr `isMember` mems) $ Data.removeMember zusr cnv

unblockConvH :: UserId ::: Maybe ConnId ::: ConvId -> Galley Response
unblockConvH (usr ::: conn ::: cnv) = do
  setStatus status200 . json <$> unblockConv usr conn cnv

unblockConv :: UserId -> Maybe ConnId -> ConvId -> Galley Conversation
unblockConv usr conn cnv = do
  conv <- Data.conversation cnv >>= ifNothing convNotFound
  unless (Data.convType conv `elem` [ConnectConv, One2OneConv])
    $ throwM
    $ invalidOp "unblock: invalid conversation type"
  conv' <- acceptOne2One usr conv conn
  conversationView usr conv'

-- conversation updates

data UpdateResult
  = Updated Event
  | Unchanged

handleUpdateResult :: UpdateResult -> Response
handleUpdateResult = \case
  Updated ev -> json ev & setStatus status200
  Unchanged -> empty & setStatus status204

updateConversationAccessH :: UserId ::: ConnId ::: ConvId ::: JsonRequest ConversationAccessUpdate -> Galley Response
updateConversationAccessH (usr ::: zcon ::: cnv ::: req) = do
  update <- fromJsonBody req
  handleUpdateResult <$> updateConversationAccess usr zcon cnv update

updateConversationAccess :: UserId -> ConnId -> ConvId -> ConversationAccessUpdate -> Galley UpdateResult
updateConversationAccess usr zcon cnv update = do
  let targetAccess = Set.fromList (toList (cupAccess update))
      targetRole = cupAccessRole update
  -- 'PrivateAccessRole' is for self-conversations, 1:1 conversations and
  -- so on; users are not supposed to be able to make other conversations
  -- have 'PrivateAccessRole'
  when (PrivateAccess `elem` targetAccess || PrivateAccessRole == targetRole) $
    throwM invalidTargetAccess
  -- The user who initiated access change has to be a conversation member
  (bots, users) <- botsAndUsers <$> Data.members cnv
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
      activated <- fmap User.userId <$> lift (lookupActivatedUsers mIds)
      usersL %= filter (\user -> memId user `elem` activated)
    -- In a team-only conversation we also want to remove bots and guests
    case (targetRole, Data.convTeam conv) of
      (TeamAccessRole, Just tid) -> do
        currentUsers <- use usersL
        onlyTeamUsers <- filterM (\user -> lift $ isJust <$> Data.teamMember tid (memId user)) currentUsers
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
      e <- Data.removeMembers conv usr (Local <$> list1 x xs)
      -- push event to all clients, including zconn
      -- since updateConversationAccess generates a second (member removal) event here
      for_ (newPush (evtFrom e) (ConvEvent e) (recipient <$> users)) $ \p -> push1 p
      void . forkIO $ void $ External.deliver (newBots `zip` repeat e)
  -- Return the event
  pure accessEvent
  where
    usersL :: Lens' ([Member], [BotMember]) [Member]
    usersL = _1
    botsL :: Lens' ([Member], [BotMember]) [BotMember]
    botsL = _2

updateConversationReceiptModeH :: UserId ::: ConnId ::: ConvId ::: JsonRequest ConversationReceiptModeUpdate ::: JSON -> Galley Response
updateConversationReceiptModeH (usr ::: zcon ::: cnv ::: req ::: _) = do
  update <- fromJsonBody req
  handleUpdateResult <$> updateConversationReceiptMode usr zcon cnv update

updateConversationReceiptMode :: UserId -> ConnId -> ConvId -> ConversationReceiptModeUpdate -> Galley UpdateResult
updateConversationReceiptMode usr zcon cnv receiptModeUpdate@(ConversationReceiptModeUpdate target) = do
  (bots, users) <- botsAndUsers <$> Data.members cnv
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

updateConversationMessageTimerH :: UserId ::: ConnId ::: ConvId ::: JsonRequest ConversationMessageTimerUpdate -> Galley Response
updateConversationMessageTimerH (usr ::: zcon ::: cnv ::: req) = do
  timerUpdate <- fromJsonBody req
  handleUpdateResult <$> updateConversationMessageTimer usr zcon cnv timerUpdate

updateConversationMessageTimer :: UserId -> ConnId -> ConvId -> ConversationMessageTimerUpdate -> Galley UpdateResult
updateConversationMessageTimer usr zcon cnv timerUpdate@(ConversationMessageTimerUpdate target) = do
  -- checks and balances
  (bots, users) <- botsAndUsers <$> Data.members cnv
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
  for_ (newPush (evtFrom e) (ConvEvent e) (recipient <$> users)) $ \p ->
    push1 $ p & pushConn ?~ zcon
  void . forkIO $ void $ External.deliver (bots `zip` repeat e)

addCodeH :: UserId ::: ConnId ::: ConvId -> Galley Response
addCodeH (usr ::: zcon ::: cnv) = do
  addCode usr zcon cnv <&> \case
    CodeAdded event -> json event & setStatus status201
    CodeAlreadyExisted conversationCode -> json conversationCode & setStatus status200

data AddCodeResult
  = CodeAdded Event
  | CodeAlreadyExisted ConversationCode

addCode :: UserId -> ConnId -> ConvId -> Galley AddCodeResult
addCode usr zcon cnv = do
  conv <- Data.conversation cnv >>= ifNothing convNotFound
  ensureConvMember (Data.convMembers conv) usr
  ensureAccess conv CodeAccess
  let (bots, users) = botsAndUsers $ Data.convMembers conv
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

rmCode :: UserId -> ConnId -> ConvId -> Galley Event
rmCode usr zcon cnv = do
  conv <- Data.conversation cnv >>= ifNothing convNotFound
  ensureConvMember (Data.convMembers conv) usr
  ensureAccess conv CodeAccess
  let (bots, users) = botsAndUsers $ Data.convMembers conv
  key <- mkKey cnv
  Data.deleteCode key ReusableCode
  now <- liftIO getCurrentTime
  let event = Event ConvCodeDelete cnv usr now Nothing
  pushEvent event users bots zcon
  pure event

getCodeH :: UserId ::: ConvId -> Galley Response
getCodeH (usr ::: cnv) = do
  setStatus status200 . json <$> getCode usr cnv

getCode :: UserId -> ConvId -> Galley ConversationCode
getCode usr cnv = do
  conv <- Data.conversation cnv >>= ifNothing convNotFound
  ensureAccess conv CodeAccess
  ensureConvMember (Data.convMembers conv) usr
  key <- mkKey cnv
  c <- Data.lookupCode key ReusableCode >>= ifNothing codeNotFound
  returnCode c

returnCode :: Code -> Galley ConversationCode
returnCode c = do
  urlPrefix <- view $ options . optSettings . setConversationCodeURI
  pure $ mkConversationCode (codeKey c) (codeValue c) urlPrefix

checkReusableCodeH :: JsonRequest ConversationCode -> Galley Response
checkReusableCodeH req = do
  convCode <- fromJsonBody req
  checkReusableCode convCode
  pure empty

checkReusableCode :: ConversationCode -> Galley ()
checkReusableCode convCode = do
  void $ verifyReusableCode convCode

verifyReusableCode :: ConversationCode -> Galley Code
verifyReusableCode convCode = do
  c <- Data.lookupCode (conversationKey convCode) ReusableCode >>= ifNothing codeNotFound
  unless (codeValue c == conversationCode convCode) $
    throwM codeNotFound
  return c

joinConversationByReusableCodeH :: UserId ::: ConnId ::: JsonRequest ConversationCode -> Galley Response
joinConversationByReusableCodeH (zusr ::: zcon ::: req) = do
  convCode <- fromJsonBody req
  handleUpdateResult <$> joinConversationByReusableCode zusr zcon convCode

joinConversationByReusableCode :: UserId -> ConnId -> ConversationCode -> Galley UpdateResult
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
  let newUsers = filter (notIsMember conv . makeIdOpaque) [zusr]
  ensureMemberLimit (toList $ Data.convMembers conv) (makeIdOpaque <$> newUsers)
  -- NOTE: When joining conversations, all users become members
  -- as this is our desired behavior for these types of conversations
  -- where there is no way to control who joins, etc.
  addToConversation (botsAndUsers (Data.convMembers conv)) (zusr, roleNameWireMember) zcon ((,roleNameWireMember) <$> newUsers) conv

addMembersH :: UserId ::: ConnId ::: OpaqueConvId ::: JsonRequest Invite -> Galley Response
addMembersH (zusr ::: zcon ::: cid ::: req) = do
  invite <- fromJsonBody req
  handleUpdateResult <$> addMembers zusr zcon cid invite

addMembers :: UserId -> ConnId -> OpaqueConvId -> Invite -> Galley UpdateResult
addMembers zusr zcon cid invite = do
  resolveOpaqueConvId cid >>= \case
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
      let mems = botsAndUsers (Data.convMembers conv)
      self <- getSelfMember zusr (snd mems)
      ensureActionAllowed AddConversationMember self
      toAdd <- fromMemberSize <$> checkedMemberAddSize (toList $ invUsers invite)
      let newOpaqueUsers = filter (notIsMember conv) (toList toAdd)
      ensureMemberLimit (toList $ Data.convMembers conv) newOpaqueUsers
      ensureAccess conv InviteAccess
      ensureConvRoleNotElevated self (invRoleName invite)
      (newUsers, newQualifiedUsers) <- partitionMappedOrLocalIds <$> traverse resolveOpaqueUserId newOpaqueUsers
      -- FUTUREWORK(federation): allow adding remote members
      -- this one is a bit tricky because all of the checks that need to be done,
      -- some of them on remote backends.
      for_ (nonEmpty newQualifiedUsers) $
        throwM . federationNotImplemented
      case Data.convTeam conv of
        Nothing -> do
          ensureAccessRole (Data.convAccessRole conv) (zip newUsers $ repeat Nothing)
          ensureConnectedOrSameTeam zusr newUsers
        Just ti -> teamConvChecks ti newUsers convId conv
      addToConversation mems (zusr, memConvRoleName self) zcon ((,invRoleName invite) <$> newUsers) conv
    userIsMember u = (^. userId . to (== u))
    teamConvChecks tid newUsers convId conv = do
      tms <- Data.teamMembersLimited tid newUsers
      let userMembershipMap = map (\u -> (u, find (userIsMember u) tms)) newUsers
      ensureAccessRole (Data.convAccessRole conv) userMembershipMap
      tcv <- Data.teamConversation tid convId
      when (maybe True (view managedConversation) tcv) $
        throwM noAddToManaged
      ensureConnectedOrSameTeam zusr newUsers

updateSelfMemberH :: UserId ::: ConnId ::: ConvId ::: JsonRequest MemberUpdate -> Galley Response
updateSelfMemberH (zusr ::: zcon ::: cid ::: req) = do
  update <- fromJsonBody req
  updateSelfMember zusr zcon cid update
  return empty

updateSelfMember :: UserId -> ConnId -> ConvId -> MemberUpdate -> Galley ()
updateSelfMember zusr zcon cid update = do
  conv <- getConversationAndCheckMembership zusr (Local cid)
  m <- getSelfMember zusr (Data.convMembers conv)
  -- Ensure no self role upgrades
  for_ (mupConvRoleName update) $ ensureConvRoleNotElevated m
  void $ processUpdateMemberEvent zusr zcon cid [m] m update

updateOtherMemberH :: UserId ::: ConnId ::: ConvId ::: UserId ::: JsonRequest OtherMemberUpdate -> Galley Response
updateOtherMemberH (zusr ::: zcon ::: cid ::: victim ::: req) = do
  update <- fromJsonBody req
  updateOtherMember zusr zcon cid victim update
  return empty

updateOtherMember :: UserId -> ConnId -> ConvId -> UserId -> OtherMemberUpdate -> Galley ()
updateOtherMember zusr zcon cid victim update = do
  when (zusr == victim) $
    throwM invalidTargetUserOp
  conv <- getConversationAndCheckMembership zusr (Local cid)
  let (bots, users) = botsAndUsers (Data.convMembers conv)
  ensureActionAllowed ModifyOtherConversationMember =<< getSelfMember zusr users
  memTarget <- getOtherMember victim users
  e <- processUpdateMemberEvent zusr zcon cid users memTarget (memberUpdate {mupConvRoleName = omuConvRoleName update})
  void . forkIO $ void $ External.deliver (bots `zip` repeat e)

removeMemberH :: UserId ::: ConnId ::: OpaqueConvId ::: OpaqueUserId -> Galley Response
removeMemberH (zusr ::: zcon ::: cid ::: victim) = do
  handleUpdateResult <$> removeMember zusr zcon cid victim

removeMember :: UserId -> ConnId -> OpaqueConvId -> OpaqueUserId -> Galley UpdateResult
removeMember zusr zcon cid victim = do
  resolveOpaqueConvId cid >>= \case
    Mapped idMapping -> throwM . federationNotImplemented $ pure idMapping
    Local localConvId -> removeMemberOfLocalConversation localConvId
  where
    removeMemberOfLocalConversation convId = do
      conv <- Data.conversation convId >>= ifNothing convNotFound
      let (bots, users) = botsAndUsers (Data.convMembers conv)
      genConvChecks conv users
      case Data.convTeam conv of
        Nothing -> pure ()
        Just ti -> teamConvChecks convId ti
      if victim `isMember` users
        then do
          resolvedVictim <- resolveOpaqueUserId victim
          event <- Data.removeMembers conv zusr (singleton resolvedVictim)
          case resolvedVictim of
            Local _ -> pure () -- nothing to do
            Mapped _ -> do
              -- FUTUREWORK(federation): users can be on other backend, how to notify it?
              pure ()
          for_ (newPush (evtFrom event) (ConvEvent event) (recipient <$> users)) $ \p ->
            push1 $ p & pushConn ?~ zcon
          void . forkIO $ void $ External.deliver (bots `zip` repeat event)
          pure $ Updated event
        else pure Unchanged
    genConvChecks conv usrs = do
      ensureGroupConv conv
      if makeIdOpaque zusr == victim
        then ensureActionAllowed LeaveConversation =<< getSelfMember zusr usrs
        else ensureActionAllowed RemoveConversationMember =<< getSelfMember zusr usrs
    teamConvChecks convId tid = do
      tcv <- Data.teamConversation tid convId
      when (maybe False (view managedConversation) tcv) $
        throwM (invalidOp "Users can not be removed from managed conversations.")

-- OTR

data OtrResult
  = OtrSent !ClientMismatch
  | OtrMissingRecipients !ClientMismatch

handleOtrResult :: OtrResult -> Response
handleOtrResult = \case
  OtrSent m -> json m & setStatus status201
  OtrMissingRecipients m -> json m & setStatus status412

postBotMessageH :: BotId ::: ConvId ::: OtrFilterMissing ::: JsonRequest NewOtrMessage ::: JSON -> Galley Response
postBotMessageH (zbot ::: zcnv ::: val ::: req ::: _) = do
  message <- fromJsonBody req
  handleOtrResult <$> postBotMessage zbot zcnv val message

postBotMessage :: BotId -> ConvId -> OtrFilterMissing -> NewOtrMessage -> Galley OtrResult
postBotMessage zbot zcnv val message = do
  postNewOtrMessage (botUserId zbot) Nothing (makeIdOpaque zcnv) val message

postProtoOtrMessageH :: UserId ::: ConnId ::: OpaqueConvId ::: OtrFilterMissing ::: Request ::: Media "application" "x-protobuf" -> Galley Response
postProtoOtrMessageH (zusr ::: zcon ::: cnv ::: val ::: req ::: _) = do
  message <- Proto.toNewOtrMessage <$> fromProtoBody req
  handleOtrResult <$> postOtrMessage zusr zcon cnv val message

postOtrMessageH :: UserId ::: ConnId ::: OpaqueConvId ::: OtrFilterMissing ::: JsonRequest NewOtrMessage -> Galley Response
postOtrMessageH (zusr ::: zcon ::: cnv ::: val ::: req) = do
  message <- fromJsonBody req
  handleOtrResult <$> postOtrMessage zusr zcon cnv val message

postOtrMessage :: UserId -> ConnId -> OpaqueConvId -> OtrFilterMissing -> NewOtrMessage -> Galley OtrResult
postOtrMessage zusr zcon cnv val message =
  postNewOtrMessage zusr (Just zcon) cnv val message

postProtoOtrBroadcastH :: UserId ::: ConnId ::: OtrFilterMissing ::: Request ::: JSON -> Galley Response
postProtoOtrBroadcastH (zusr ::: zcon ::: val ::: req ::: _) = do
  message <- Proto.toNewOtrMessage <$> fromProtoBody req
  handleOtrResult <$> postOtrBroadcast zusr zcon val message

postOtrBroadcastH :: UserId ::: ConnId ::: OtrFilterMissing ::: JsonRequest NewOtrMessage -> Galley Response
postOtrBroadcastH (zusr ::: zcon ::: val ::: req) = do
  message <- fromJsonBody req
  handleOtrResult <$> postOtrBroadcast zusr zcon val message

postOtrBroadcast :: UserId -> ConnId -> OtrFilterMissing -> NewOtrMessage -> Galley OtrResult
postOtrBroadcast zusr zcon val message =
  postNewOtrBroadcast zusr (Just zcon) val message

-- internal OTR helpers

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
  resolveOpaqueConvId cnv >>= \case
    Mapped idMapping ->
      -- FUTUREWORK(federation): forward message to backend owning the conversation
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
  (Member, ClientId, Text) ->
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
      conv = fromMaybe (selfConv $ memId m) cnv -- use recipient's client's self conversation on broadcast
      e = Event OtrMessageAdd conv usr now (Just $ EdOtrMessage o)
      r = recipient m & recipientClients .~ (RecipientClientsSome $ singleton c)
   in case newBotMember m of
        Just b -> ((b, e) : toBots, toUsers)
        Nothing ->
          let p =
                newPush (evtFrom e) (ConvEvent e) [r]
                  <&> set pushConn con
                  . set pushNativePriority (newOtrNativePriority msg)
                  . set pushRoute (bool RouteDirect RouteAny (newOtrNativePush msg))
                  . set pushTransient (newOtrTransient msg)
           in (toBots, p : toUsers)

updateConversationDeprecatedH :: UserId ::: ConnId ::: ConvId ::: JsonRequest ConversationRename -> Galley Response
updateConversationDeprecatedH (zusr ::: zcon ::: cnv ::: req) = do
  convRename <- fromJsonBody req
  setStatus status200 . json <$> updateConversationName zusr zcon cnv convRename

updateConversationNameH :: UserId ::: ConnId ::: ConvId ::: JsonRequest ConversationRename -> Galley Response
updateConversationNameH (zusr ::: zcon ::: cnv ::: req) = do
  convRename <- fromJsonBody req
  setStatus status200 . json <$> updateConversationName zusr zcon cnv convRename

updateConversationName :: UserId -> ConnId -> ConvId -> ConversationRename -> Galley Event
updateConversationName zusr zcon cnv convRename = do
  alive <- Data.isConvAlive cnv
  unless alive $ do
    Data.deleteConversation cnv
    throwM convNotFound
  (bots, users) <- botsAndUsers <$> Data.members cnv
  ensureActionAllowed ModifyConversationName =<< getSelfMember zusr users
  now <- liftIO getCurrentTime
  cn <- rangeChecked (cupName convRename)
  Data.updateConversation cnv cn
  let e = Event ConvRename cnv zusr now (Just $ EdConvRename convRename)
  for_ (newPush (evtFrom e) (ConvEvent e) (recipient <$> users)) $ \p ->
    push1 $ p & pushConn ?~ zcon
  void . forkIO $ void $ External.deliver (bots `zip` repeat e)
  return e

isTypingH :: UserId ::: ConnId ::: ConvId ::: JsonRequest TypingData -> Galley Response
isTypingH (zusr ::: zcon ::: cnv ::: req) = do
  typingData <- fromJsonBody req
  isTyping zusr zcon cnv typingData
  pure empty

isTyping :: UserId -> ConnId -> ConvId -> TypingData -> Galley ()
isTyping zusr zcon cnv typingData = do
  mm <- Data.members cnv
  unless (makeIdOpaque zusr `isMember` mm) $
    throwM convNotFound
  now <- liftIO getCurrentTime
  let e = Event Typing cnv zusr now (Just $ EdTyping typingData)
  for_ (newPush (evtFrom e) (ConvEvent e) (recipient <$> mm)) $ \p ->
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
  for_ (newPush (evtFrom e) (ConvEvent e) (recipient <$> users)) $ \p ->
    push1 $ p & pushConn ?~ zcon
  void . forkIO $ void $ External.deliver ((bm : bots) `zip` repeat e)
  pure e
  where
    regularConvChecks c = do
      let (bots, users) = botsAndUsers (Data.convMembers c)
      unless (makeIdOpaque zusr `isMember` users) $
        throwM convNotFound
      ensureGroupConv c
      ensureActionAllowed AddConversationMember =<< getSelfMember zusr users
      unless (any ((== b ^. addBotId) . botMemId) bots) $
        ensureMemberLimit (toList $ Data.convMembers c) [makeIdOpaque (botUserId (b ^. addBotId))]
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
  unless (makeIdOpaque zusr `isMember` Data.convMembers c) $
    throwM convNotFound
  let (bots, users) = botsAndUsers (Data.convMembers c)
  if not (any ((== b ^. rmBotId) . botMemId) bots)
    then pure Unchanged
    else do
      t <- liftIO getCurrentTime
      let evd = Just (EdMembersLeave (UserIdList [botUserId (b ^. rmBotId)]))
      let e = Event MemberLeave (Data.convId c) zusr t evd
      for_ (newPush (evtFrom e) (ConvEvent e) (recipient <$> users)) $ \p ->
        push1 $ p & pushConn .~ zcon
      Data.removeMember (botUserId (b ^. rmBotId)) (Data.convId c)
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
  for_ (newPush (evtFrom e) (ConvEvent e) (recipient <$> allMembers (toList mm))) $ \p ->
    push1 $ p & pushConn ?~ conn
  void . forkIO $ void $ External.deliver (bots `zip` repeat e)
  pure $ Updated e
  where
    -- TODO: simplify to use `nub (new <> others)`
    allMembers new = foldl' fn new others
      where
        fn acc m
          | any ((== memId m) . memId) acc = acc
          | otherwise = m : acc

ensureGroupConv :: MonadThrow m => Data.Conversation -> m ()
ensureGroupConv c = case Data.convType c of
  SelfConv -> throwM invalidSelfOp
  One2OneConv -> throwM invalidOne2OneOp
  ConnectConv -> throwM invalidConnectOp
  _ -> return ()

ensureMemberLimit :: [Member] -> [OpaqueUserId] -> Galley ()
ensureMemberLimit old new = do
  o <- view options
  let maxSize = fromIntegral (o ^. optSettings . setMaxConvSize)
  when (length old + length new > maxSize) $
    throwM tooManyMembers

notIsMember :: Data.Conversation -> OpaqueUserId -> Bool
notIsMember cc u = not $ isMember u (Data.convMembers cc)

ensureConvMember :: [Member] -> UserId -> Galley ()
ensureConvMember users usr =
  unless (makeIdOpaque usr `isMember` users) $
    throwM convNotFound

ensureAccess :: Data.Conversation -> Access -> Galley ()
ensureAccess conv access =
  unless (access `elem` Data.convAccess conv) $
    throwM convAccessDenied

applyMemUpdateChanges :: Member -> MemberUpdateData -> Member
applyMemUpdateChanges m u =
  m
    { memOtrMuted = fromMaybe (memOtrMuted m) (misOtrMuted u),
      memOtrMutedRef = misOtrMutedRef u <|> memOtrMutedRef m,
      memOtrArchived = fromMaybe (memOtrArchived m) (misOtrArchived u),
      memOtrArchivedRef = misOtrArchivedRef u <|> memOtrArchivedRef m,
      memHidden = fromMaybe (memHidden m) (misHidden u),
      memHiddenRef = misHiddenRef u <|> memHiddenRef m,
      memConvRoleName = fromMaybe (memConvRoleName m) (misConvRoleName u)
    }

processUpdateMemberEvent ::
  UserId ->
  ConnId ->
  ConvId ->
  [Member] ->
  Member ->
  MemberUpdate ->
  Galley Event
processUpdateMemberEvent zusr zcon cid users target update = do
  up <- Data.updateMember cid (memId target) update
  now <- liftIO getCurrentTime
  let e = Event MemberStateUpdate cid zusr now (Just $ EdMemberUpdate up)
  let ms = applyMemUpdateChanges target up
  for_ (newPush (evtFrom e) (ConvEvent e) (recipient ms : fmap recipient (delete target users))) $ \p ->
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
    ValidOtrRecipients !ClientMismatch [(Member, ClientId, Text)]
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
  ([(Member, ClientId, Text)] -> Galley ()) ->
  Galley OtrResult
withValidOtrBroadcastRecipients usr clt rcps val now go = Teams.withBindingTeam usr $ \tid -> do
  tMembers <- fmap (view userId) <$> Data.teamMembersUnsafeForLargeTeams tid
  contacts <- getContactList usr
  let users = Set.toList $ Set.union (Set.fromList tMembers) (Set.fromList contacts)
  isInternal <- view $ options . optSettings . setIntraListing
  clts <-
    if isInternal
      then Clients.fromUserClients <$> Intra.lookupClients users
      else Data.lookupClients users
  let membs = Data.newMember <$> users
  handleOtrResponse usr clt rcps membs clts val now go

withValidOtrRecipients ::
  UserId ->
  ClientId ->
  ConvId ->
  OtrRecipients ->
  OtrFilterMissing ->
  UTCTime ->
  ([(Member, ClientId, Text)] -> Galley ()) ->
  Galley OtrResult
withValidOtrRecipients usr clt cnv rcps val now go = do
  alive <- Data.isConvAlive cnv
  unless alive $ do
    Data.deleteConversation cnv
    throwM convNotFound
  membs <- Data.members cnv
  let memIds = (memId <$> membs)
  isInternal <- view $ options . optSettings . setIntraListing
  clts <-
    if isInternal
      then Clients.fromUserClients <$> Intra.lookupClients memIds
      else Data.lookupClients memIds
  handleOtrResponse usr clt rcps membs clts val now go

handleOtrResponse ::
  -- | Proposed sender (user)
  UserId ->
  -- | Proposed sender (client)
  ClientId ->
  -- | Proposed recipients (users & clients).
  OtrRecipients ->
  -- | Members to consider as valid recipients.
  [Member] ->
  -- | Clients to consider as valid recipients.
  Clients ->
  -- | How to filter missing clients.
  OtrFilterMissing ->
  -- | The current timestamp.
  UTCTime ->
  -- | Callback if OtrRecipients are valid
  ([(Member, ClientId, Text)] -> Galley ()) ->
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
  [Member] ->
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
    member :: OpaqueUserId -> ClientId -> Maybe Member
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
