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
    updateUnqualifiedConversationName,
    updateConversationName,
    updateConversationAccessH,
    updateConversationReceiptModeUnqualified,
    updateConversationReceiptMode,
    updateLocalConversationMessageTimer,
    updateConversationMessageTimerUnqualified,
    updateConversationMessageTimer,

    -- * Managing Members
    addMembersH,
    addMembers,
    updateUnqualifiedSelfMember,
    updateSelfMember,
    updateOtherMember,
    updateOtherMemberUnqualified,
    removeMember,
    removeMemberQualified,
    removeMemberUnqualified,
    removeMemberFromLocalConv,

    -- * Talking
    postProteusMessage,
    postOtrMessageUnqualified,
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
import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad.Catch
import Control.Monad.State
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE, withExceptT)
import Data.Code
import Data.Domain (Domain)
import Data.Either.Extra (mapRight)
import Data.Id
import Data.Json.Util (fromBase64TextLenient, toUTCTimeMillis)
import Data.List.Extra (nubOrd)
import Data.List.NonEmpty (NonEmpty (..))
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
import Galley.Types.Conversations.Roles (Action (..), RoleName, roleNameWireMember)
import Galley.Types.Teams hiding (Event, EventData (..), EventType (..), self)
import Galley.Validation
import Gundeck.Types.Push.V2 (RecipientClients (..))
import Imports
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (and, failure, setStatus, _1, _2)
import Network.Wai.Utilities
import UnliftIO (pooledForConcurrentlyN)
import Wire.API.Conversation
  ( ConversationAction (..),
    InviteQualified (invQRoleName),
  )
import qualified Wire.API.Conversation as Public
import qualified Wire.API.Conversation.Code as Public
import Wire.API.Conversation.Role (roleNameWireAdmin)
import Wire.API.ErrorDescription
  ( CodeNotFound,
    ConvMemberNotFound,
    ConvNotFound,
    MissingLegalholdConsent,
    UnknownClient,
    mkErrorDescription,
  )
import qualified Wire.API.ErrorDescription as Public
import qualified Wire.API.Event.Conversation as Public
import qualified Wire.API.Federation.API.Galley as FederatedGalley
import Wire.API.Federation.Error (federationNotImplemented)
import qualified Wire.API.Message as Public
import Wire.API.Routes.Public.Galley.Responses
import Wire.API.Routes.Public.Util (UpdateResult (..))
import Wire.API.ServantProto (RawProto (..))
import Wire.API.Team.LegalHold (LegalholdProtectee (..))
import Wire.API.User.Client

acceptConvH :: UserId ::: Maybe ConnId ::: ConvId -> Galley Response
acceptConvH (usr ::: conn ::: cnv) =
  setStatus status200 . json <$> acceptConv usr conn cnv

acceptConv :: UserId -> Maybe ConnId -> ConvId -> Galley Conversation
acceptConv usr conn cnv = do
  conv <- Data.conversation cnv >>= ifNothing (errorDescriptionTypeToWai @ConvNotFound)
  conv' <- acceptOne2One usr conv conn
  conversationView usr conv'

blockConvH :: UserId ::: ConvId -> Galley Response
blockConvH (zusr ::: cnv) =
  empty <$ blockConv zusr cnv

blockConv :: UserId -> ConvId -> Galley ()
blockConv zusr cnv = do
  conv <- Data.conversation cnv >>= ifNothing (errorDescriptionTypeToWai @ConvNotFound)
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
  conv <- Data.conversation cnv >>= ifNothing (errorDescriptionTypeToWai @ConvNotFound)
  unless (Data.convType conv `elem` [ConnectConv, One2OneConv]) $
    throwM $
      invalidOp "unblock: invalid conversation type"
  conv' <- acceptOne2One usr conv conn
  conversationView usr conv'

-- conversation updates

handleUpdateResult :: UpdateResult Event -> Response
handleUpdateResult = \case
  Updated ev -> json ev & setStatus status200
  Unchanged -> empty & setStatus status204

updateConversationAccessH :: UserId ::: ConnId ::: ConvId ::: JsonRequest Public.ConversationAccessUpdate -> Galley Response
updateConversationAccessH (usr ::: zcon ::: cnv ::: req) = do
  update <- fromJsonBody req
  handleUpdateResult <$> updateConversationAccess usr zcon cnv update

updateConversationAccess :: UserId -> ConnId -> ConvId -> Public.ConversationAccessUpdate -> Galley (UpdateResult Event)
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
  conv <- Data.conversation cnv >>= ifNothing (errorDescriptionTypeToWai @ConvNotFound)
  -- The conversation has to be a group conversation
  ensureGroupConvThrowing conv
  self <- getSelfMemberFromLocalsLegacy usr users
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
      mIds <- map lmId <$> use usersL
      activated <- fmap User.userId <$> lift (lookupActivatedUsers mIds)
      let isActivated user = lmId user `elem` activated
      usersL %= filter isActivated
    -- In a team-only conversation we also want to remove bots and guests
    case (targetRole, Data.convTeam conv) of
      (TeamAccessRole, Just tid) -> do
        currentUsers <- use usersL
        onlyTeamUsers <- flip filterM currentUsers $ \user ->
          lift $ isJust <$> Data.teamMember tid (lmId user)
        assign usersL onlyTeamUsers
        botsL .= []
      _ -> return ()
  -- Update Cassandra & send an event
  now <- liftIO getCurrentTime
  let accessEvent = Event ConvAccessUpdate qcnv qusr now (EdConvAccessUpdate body)
  Data.updateConversationAccess cnv targetAccess targetRole
  pushConversationEvent (Just zcon) accessEvent (map lmId users) bots
  -- Remove users and bots
  let removedUsers = map lmId users \\ map lmId newUsers
      removedBots = map botMemId bots \\ map botMemId newBots
  mapM_ (deleteBot cnv) removedBots
  case removedUsers of
    [] -> return ()
    x : xs -> do
      -- FUTUREWORK: deal with remote members, too, see removeMembers (Jira SQCORE-903)
      e <- Data.removeLocalMembersFromLocalConv localDomain conv (Qualified usr localDomain) (x :| xs)
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

updateConversationReceiptMode ::
  UserId ->
  ConnId ->
  Qualified ConvId ->
  Public.ConversationReceiptModeUpdate ->
  Galley (UpdateResult Event)
updateConversationReceiptMode usr zcon qcnv update = do
  localDomain <- viewFederationDomain
  if qDomain qcnv == localDomain
    then updateConversationReceiptModeUnqualified usr zcon (qUnqualified qcnv) update
    else throwM federationNotImplemented

updateConversationReceiptModeUnqualified ::
  UserId ->
  ConnId ->
  ConvId ->
  Public.ConversationReceiptModeUpdate ->
  Galley (UpdateResult Event)
updateConversationReceiptModeUnqualified usr zcon cnv receiptModeUpdate@(Public.ConversationReceiptModeUpdate target) = do
  localDomain <- viewFederationDomain
  let qcnv = Qualified cnv localDomain
      qusr = Qualified usr localDomain
  (bots, users) <- localBotsAndUsers <$> Data.members cnv
  ensureActionAllowedThrowing ModifyConversationReceiptMode =<< getSelfMemberFromLocalsLegacy usr users
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
      pushConversationEvent (Just zcon) receiptEvent (map lmId users) bots
      pure receiptEvent

updateConversationMessageTimerUnqualified ::
  UserId ->
  ConnId ->
  ConvId ->
  Public.ConversationMessageTimerUpdate ->
  Galley (UpdateResult Event)
updateConversationMessageTimerUnqualified usr zcon cnv update = do
  lusr <- qualifyLocal usr
  lcnv <- qualifyLocal cnv
  updateLocalConversationMessageTimer lusr zcon lcnv update

updateConversationMessageTimer ::
  UserId ->
  ConnId ->
  Qualified ConvId ->
  Public.ConversationMessageTimerUpdate ->
  Galley (UpdateResult Event)
updateConversationMessageTimer usr zcon qcnv update = do
  localDomain <- viewFederationDomain
  lusr <- qualifyLocal usr
  if qDomain qcnv == localDomain
    then updateLocalConversationMessageTimer lusr zcon (toLocal qcnv) update
    else throwM federationNotImplemented

updateLocalConversationMessageTimer ::
  Local UserId ->
  ConnId ->
  Local ConvId ->
  Public.ConversationMessageTimerUpdate ->
  Galley (UpdateResult Event)
updateLocalConversationMessageTimer lusr zcon lcnv update = do
  (conv, self) <-
    getConversationAndMemberWithError
      (errorDescriptionTypeToWai @ConvNotFound)
      (lUnqualified lusr)
      (lUnqualified lcnv)

  -- perform checks
  ensureActionAllowedThrowing ModifyConversationMessageTimer self
  ensureGroupConvThrowing conv

  let currentTimer = Data.convMessageTimer conv
  if currentTimer == cupMessageTimer update
    then pure Unchanged
    else
      Updated <$> do
        -- perform update
        Data.updateConversationMessageTimer (lUnqualified lcnv) (cupMessageTimer update)

        -- send notifications
        let action = ConversationActionMessageTimerUpdate update
        let targets = convTargets conv
        notifyConversationMetadataUpdate (unTagged lusr) zcon lcnv targets action

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
  conv <- Data.conversation cnv >>= ifNothing (errorDescriptionTypeToWai @ConvNotFound)
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
      pushConversationEvent (Just zcon) event (map lmId users) bots
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
  conv <- Data.conversation cnv >>= ifNothing (errorDescriptionTypeToWai @ConvNotFound)
  ensureConvMember (Data.convLocalMembers conv) usr
  ensureAccess conv CodeAccess
  let (bots, users) = localBotsAndUsers $ Data.convLocalMembers conv
  key <- mkKey cnv
  Data.deleteCode key ReusableCode
  now <- liftIO getCurrentTime
  let event = Event ConvCodeDelete qcnv qusr now EdConvCodeDelete
  pushConversationEvent (Just zcon) event (map lmId users) bots
  pure event

getCodeH :: UserId ::: ConvId -> Galley Response
getCodeH (usr ::: cnv) =
  setStatus status200 . json <$> getCode usr cnv

getCode :: UserId -> ConvId -> Galley Public.ConversationCode
getCode usr cnv = do
  conv <- Data.conversation cnv >>= ifNothing (errorDescriptionTypeToWai @ConvNotFound)
  ensureAccess conv CodeAccess
  ensureConvMember (Data.convLocalMembers conv) usr
  key <- mkKey cnv
  c <-
    Data.lookupCode key ReusableCode
      >>= ifNothing (errorDescriptionTypeToWai @CodeNotFound)
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

joinConversationByReusableCode :: UserId -> ConnId -> Public.ConversationCode -> Galley (UpdateResult Event)
joinConversationByReusableCode zusr zcon convCode = do
  c <- verifyReusableCode convCode
  joinConversation zusr zcon (codeConversation c) CodeAccess

joinConversationByIdH :: UserId ::: ConnId ::: ConvId ::: JSON -> Galley Response
joinConversationByIdH (zusr ::: zcon ::: cnv ::: _) =
  handleUpdateResult <$> joinConversationById zusr zcon cnv

joinConversationById :: UserId -> ConnId -> ConvId -> Galley (UpdateResult Event)
joinConversationById zusr zcon cnv =
  joinConversation zusr zcon cnv LinkAccess

joinConversation :: UserId -> ConnId -> ConvId -> Access -> Galley (UpdateResult Event)
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
  addToConversation mems rMems (zusr, roleNameWireMember) zcon ((,roleNameWireMember) <$> newUsers) [] conv

addMembersH :: UserId ::: ConnId ::: ConvId ::: JsonRequest Public.Invite -> Galley Response
addMembersH (zusr ::: zcon ::: cid ::: req) = do
  (Invite u r) <- fromJsonBody req
  domain <- viewFederationDomain
  let qInvite = Public.InviteQualified (flip Qualified domain <$> toNonEmpty u) r
  handleUpdateResult <$> addMembers zusr zcon cid qInvite

addMembers :: UserId -> ConnId -> ConvId -> Public.InviteQualified -> Galley (UpdateResult Event)
addMembers zusr zcon convId invite = do
  conv <- Data.conversation convId >>= ifNothing (errorDescriptionTypeToWai @ConvNotFound)
  let mems = localBotsAndUsers (Data.convLocalMembers conv)
  let rMems = Data.convRemoteMembers conv
  self <- getSelfMemberFromLocalsLegacy zusr (snd mems)
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
  addToConversation mems rMems (zusr, lmConvRoleName self) zcon (withRoles newLocals) (withRoles newRemotes) conv
  where
    userIsMember u = (^. userId . to (== u))

    withRoles :: [a] -> [(a, RoleName)]
    withRoles = map (,invQRoleName invite)

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

      whenM (anyLegalholdActivated (lmId <$> convUsers)) $
        unless allNewUsersGaveConsent $
          throwErrorDescriptionType @MissingLegalholdConsent

      whenM (anyLegalholdActivated newUsers) $ do
        unless allNewUsersGaveConsent $
          throwErrorDescriptionType @MissingLegalholdConsent

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
            localDomain <- viewFederationDomain
            for_ convUsersLHStatus $ \(mem, status) ->
              when (consentGiven status == ConsentNotGiven) $
                let qvictim = Qualified (lmId mem) localDomain
                 in void $
                      removeMember (lmId mem `Qualified` localDomain) Nothing (Data.convId conv `Qualified` localDomain) qvictim
          else throwErrorDescriptionType @MissingLegalholdConsent

    checkLHPolicyConflictsRemote :: FutureWork 'LegalholdPlusFederationNotImplemented [Remote UserId] -> Galley ()
    checkLHPolicyConflictsRemote _remotes = pure ()

updateSelfMember :: UserId -> ConnId -> Qualified ConvId -> Public.MemberUpdate -> Galley ()
updateSelfMember zusr zcon qcnv update = do
  localDomain <- viewFederationDomain
  if qDomain qcnv == localDomain
    then updateLocalSelfMember zusr zcon (toLocal qcnv) update
    else updateRemoteSelfMember zusr zcon (toRemote qcnv) update

updateUnqualifiedSelfMember :: UserId -> ConnId -> ConvId -> Public.MemberUpdate -> Galley ()
updateUnqualifiedSelfMember zusr zcon cid update = do
  localDomain <- viewFederationDomain
  updateLocalSelfMember zusr zcon (toLocal (Qualified cid localDomain)) update

updateLocalSelfMember :: UserId -> ConnId -> Local ConvId -> Public.MemberUpdate -> Galley ()
updateLocalSelfMember zusr zcon (Tagged qcid) update = do
  -- FUTUREWORK: no need to fetch the whole conversation here: the
  -- getConversationAndCheckMembership function results in 3 queries (for the
  -- conversation metadata, remote members and local members respectively), but
  -- only one is really needed (local members).
  conv <- getConversationAndCheckMembership zusr (qUnqualified qcid)
  m <- getSelfMemberFromLocalsLegacy zusr (Data.convLocalMembers conv)
  void $ processUpdateMemberEvent zusr zcon qcid [lmId m] (lmId m) update

updateRemoteSelfMember ::
  UserId ->
  ConnId ->
  Remote ConvId ->
  Public.MemberUpdate ->
  Galley ()
updateRemoteSelfMember zusr zcon rcid update = do
  statusMap <- Data.remoteConversationStatus zusr [rcid]
  case Map.lookup rcid statusMap of
    Nothing -> throwErrorDescriptionType @ConvMemberNotFound
    Just _ ->
      void $ processUpdateMemberEvent zusr zcon (unTagged rcid) [zusr] zusr update

updateOtherMember ::
  UserId ->
  ConnId ->
  Qualified ConvId ->
  Qualified UserId ->
  Public.OtherMemberUpdate ->
  Galley ()
updateOtherMember zusr zcon qcid qvictim update = do
  localDomain <- viewFederationDomain
  if qDomain qcid == localDomain && qDomain qvictim == localDomain
    then updateOtherMemberUnqualified zusr zcon (qUnqualified qcid) (qUnqualified qvictim) update
    else throwM federationNotImplemented

updateOtherMemberUnqualified ::
  UserId ->
  ConnId ->
  ConvId ->
  UserId ->
  Public.OtherMemberUpdate ->
  Galley ()
updateOtherMemberUnqualified zusr zcon cid victim update = do
  localDomain <- viewFederationDomain
  when (zusr == victim) $
    throwM invalidTargetUserOp
  conv <- getConversationAndCheckMembership zusr cid
  let (bots, users) = localBotsAndUsers (Data.convLocalMembers conv)
  ensureActionAllowedThrowing ModifyOtherConversationMember =<< getSelfMemberFromLocalsLegacy zusr users
  -- this has the side effect of checking that the victim is indeed part of the conversation
  memTarget <- getOtherMemberLegacy victim users
  e <- processUpdateMemberEvent zusr zcon (Qualified cid localDomain) (map lmId users) (lmId memTarget) update
  void . forkIO $ void $ External.deliver (bots `zip` repeat e)

-- | A general conversation member removal function used both by the unqualified
-- and the qualified endpoint for member removal. This is also used to leave a
-- conversation.
removeMember ::
  Qualified UserId ->
  Maybe ConnId ->
  Qualified ConvId ->
  Qualified UserId ->
  Galley RemoveFromConversationResponse
removeMember remover zcon qconvId@(Qualified conv convDomain) victim = do
  localDomain <- viewFederationDomain
  if localDomain == convDomain
    then
      runExceptT $
        removeMemberFromLocalConv remover zcon conv victim
    else
      if remover == victim
        then do
          let lc = FederatedGalley.LeaveConversationRequest conv (qUnqualified victim)
          let rpc =
                FederatedGalley.leaveConversation
                  FederatedGalley.clientRoutes
                  (qDomain victim)
                  lc
          t <- liftIO getCurrentTime
          let successEvent = Event MemberLeave qconvId remover t (EdMembersLeave (QualifiedUserIdList [victim]))
          mapRight (const successEvent) . FederatedGalley.leaveResponse <$> runFederated convDomain rpc
        else pure . Left $ RemoveFromConversationErrorRemovalNotAllowed

removeMemberUnqualified :: UserId -> ConnId -> ConvId -> UserId -> Galley RemoveFromConversationResponse
removeMemberUnqualified zusr zcon conv victim = do
  localDomain <- viewFederationDomain
  let qualify :: v -> Qualified v
      qualify a = a `Qualified` localDomain
  removeMember (qualify zusr) (Just zcon) (qualify conv) (qualify victim)

removeMemberQualified ::
  UserId ->
  ConnId ->
  Qualified ConvId ->
  Qualified UserId ->
  Galley RemoveFromConversationResponse
removeMemberQualified zusr zcon conv victim = do
  localDomain <- viewFederationDomain
  removeMember (Qualified zusr localDomain) (Just zcon) conv victim

-- | Remove a member from a local conversation.
removeMemberFromLocalConv ::
  -- | The remover
  Qualified UserId ->
  -- | Optional connection ID
  Maybe ConnId ->
  -- | The ID of a conversation local to this domain
  ConvId ->
  -- | The member to remove
  Qualified UserId ->
  ExceptT RemoveFromConversationError Galley Public.Event
removeMemberFromLocalConv remover@(Qualified removerUid removerDomain) zcon convId qvictim@(Qualified victim victimDomain) = do
  localDomain <- viewFederationDomain
  conv <-
    lift (Data.conversation convId)
      >>= maybe (throwE RemoveFromConversationErrorNotFound) pure
  let (bots, locals) = localBotsAndUsers (Data.convLocalMembers conv)

  removerRole <-
    withExceptT (const @_ @ConvNotFound RemoveFromConversationErrorNotFound) $
      if localDomain == removerDomain
        then lmConvRoleName <$> getSelfMemberFromLocals removerUid locals
        else rmConvRoleName <$> getSelfMemberFromRemotes (toRemote remover) (Data.convRemoteMembers conv)

  generalConvChecks localDomain removerRole conv

  unless
    ( (victimDomain == localDomain && victim `isMember` locals)
        || toRemote qvictim `isRemoteMember` Data.convRemoteMembers conv
    )
    $ throwE RemoveFromConversationErrorUnchanged

  event <-
    if victimDomain == localDomain
      then Data.removeLocalMembersFromLocalConv localDomain conv remover (pure victim)
      else Data.removeRemoteMembersFromLocalConv localDomain conv remover (pure . toRemote $ qvictim)

  -- Notify local users
  let localRemover = guard (removerDomain == localDomain) $> removerUid
  for_ (newPush ListComplete localRemover (ConvEvent event) (recipient <$> locals)) $ \p ->
    lift . push1 $ p & pushConn .~ zcon

  -- Notify the bots
  lift . void . forkIO . void $ External.deliver (bots `zip` repeat event)

  -- Notify remote backends
  let existingRemotes = rmId <$> Data.convRemoteMembers conv
  let action = ConversationActionRemoveMembers $ pure qvictim
  lift $ notifyRemoteAboutConvUpdate remover convId (evtTime event) action existingRemotes

  pure event
  where
    generalConvChecks ::
      Monad m =>
      Domain ->
      RoleName ->
      Data.Conversation ->
      ExceptT RemoveFromConversationError m ()
    generalConvChecks localDomain removerRole conv = do
      -- remote users can't remove others
      when (removerDomain /= localDomain && remover /= qvictim) $
        throwE RemoveFromConversationErrorRemovalNotAllowed

      case ensureGroupConv (Data.convType conv) of
        Left GroupConvInvalidOpSelfConv -> throwE RemoveFromConversationErrorSelfConv
        Left GroupConvInvalidOpOne2OneConv -> throwE RemoveFromConversationErrorOne2OneConv
        Left GroupConvInvalidOpConnectConv -> throwE RemoveFromConversationErrorConnectConv
        Right () -> pure ()
      let action
            | remover == qvictim = LeaveConversation
            | otherwise = RemoveConversationMember
      case ensureActionAllowed action removerRole of
        ACOAllowed ->
          pure ()
        ACOActionDenied _ ->
          throwE RemoveFromConversationErrorRemovalNotAllowed
        ACOCustomRolesNotSupported ->
          throwE RemoveFromConversationErrorCustomRolesNotSupported

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
  OtrUnknownClient _ -> throwErrorDescriptionType @UnknownClient
  OtrConversationNotFound _ -> throwErrorDescriptionType @ConvNotFound

postBotMessageH :: BotId ::: ConvId ::: Public.OtrFilterMissing ::: JsonRequest Public.NewOtrMessage ::: JSON -> Galley Response
postBotMessageH (zbot ::: zcnv ::: val ::: req ::: _) = do
  message <- fromJsonBody req
  let val' = allowOtrFilterMissingInBody val message
  handleOtrResult =<< postBotMessage zbot zcnv val' message

postBotMessage :: BotId -> ConvId -> Public.OtrFilterMissing -> Public.NewOtrMessage -> Galley OtrResult
postBotMessage zbot zcnv val message =
  postNewOtrMessage Bot (botUserId zbot) Nothing zcnv val message

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
      qconv = fromMaybe ((`Qualified` qDomain qusr) . selfConv $ lmId m) qcnv
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

updateConversationName ::
  UserId ->
  ConnId ->
  Qualified ConvId ->
  Public.ConversationRename ->
  Galley (Maybe Public.Event)
updateConversationName zusr zcon qcnv convRename = do
  lusr <- qualifyLocal zusr
  if qDomain qcnv == lDomain lusr
    then updateLocalConversationName lusr zcon (toLocal qcnv) convRename
    else throwM federationNotImplemented

updateUnqualifiedConversationName ::
  UserId ->
  ConnId ->
  ConvId ->
  Public.ConversationRename ->
  Galley (Maybe Public.Event)
updateUnqualifiedConversationName zusr zcon cnv rename = do
  lusr <- qualifyLocal zusr
  lcnv <- qualifyLocal cnv
  updateLocalConversationName lusr zcon lcnv rename

updateLocalConversationName ::
  Local UserId ->
  ConnId ->
  Local ConvId ->
  Public.ConversationRename ->
  Galley (Maybe Public.Event)
updateLocalConversationName lusr zcon lcnv convRename = do
  alive <- Data.isConvAlive (lUnqualified lcnv)
  if alive
    then Just <$> updateLiveLocalConversationName lusr zcon lcnv convRename
    else Nothing <$ Data.deleteConversation (lUnqualified lcnv)

updateLiveLocalConversationName ::
  Local UserId ->
  ConnId ->
  Local ConvId ->
  Public.ConversationRename ->
  Galley Public.Event
updateLiveLocalConversationName lusr zcon lcnv convRename = do
  -- get local members and bots
  (bots, lusers) <- localBotsAndUsers <$> Data.members (lUnqualified lcnv)

  -- perform update
  ensureActionAllowedThrowing ModifyConversationName
    =<< getSelfMemberFromLocalsLegacy (lUnqualified lusr) lusers
  cn <- rangeChecked (cupName convRename)
  Data.updateConversation (lUnqualified lcnv) cn

  -- send notifications
  rusers <- Data.lookupRemoteMembers (lUnqualified lcnv)
  let targets =
        NotificationTargets
          { ntLocals = map lmId lusers,
            ntRemotes = map rmId rusers,
            ntBots = bots
          }
  let action = ConversationActionRename convRename
  notifyConversationMetadataUpdate (unTagged lusr) zcon lcnv targets action

notifyConversationMetadataUpdate ::
  Qualified UserId ->
  ConnId ->
  Local ConvId ->
  NotificationTargets ->
  ConversationAction ->
  Galley Event
notifyConversationMetadataUpdate quid con (Tagged qcnv) targets action = do
  localDomain <- viewFederationDomain
  now <- liftIO getCurrentTime
  let e = Public.conversationActionToEvent now quid qcnv action

  -- notify remote participants
  let rusersByDomain = partitionRemote (ntRemotes targets)
  void . pooledForConcurrentlyN 8 rusersByDomain $ \(domain, uids) -> do
    let req = FederatedGalley.ConversationUpdate now quid (qUnqualified qcnv) uids action
        rpc =
          FederatedGalley.onConversationUpdated
            FederatedGalley.clientRoutes
            localDomain
            req
    runFederatedGalley domain rpc

  -- notify local participants and bots
  pushConversationEvent (Just con) e (ntLocals targets) (ntBots targets) $> e

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
    throwErrorDescriptionType @ConvNotFound
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
  c <- Data.conversation (b ^. addBotConv) >>= ifNothing (errorDescriptionTypeToWai @ConvNotFound)
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
        throwErrorDescriptionType @ConvNotFound
      ensureGroupConvThrowing c
      ensureActionAllowedThrowing AddConversationMember =<< getSelfMemberFromLocalsLegacy zusr users
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

rmBot :: UserId -> Maybe ConnId -> RemoveBot -> Galley (UpdateResult Event)
rmBot zusr zcon b = do
  c <- Data.conversation (b ^. rmBotConv) >>= ifNothing (errorDescriptionTypeToWai @ConvNotFound)
  localDomain <- viewFederationDomain
  let qcnv = Qualified (Data.convId c) localDomain
      qusr = Qualified zusr localDomain
  unless (zusr `isMember` Data.convLocalMembers c) $
    throwErrorDescriptionType @ConvNotFound
  let (bots, users) = localBotsAndUsers (Data.convLocalMembers c)
  if not (any ((== b ^. rmBotId) . botMemId) bots)
    then pure Unchanged
    else do
      t <- liftIO getCurrentTime
      let evd = EdMembersLeave (QualifiedUserIdList [Qualified (botUserId (b ^. rmBotId)) localDomain])
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
  -- | New local users to be added and their roles
  [(UserId, RoleName)] ->
  -- | New remote users to be added and their roles
  [(Remote UserId, RoleName)] ->
  -- | The conversation to modify
  Data.Conversation ->
  Galley (UpdateResult Event)
addToConversation _ _ _ _ [] [] _ = pure Unchanged
addToConversation (bots, existingLocals) existingRemotes (usr, usrRole) conn newLocals newRemotes c = do
  ensureGroupConvThrowing c
  mems <- checkedMemberAddSize newLocals newRemotes
  now <- liftIO getCurrentTime
  localDomain <- viewFederationDomain
  (e, lmm, rmm) <- Data.addMembersWithRole localDomain now (Data.convId c) (usr, usrRole) mems
  let newMembersWithRoles =
        ((flip Qualified localDomain . lmId &&& lmConvRoleName) <$> lmm)
          <> ((unTagged . rmId &&& rmConvRoleName) <$> rmm)
  case newMembersWithRoles of
    [] ->
      pure ()
    (x : xs) -> do
      let action = ConversationActionAddMembers (x :| xs)
          qusr = Qualified usr localDomain
      notifyRemoteAboutConvUpdate qusr (convId c) now action (rmId <$> existingRemotes <> rmm)
  let localsToNotify = nubOrd . fmap lmId $ existingLocals <> lmm
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
    throwErrorDescriptionType @ConvNotFound

-- | Update a member of a conversation and propagate events.
--
-- Note: the target is assumed to be a member of the conversation.
processUpdateMemberEvent ::
  Data.IsMemberUpdate mu =>
  -- | Originating user
  UserId ->
  -- | Connection ID for the originating user
  ConnId ->
  -- | Conversation whose members are being updated
  Qualified ConvId ->
  -- | Recipients of the notification
  [UserId] ->
  -- | User being updated
  UserId ->
  -- | Update structure
  mu ->
  Galley Event
processUpdateMemberEvent zusr zcon qcid users target update = do
  localDomain <- viewFederationDomain
  let qusr = Qualified zusr localDomain
  up <-
    if localDomain == qDomain qcid
      then Data.updateMember (qUnqualified qcid) target update
      else Data.updateMemberRemoteConv (toRemote qcid) target update
  now <- liftIO getCurrentTime
  let e = Event MemberStateUpdate qcid qusr now (EdMemberUpdate up)
  let recipients = fmap userRecipient (target : filter (/= target) users)
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
      pure $ OtrConversationNotFound mkErrorDescription
    else do
      localMembers <- Data.members cnv
      let localMemberIds = lmId <$> localMembers
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
      >>= either (const (throwErrorDescriptionType @MissingLegalholdConsent)) pure
    pure (OtrMissingRecipients m)
  InvalidOtrSenderUser -> pure $ OtrConversationNotFound mkErrorDescription
  InvalidOtrSenderClient -> pure $ OtrUnknownClient mkErrorDescription

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
    vmembers :: Map UserId LocalMember
    vmembers = Map.fromList $ map (\m -> (lmId m, m)) vms

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
