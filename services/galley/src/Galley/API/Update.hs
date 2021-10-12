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
    updateConversationReceiptModeUnqualified,
    updateConversationReceiptMode,
    updateLocalConversationMessageTimer,
    updateConversationMessageTimerUnqualified,
    updateConversationMessageTimer,
    updateLocalConversation,
    updateConversationAccessUnqualified,
    updateConversationAccess,

    -- * Managing Members
    addMembersUnqualified,
    addMembers,
    updateUnqualifiedSelfMember,
    updateSelfMember,
    updateOtherMember,
    updateOtherMemberUnqualified,
    removeMemberQualified,
    removeMemberUnqualified,
    removeMemberFromLocalConv,
    removeMemberFromRemoteConv,

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
import Control.Lens
import Control.Monad.Catch
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Code
import Data.Either.Extra (mapRight)
import Data.Id
import Data.Json.Util (fromBase64TextLenient, toUTCTimeMillis)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.List1
import qualified Data.Map.Strict as Map
import Data.Misc (FutureWork (FutureWork))
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
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
import Galley.Types.UserList
import Galley.Validation
import Gundeck.Types.Push.V2 (RecipientClients (..))
import Imports
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (and, failure, setStatus, _1, _2)
import Network.Wai.Utilities
import UnliftIO (pooledForConcurrentlyN)
import qualified Wire.API.Conversation as Public
import Wire.API.Conversation.Action
import qualified Wire.API.Conversation.Code as Public
import Wire.API.Conversation.Role (roleNameWireAdmin)
import Wire.API.ErrorDescription
  ( CodeNotFound,
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

updateConversationAccess ::
  UserId ->
  ConnId ->
  Qualified ConvId ->
  Public.ConversationAccessData ->
  Galley (UpdateResult Event)
updateConversationAccess usr con qcnv update = do
  lusr <- qualifyLocal usr
  let doUpdate =
        foldQualified
          lusr
          updateLocalConversationAccess
          updateRemoteConversationAccess
  doUpdate qcnv lusr con update

updateConversationAccessUnqualified ::
  UserId ->
  ConnId ->
  ConvId ->
  Public.ConversationAccessData ->
  Galley (UpdateResult Event)
updateConversationAccessUnqualified usr zcon cnv update = do
  lusr <- qualifyLocal usr
  lcnv <- qualifyLocal cnv
  updateLocalConversationAccess lcnv lusr zcon update

updateLocalConversationAccess ::
  Local ConvId ->
  Local UserId ->
  ConnId ->
  Public.ConversationAccessData ->
  Galley (UpdateResult Event)
updateLocalConversationAccess lcnv lusr con target =
  getUpdateResult
    . updateLocalConversation lcnv (qUntagged lusr) (Just con)
    . ConversationActionAccessUpdate
    $ target

updateRemoteConversationAccess ::
  Remote ConvId ->
  Local UserId ->
  ConnId ->
  Public.ConversationAccessData ->
  Galley (UpdateResult Event)
updateRemoteConversationAccess _ _ _ _ = throwM federationNotImplemented

performAccessUpdateAction ::
  Qualified UserId ->
  Data.Conversation ->
  ConversationAccessData ->
  MaybeT Galley ()
performAccessUpdateAction qusr conv target = do
  lcnv <- qualifyLocal (Data.convId conv)
  guard $ Data.convAccessData conv /= target
  -- Remove conversation codes if CodeAccess is revoked
  when
    ( CodeAccess `elem` Data.convAccess conv
        && CodeAccess `notElem` cupAccess target
    )
    $ lift $ do
      key <- mkKey (tUnqualified lcnv)
      Data.deleteCode key ReusableCode

  -- Determine bots and members to be removed
  let bm0 = convBotsAndMembers conv
  toRemove <-
    lift . fmap (bmDiff bm0) $
      filterTeammates =<< filterActivated bm0

  -- Update Cassandra
  lift $ Data.updateConversationAccess (tUnqualified lcnv) target
  -- Remove users and bots
  lift . void . forkIO $ do
    traverse_ (deleteBot (tUnqualified lcnv)) (map botMemId (toList (bmBots toRemove)))
    for_ (nonEmpty (toList (bmLocals toRemove))) $ \victims -> do
      -- FUTUREWORK: deal with remote members, too, see updateLocalConversation (Jira SQCORE-903)
      Data.removeLocalMembersFromLocalConv (tUnqualified lcnv) victims
      now <- liftIO getCurrentTime
      let qvictims = QualifiedUserIdList . map (qUntagged . qualifyAs lcnv) . toList $ victims
      let e = Event MemberLeave (qUntagged lcnv) qusr now (EdMembersLeave qvictims)
      -- push event to all clients, including zconn
      -- since updateConversationAccess generates a second (member removal) event here
      traverse_ push1 $
        newPushLocal
          ListComplete
          (qUnqualified qusr)
          (ConvEvent e)
          (userRecipient <$> toList (bmLocals bm0))
      void . forkIO $ void $ External.deliver (toList (bmBots bm0) `zip` repeat e)
  where
    filterActivated :: BotsAndMembers -> Galley BotsAndMembers
    filterActivated bm
      -- TODO(akshay): Remove Ord instance for AccessRole. It is dangerous
      -- to make assumption about the order of roles and implement policy
      -- based on those assumptions.
      | ( Data.convAccessRole conv > ActivatedAccessRole
            && cupAccessRole target <= ActivatedAccessRole
        ) = do
        activated <- map User.userId <$> lookupActivatedUsers (toList (bmLocals bm))
        -- FUTUREWORK: should we also remove non-activated remote users?
        pure $ bm {bmLocals = Set.fromList activated}
      | otherwise = pure bm

    filterTeammates :: BotsAndMembers -> Galley BotsAndMembers
    filterTeammates bm = do
      -- In a team-only conversation we also want to remove bots and guests
      case (cupAccessRole target, Data.convTeam conv) of
        (TeamAccessRole, Just tid) -> do
          onlyTeamUsers <- flip filterM (toList (bmLocals bm)) $ \user ->
            isJust <$> Data.teamMember tid user
          pure $
            BotsAndMembers
              { bmLocals = Set.fromList onlyTeamUsers,
                bmBots = mempty,
                bmRemotes = mempty
              }
        _ -> pure bm

updateConversationReceiptMode ::
  UserId ->
  ConnId ->
  Qualified ConvId ->
  Public.ConversationReceiptModeUpdate ->
  Galley (UpdateResult Event)
updateConversationReceiptMode usr zcon qcnv update = do
  lusr <- qualifyLocal usr
  let doUpdate =
        foldQualified
          lusr
          updateLocalConversationReceiptMode
          updateRemoteConversationReceiptMode
  doUpdate qcnv lusr zcon update

updateConversationReceiptModeUnqualified ::
  UserId ->
  ConnId ->
  ConvId ->
  Public.ConversationReceiptModeUpdate ->
  Galley (UpdateResult Event)
updateConversationReceiptModeUnqualified usr zcon cnv update = do
  lusr <- qualifyLocal usr
  lcnv <- qualifyLocal cnv
  updateLocalConversationReceiptMode lcnv lusr zcon update

updateLocalConversationReceiptMode ::
  Local ConvId ->
  Local UserId ->
  ConnId ->
  Public.ConversationReceiptModeUpdate ->
  Galley (UpdateResult Event)
updateLocalConversationReceiptMode lcnv lusr con update =
  getUpdateResult $
    updateLocalConversation lcnv (qUntagged lusr) (Just con) $
      ConversationActionReceiptModeUpdate update

updateRemoteConversationReceiptMode ::
  Remote ConvId ->
  Local UserId ->
  ConnId ->
  Public.ConversationReceiptModeUpdate ->
  Galley (UpdateResult Event)
updateRemoteConversationReceiptMode _ _ _ _ = throwM federationNotImplemented

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
  lusr <- qualifyLocal usr
  foldQualified
    lusr
    (updateLocalConversationMessageTimer lusr zcon)
    (\_ _ -> throwM federationNotImplemented)
    qcnv
    update

updateLocalConversationMessageTimer ::
  Local UserId ->
  ConnId ->
  Local ConvId ->
  Public.ConversationMessageTimerUpdate ->
  Galley (UpdateResult Event)
updateLocalConversationMessageTimer lusr con lcnv update =
  getUpdateResult $
    updateLocalConversation lcnv (qUntagged lusr) (Just con) $
      ConversationActionMessageTimerUpdate update

-- | Update a local conversation, and notify all local and remote members.
updateLocalConversation ::
  Local ConvId ->
  Qualified UserId ->
  Maybe ConnId ->
  ConversationAction ->
  MaybeT Galley Event
updateLocalConversation lcnv qusr con action = do
  -- retrieve conversation
  (conv, self) <-
    lift $
      getConversationAndMemberWithError
        (errorDescriptionTypeToWai @ConvNotFound)
        qusr
        (tUnqualified lcnv)

  -- perform checks
  lift $ ensureConversationActionAllowed action conv self

  -- perform action
  (extraTargets, action') <- performAction qusr conv action

  -- send notifications to both local and remote users
  lift $
    notifyConversationMetadataUpdate
      qusr
      con
      lcnv
      (convBotsAndMembers conv <> extraTargets)
      action'

getUpdateResult :: Functor m => MaybeT m a -> m (UpdateResult a)
getUpdateResult = fmap (maybe Unchanged Updated) . runMaybeT

-- | Perform a conversation action, and return extra notification targets and
-- an updated action.
performAction ::
  Qualified UserId ->
  Data.Conversation ->
  ConversationAction ->
  MaybeT Galley (BotsAndMembers, ConversationAction)
performAction qusr conv action = case action of
  ConversationActionAddMembers members role ->
    performAddMemberAction qusr conv members role
  ConversationActionRemoveMember member -> do
    performRemoveMemberAction conv member
    pure (mempty, action)
  ConversationActionRename rename -> lift $ do
    cn <- rangeChecked (cupName rename)
    Data.updateConversation (Data.convId conv) cn
    pure (mempty, action)
  ConversationActionMessageTimerUpdate update -> do
    guard $ Data.convMessageTimer conv /= cupMessageTimer update
    lift $ Data.updateConversationMessageTimer (Data.convId conv) (cupMessageTimer update)
    pure (mempty, action)
  ConversationActionReceiptModeUpdate update -> do
    guard $ Data.convReceiptMode conv /= Just (cruReceiptMode update)
    lift $ Data.updateConversationReceiptMode (Data.convId conv) (cruReceiptMode update)
    pure (mempty, action)
  ConversationActionMemberUpdate target update -> lift $ do
    lcnv <- qualifyLocal (Data.convId conv)
    void $ ensureOtherMember lcnv target conv
    Data.updateOtherMemberLocalConv lcnv target update
    pure (mempty, action)
  ConversationActionAccessUpdate update -> do
    performAccessUpdateAction qusr conv update
    pure (mempty, action)

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
  lusr <- qualifyLocal zusr
  lcnv <- qualifyLocal cnv
  conv <- ensureConversationAccess zusr cnv access
  ensureGroupConvThrowing conv
  -- FUTUREWORK: remote users?
  ensureMemberLimit (toList $ Data.convLocalMembers conv) [zusr]
  getUpdateResult $ do
    -- NOTE: When joining conversations, all users become members
    -- as this is our desired behavior for these types of conversations
    -- where there is no way to control who joins, etc.
    let users = filter (notIsConvMember lusr conv) [zusr]
    (extraTargets, action) <-
      addMembersToLocalConversation lcnv (UserList users []) roleNameWireMember
    lift $
      notifyConversationMetadataUpdate
        (qUntagged lusr)
        (Just zcon)
        lcnv
        (convBotsAndMembers conv <> extraTargets)
        action

-- | Add users to a conversation without performing any checks. Return extra
-- notification targets and the action performed.
addMembersToLocalConversation ::
  Local ConvId ->
  UserList UserId ->
  RoleName ->
  MaybeT Galley (BotsAndMembers, ConversationAction)
addMembersToLocalConversation lcnv users role = do
  (lmems, rmems) <- lift $ Data.addMembers lcnv (fmap (,role) users)
  neUsers <- maybe mzero pure . nonEmpty . ulAll lcnv $ users
  let action = ConversationActionAddMembers neUsers role
  pure (bmFromMembers lmems rmems, action)

performAddMemberAction ::
  Qualified UserId ->
  Data.Conversation ->
  NonEmpty (Qualified UserId) ->
  RoleName ->
  MaybeT Galley (BotsAndMembers, ConversationAction)
performAddMemberAction qusr conv invited role = do
  lcnv <- lift $ qualifyLocal (Data.convId conv)
  let newMembers = ulNewMembers lcnv conv . toUserList lcnv $ invited
  lift $ do
    ensureMemberLimit (toList (Data.convLocalMembers conv)) newMembers
    ensureAccess conv InviteAccess
    checkLocals lcnv (Data.convTeam conv) (ulLocals newMembers)
    checkRemoteUsersExist (ulRemotes newMembers)
    checkLHPolicyConflictsLocal lcnv (ulLocals newMembers)
    checkLHPolicyConflictsRemote (FutureWork (ulRemotes newMembers))
  addMembersToLocalConversation lcnv newMembers role
  where
    userIsMember u = (^. userId . to (== u))

    checkLocals :: Local ConvId -> Maybe TeamId -> [UserId] -> Galley ()
    checkLocals lcnv (Just tid) newUsers = do
      tms <- Data.teamMembersLimited tid newUsers
      let userMembershipMap = map (\u -> (u, find (userIsMember u) tms)) newUsers
      ensureAccessRole (Data.convAccessRole conv) userMembershipMap
      tcv <- Data.teamConversation tid (tUnqualified lcnv)
      when (maybe True (view managedConversation) tcv) $
        throwM noAddToManaged
      ensureConnectedOrSameTeam qusr newUsers
    checkLocals _ Nothing newUsers = do
      ensureAccessRole (Data.convAccessRole conv) (zip newUsers $ repeat Nothing)
      ensureConnectedOrSameTeam qusr newUsers

    checkLHPolicyConflictsLocal :: Local ConvId -> [UserId] -> Galley ()
    checkLHPolicyConflictsLocal lcnv newUsers = do
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
            for_ convUsersLHStatus $ \(mem, status) ->
              when (consentGiven status == ConsentNotGiven) $ do
                qvictim <- qUntagged <$> qualifyLocal (lmId mem)
                void . runMaybeT $
                  updateLocalConversation lcnv qvictim Nothing $
                    ConversationActionRemoveMember qvictim
          else throwErrorDescriptionType @MissingLegalholdConsent

    checkLHPolicyConflictsRemote :: FutureWork 'LegalholdPlusFederationNotImplemented [Remote UserId] -> Galley ()
    checkLHPolicyConflictsRemote _remotes = pure ()

addMembersUnqualified ::
  UserId -> ConnId -> ConvId -> Public.Invite -> Galley (UpdateResult Event)
addMembersUnqualified zusr zcon cnv (Public.Invite users role) = do
  qusers <- traverse (fmap qUntagged . qualifyLocal) (toNonEmpty users)
  addMembers zusr zcon cnv (Public.InviteQualified qusers role)

addMembers :: UserId -> ConnId -> ConvId -> Public.InviteQualified -> Galley (UpdateResult Event)
addMembers zusr zcon cnv (Public.InviteQualified users role) = do
  lusr <- qualifyLocal zusr
  lcnv <- qualifyLocal cnv
  getUpdateResult $
    updateLocalConversation lcnv (qUntagged lusr) (Just zcon) $
      ConversationActionAddMembers users role

updateSelfMember :: UserId -> ConnId -> Qualified ConvId -> Public.MemberUpdate -> Galley ()
updateSelfMember zusr zcon qcnv update = do
  lusr <- qualifyLocal zusr
  exists <- foldQualified lusr checkLocalMembership checkRemoteMembership qcnv lusr
  unless exists (throwErrorDescriptionType @ConvNotFound)
  Data.updateSelfMember lusr qcnv lusr update
  now <- liftIO getCurrentTime
  let e = Event MemberStateUpdate qcnv (qUntagged lusr) now (EdMemberUpdate (updateData lusr))
  pushConversationEvent (Just zcon) e [zusr] []
  where
    checkLocalMembership lcnv lusr =
      isMember (tUnqualified lusr)
        <$> Data.members (tUnqualified lcnv)
    checkRemoteMembership rcnv lusr =
      isJust . Map.lookup rcnv
        <$> Data.remoteConversationStatus (tUnqualified lusr) [rcnv]
    updateData luid =
      MemberUpdateData
        { misTarget = qUntagged luid,
          misOtrMutedStatus = mupOtrMuteStatus update,
          misOtrMutedRef = mupOtrMuteRef update,
          misOtrArchived = mupOtrArchive update,
          misOtrArchivedRef = mupOtrArchiveRef update,
          misHidden = mupHidden update,
          misHiddenRef = mupHiddenRef update,
          misConvRoleName = Nothing
        }

updateUnqualifiedSelfMember :: UserId -> ConnId -> ConvId -> Public.MemberUpdate -> Galley ()
updateUnqualifiedSelfMember zusr zcon cnv update = do
  lcnv <- qualifyLocal cnv
  updateSelfMember zusr zcon (qUntagged lcnv) update

updateOtherMemberUnqualified ::
  UserId ->
  ConnId ->
  ConvId ->
  UserId ->
  Public.OtherMemberUpdate ->
  Galley ()
updateOtherMemberUnqualified zusr zcon cnv victim update = do
  lusr <- qualifyLocal zusr
  lcnv <- qualifyLocal cnv
  lvictim <- qualifyLocal victim
  updateOtherMemberLocalConv lcnv lusr zcon (qUntagged lvictim) update

updateOtherMember ::
  UserId ->
  ConnId ->
  Qualified ConvId ->
  Qualified UserId ->
  Public.OtherMemberUpdate ->
  Galley ()
updateOtherMember zusr zcon qcnv qvictim update = do
  lusr <- qualifyLocal zusr
  let doUpdate = foldQualified lusr updateOtherMemberLocalConv updateOtherMemberRemoteConv
  doUpdate qcnv lusr zcon qvictim update

updateOtherMemberLocalConv ::
  Local ConvId ->
  Local UserId ->
  ConnId ->
  Qualified UserId ->
  Public.OtherMemberUpdate ->
  Galley ()
updateOtherMemberLocalConv lcnv lusr con qvictim update = void . getUpdateResult $ do
  when (qUntagged lusr == qvictim) $
    throwM invalidTargetUserOp
  updateLocalConversation lcnv (qUntagged lusr) (Just con) $
    ConversationActionMemberUpdate qvictim update

updateOtherMemberRemoteConv ::
  Remote ConvId ->
  Local UserId ->
  ConnId ->
  Qualified UserId ->
  Public.OtherMemberUpdate ->
  Galley ()
updateOtherMemberRemoteConv _ _ _ _ _ = throwM federationNotImplemented

removeMemberUnqualified :: UserId -> ConnId -> ConvId -> UserId -> Galley RemoveFromConversationResponse
removeMemberUnqualified zusr con cnv victim = do
  lcnv <- qualifyLocal cnv
  lvictim <- qualifyLocal victim
  removeMemberQualified zusr con (qUntagged lcnv) (qUntagged lvictim)

removeMemberQualified ::
  UserId ->
  ConnId ->
  Qualified ConvId ->
  Qualified UserId ->
  Galley RemoveFromConversationResponse
removeMemberQualified zusr con qcnv victim = do
  lusr <- qualifyLocal zusr
  foldQualified lusr removeMemberFromLocalConv removeMemberFromRemoteConv qcnv lusr (Just con) victim

removeMemberFromRemoteConv ::
  Remote ConvId ->
  Local UserId ->
  Maybe ConnId ->
  Qualified UserId ->
  Galley RemoveFromConversationResponse
removeMemberFromRemoteConv (qUntagged -> qcnv) lusr _ victim
  | qUntagged lusr == victim =
    do
      let lc = FederatedGalley.LeaveConversationRequest (qUnqualified qcnv) (qUnqualified victim)
      let rpc =
            FederatedGalley.leaveConversation
              FederatedGalley.clientRoutes
              (qDomain victim)
              lc
      t <- liftIO getCurrentTime
      let successEvent =
            Event MemberLeave qcnv (qUntagged lusr) t $
              EdMembersLeave (QualifiedUserIdList [victim])
      mapRight (const successEvent) . FederatedGalley.leaveResponse <$> runFederated (qDomain qcnv) rpc
  | otherwise = pure . Left $ RemoveFromConversationErrorRemovalNotAllowed

performRemoveMemberAction ::
  Data.Conversation ->
  Qualified UserId ->
  MaybeT Galley ()
performRemoveMemberAction conv victim = do
  loc <- qualifyLocal ()
  guard $ isConvMember loc conv victim
  let removeLocal u c = Data.removeLocalMembersFromLocalConv c (pure (tUnqualified u))
      removeRemote u c = Data.removeRemoteMembersFromLocalConv c (pure u)
  lift $ foldQualified loc removeLocal removeRemote victim (Data.convId conv)

-- | Remove a member from a local conversation.
removeMemberFromLocalConv ::
  Local ConvId ->
  Local UserId ->
  Maybe ConnId ->
  Qualified UserId ->
  Galley RemoveFromConversationResponse
removeMemberFromLocalConv lcnv lusr con victim =
  -- FUTUREWORK: actually return errors as part of the response instead of throwing
  fmap (maybe (Left RemoveFromConversationErrorUnchanged) Right)
    . runMaybeT
    . updateLocalConversation lcnv (qUntagged lusr) con
    . ConversationActionRemoveMember
    $ victim

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
  foldQualified
    lusr
    (updateLocalConversationName lusr zcon)
    (\_ _ -> throwM federationNotImplemented)
    qcnv
    convRename

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
  alive <- Data.isConvAlive (tUnqualified lcnv)
  if alive
    then updateLiveLocalConversationName lusr zcon lcnv convRename
    else Nothing <$ Data.deleteConversation (tUnqualified lcnv)

updateLiveLocalConversationName ::
  Local UserId ->
  ConnId ->
  Local ConvId ->
  Public.ConversationRename ->
  Galley (Maybe Public.Event)
updateLiveLocalConversationName lusr con lcnv rename =
  runMaybeT $
    updateLocalConversation lcnv (qUntagged lusr) (Just con) $
      ConversationActionRename rename

notifyConversationMetadataUpdate ::
  Qualified UserId ->
  Maybe ConnId ->
  Local ConvId ->
  BotsAndMembers ->
  ConversationAction ->
  Galley Event
notifyConversationMetadataUpdate quid con (qUntagged -> qcnv) targets action = do
  localDomain <- viewFederationDomain
  now <- liftIO getCurrentTime
  let e = conversationActionToEvent now quid qcnv action

  -- notify remote participants
  let rusersByDomain = indexRemote (toList (bmRemotes targets))
  void . pooledForConcurrentlyN 8 rusersByDomain $ \(qUntagged -> Qualified uids domain) -> do
    let req = FederatedGalley.ConversationUpdate now quid (qUnqualified qcnv) uids action
        rpc =
          FederatedGalley.onConversationUpdated
            FederatedGalley.clientRoutes
            localDomain
            req
    runFederatedGalley domain rpc

  -- notify local participants and bots
  pushConversationEvent con e (bmLocals targets) (bmBots targets) $> e

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
  lusr <- qualifyLocal zusr
  c <- Data.conversation (b ^. addBotConv) >>= ifNothing (errorDescriptionTypeToWai @ConvNotFound)
  -- Check some preconditions on adding bots to a conversation
  for_ (Data.convTeam c) $ teamConvChecks (b ^. addBotConv)
  (bots, users) <- regularConvChecks lusr c
  t <- liftIO getCurrentTime
  Data.updateClient True (botUserId (b ^. addBotId)) (b ^. addBotClient)
  (e, bm) <- Data.addBotMember (qUntagged lusr) (b ^. addBotService) (b ^. addBotId) (b ^. addBotConv) t
  for_ (newPushLocal ListComplete zusr (ConvEvent e) (recipient <$> users)) $ \p ->
    push1 $ p & pushConn ?~ zcon
  void . forkIO $ void $ External.deliver ((bm : bots) `zip` repeat e)
  pure e
  where
    regularConvChecks lusr c = do
      let (bots, users) = localBotsAndUsers (Data.convLocalMembers c)
      unless (zusr `isMember` users) $
        throwErrorDescriptionType @ConvNotFound
      ensureGroupConvThrowing c
      ensureActionAllowed AddConversationMember =<< getSelfMemberFromLocalsLegacy zusr users
      unless (any ((== b ^. addBotId) . botMemId) bots) $ do
        let botId = qualifyAs lusr (botUserId (b ^. addBotId))
        ensureMemberLimit (toList $ Data.convLocalMembers c) [qUntagged botId]
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

ensureMemberLimit :: Foldable f => [LocalMember] -> f a -> Galley ()
ensureMemberLimit old new = do
  o <- view options
  let maxSize = fromIntegral (o ^. optSettings . setMaxConvSize)
  when (length old + length new > maxSize) $
    throwM tooManyMembers

ensureConvMember :: [LocalMember] -> UserId -> Galley ()
ensureConvMember users usr =
  unless (usr `isMember` users) $
    throwErrorDescriptionType @ConvNotFound

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
