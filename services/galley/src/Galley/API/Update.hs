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
    deleteLocalConversation,

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

    -- * Notifications
    notifyConversationMetadataUpdate,

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
import Galley.Effects
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
import Imports hiding (forkIO)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (and, failure, setStatus, _1, _2)
import Network.Wai.Utilities
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
import Wire.API.Federation.Client (HasFederatorConfig (..))
import Wire.API.Federation.Error (federationNotConfigured, federationNotImplemented)
import qualified Wire.API.Message as Public
import Wire.API.Routes.Public.Galley.Responses
import Wire.API.Routes.Public.Util (UpdateResult (..))
import Wire.API.ServantProto (RawProto (..))
import Wire.API.Team.LegalHold (LegalholdProtectee (..))
import Wire.API.User.Client

acceptConvH :: Member GundeckAccess r => UserId ::: Maybe ConnId ::: ConvId -> Galley r Response
acceptConvH (usr ::: conn ::: cnv) =
  setStatus status200 . json <$> acceptConv usr conn cnv

acceptConv :: Member GundeckAccess r => UserId -> Maybe ConnId -> ConvId -> Galley r Conversation
acceptConv usr conn cnv = do
  conv <- Data.conversation cnv >>= ifNothing (errorDescriptionTypeToWai @ConvNotFound)
  conv' <- acceptOne2One usr conv conn
  conversationView usr conv'

blockConvH :: UserId ::: ConvId -> Galley r Response
blockConvH (zusr ::: cnv) =
  empty <$ blockConv zusr cnv

blockConv :: UserId -> ConvId -> Galley r ()
blockConv zusr cnv = do
  conv <- Data.conversation cnv >>= ifNothing (errorDescriptionTypeToWai @ConvNotFound)
  unless (Data.convType conv `elem` [ConnectConv, One2OneConv]) $
    throwM $
      invalidOp "block: invalid conversation type"
  let mems = Data.convLocalMembers conv
  when (zusr `isMember` mems) $ Data.removeMember zusr cnv

unblockConvH ::
  Member GundeckAccess r =>
  UserId ::: Maybe ConnId ::: ConvId ->
  Galley r Response
unblockConvH (usr ::: conn ::: cnv) =
  setStatus status200 . json <$> unblockConv usr conn cnv

unblockConv ::
  Member GundeckAccess r =>
  UserId ->
  Maybe ConnId ->
  ConvId ->
  Galley r Conversation
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
  Members UpdateConversationActions r =>
  UserId ->
  ConnId ->
  Qualified ConvId ->
  Public.ConversationAccessData ->
  Galley r (UpdateResult Event)
updateConversationAccess usr con qcnv update = do
  lusr <- qualifyLocal usr
  let doUpdate =
        foldQualified
          lusr
          updateLocalConversationAccess
          updateRemoteConversationAccess
  doUpdate qcnv lusr con update

updateConversationAccessUnqualified ::
  Members UpdateConversationActions r =>
  UserId ->
  ConnId ->
  ConvId ->
  Public.ConversationAccessData ->
  Galley r (UpdateResult Event)
updateConversationAccessUnqualified usr zcon cnv update = do
  lusr <- qualifyLocal usr
  lcnv <- qualifyLocal cnv
  updateLocalConversationAccess lcnv lusr zcon update

updateLocalConversationAccess ::
  Members UpdateConversationActions r =>
  Local ConvId ->
  Local UserId ->
  ConnId ->
  Public.ConversationAccessData ->
  Galley r (UpdateResult Event)
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
  Galley r (UpdateResult Event)
updateRemoteConversationAccess _ _ _ _ = throwM federationNotImplemented

performAccessUpdateAction ::
  forall r.
  Members '[BrigAccess, BotAccess, ExternalAccess, FederatorAccess, FireAndForget, GundeckAccess] r =>
  Qualified UserId ->
  Data.Conversation ->
  ConversationAccessData ->
  MaybeT (Galley r) ()
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
  let filterBotsAndMembers = filterActivated >=> filterTeammates
  let current = convBotsAndMembers conv -- initial bots and members
  desired <- lift $ filterBotsAndMembers current -- desired bots and members
  let toRemove = bmDiff current desired -- bots and members to be removed

  -- Update Cassandra
  lift $ Data.updateConversationAccess (tUnqualified lcnv) target
  lift . fireAndForget $ do
    -- Remove bots
    traverse_ (deleteBot (tUnqualified lcnv)) (map botMemId (toList (bmBots toRemove)))

    -- Update current bots and members
    let current' = current {bmBots = bmBots desired}

    -- Remove users and notify everyone
    void . for_ (nonEmpty (bmQualifiedMembers lcnv toRemove)) $ \usersToRemove -> do
      let action = ConversationActionRemoveMembers usersToRemove
      void . runMaybeT $ performAction qusr conv action
      notifyConversationMetadataUpdate qusr Nothing lcnv current' action
  where
    filterActivated :: BotsAndMembers -> Galley r BotsAndMembers
    filterActivated bm
      | ( Data.convAccessRole conv > ActivatedAccessRole
            && cupAccessRole target <= ActivatedAccessRole
        ) = do
        activated <- map User.userId <$> lookupActivatedUsers (toList (bmLocals bm))
        -- FUTUREWORK: should we also remove non-activated remote users?
        pure $ bm {bmLocals = Set.fromList activated}
      | otherwise = pure bm

    filterTeammates :: BotsAndMembers -> Galley r BotsAndMembers
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
  Members UpdateConversationActions r =>
  UserId ->
  ConnId ->
  Qualified ConvId ->
  Public.ConversationReceiptModeUpdate ->
  Galley r (UpdateResult Event)
updateConversationReceiptMode usr zcon qcnv update = do
  lusr <- qualifyLocal usr
  let doUpdate =
        foldQualified
          lusr
          updateLocalConversationReceiptMode
          updateRemoteConversationReceiptMode
  doUpdate qcnv lusr zcon update

updateConversationReceiptModeUnqualified ::
  Members UpdateConversationActions r =>
  UserId ->
  ConnId ->
  ConvId ->
  Public.ConversationReceiptModeUpdate ->
  Galley r (UpdateResult Event)
updateConversationReceiptModeUnqualified usr zcon cnv update = do
  lusr <- qualifyLocal usr
  lcnv <- qualifyLocal cnv
  updateLocalConversationReceiptMode lcnv lusr zcon update

updateLocalConversationReceiptMode ::
  Members UpdateConversationActions r =>
  Local ConvId ->
  Local UserId ->
  ConnId ->
  Public.ConversationReceiptModeUpdate ->
  Galley r (UpdateResult Event)
updateLocalConversationReceiptMode lcnv lusr con update =
  getUpdateResult $
    updateLocalConversation lcnv (qUntagged lusr) (Just con) $
      ConversationActionReceiptModeUpdate update

updateRemoteConversationReceiptMode ::
  Remote ConvId ->
  Local UserId ->
  ConnId ->
  Public.ConversationReceiptModeUpdate ->
  Galley r (UpdateResult Event)
updateRemoteConversationReceiptMode _ _ _ _ = throwM federationNotImplemented

updateConversationMessageTimerUnqualified ::
  Members UpdateConversationActions r =>
  UserId ->
  ConnId ->
  ConvId ->
  Public.ConversationMessageTimerUpdate ->
  Galley r (UpdateResult Event)
updateConversationMessageTimerUnqualified usr zcon cnv update = do
  lusr <- qualifyLocal usr
  lcnv <- qualifyLocal cnv
  updateLocalConversationMessageTimer lusr zcon lcnv update

updateConversationMessageTimer ::
  Members UpdateConversationActions r =>
  UserId ->
  ConnId ->
  Qualified ConvId ->
  Public.ConversationMessageTimerUpdate ->
  Galley r (UpdateResult Event)
updateConversationMessageTimer usr zcon qcnv update = do
  lusr <- qualifyLocal usr
  foldQualified
    lusr
    (updateLocalConversationMessageTimer lusr zcon)
    (\_ _ -> throwM federationNotImplemented)
    qcnv
    update

updateLocalConversationMessageTimer ::
  Members UpdateConversationActions r =>
  Local UserId ->
  ConnId ->
  Local ConvId ->
  Public.ConversationMessageTimerUpdate ->
  Galley r (UpdateResult Event)
updateLocalConversationMessageTimer lusr con lcnv update =
  getUpdateResult $
    updateLocalConversation lcnv (qUntagged lusr) (Just con) $
      ConversationActionMessageTimerUpdate update

deleteLocalConversation ::
  Members UpdateConversationActions r =>
  Local UserId ->
  ConnId ->
  Local ConvId ->
  Galley r (UpdateResult Event)
deleteLocalConversation lusr con lcnv =
  getUpdateResult $
    updateLocalConversation lcnv (qUntagged lusr) (Just con) ConversationActionDelete

type UpdateConversationActions =
  '[ BotAccess,
     BrigAccess,
     ExternalAccess,
     FederatorAccess,
     FireAndForget,
     GundeckAccess
   ]

-- | Update a local conversation, and notify all local and remote members.
updateLocalConversation ::
  Members UpdateConversationActions r =>
  Local ConvId ->
  Qualified UserId ->
  Maybe ConnId ->
  ConversationAction ->
  MaybeT (Galley r) Event
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
  Members UpdateConversationActions r =>
  Qualified UserId ->
  Data.Conversation ->
  ConversationAction ->
  MaybeT (Galley r) (BotsAndMembers, ConversationAction)
performAction qusr conv action = case action of
  ConversationActionAddMembers members role ->
    performAddMemberAction qusr conv members role
  ConversationActionRemoveMembers members -> do
    performRemoveMemberAction conv (toList members)
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
  ConversationActionDelete -> lift $ do
    let cid = Data.convId conv
    (`Data.deleteCode` ReusableCode) =<< mkKey cid
    case Data.convTeam conv of
      Nothing -> Data.deleteConversation cid
      Just tid -> Data.removeTeamConv tid cid
    pure (mempty, action)

addCodeH ::
  Members '[ExternalAccess, GundeckAccess] r =>
  UserId ::: ConnId ::: ConvId ->
  Galley r Response
addCodeH (usr ::: zcon ::: cnv) =
  addCode usr zcon cnv <&> \case
    CodeAdded event -> json event & setStatus status201
    CodeAlreadyExisted conversationCode -> json conversationCode & setStatus status200

data AddCodeResult
  = CodeAdded Public.Event
  | CodeAlreadyExisted Public.ConversationCode

addCode ::
  forall r.
  Members '[ExternalAccess, GundeckAccess] r =>
  UserId ->
  ConnId ->
  ConvId ->
  Galley r AddCodeResult
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
    createCode :: Code -> Galley r ConversationCode
    createCode code = do
      urlPrefix <- view $ options . optSettings . setConversationCodeURI
      return $ mkConversationCode (codeKey code) (codeValue code) urlPrefix

rmCodeH ::
  Members '[ExternalAccess, GundeckAccess] r =>
  UserId ::: ConnId ::: ConvId ->
  Galley r Response
rmCodeH (usr ::: zcon ::: cnv) =
  setStatus status200 . json <$> rmCode usr zcon cnv

rmCode ::
  Members '[ExternalAccess, GundeckAccess] r =>
  UserId ->
  ConnId ->
  ConvId ->
  Galley r Public.Event
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

getCodeH :: UserId ::: ConvId -> Galley r Response
getCodeH (usr ::: cnv) =
  setStatus status200 . json <$> getCode usr cnv

getCode :: UserId -> ConvId -> Galley r Public.ConversationCode
getCode usr cnv = do
  conv <- Data.conversation cnv >>= ifNothing (errorDescriptionTypeToWai @ConvNotFound)
  ensureAccess conv CodeAccess
  ensureConvMember (Data.convLocalMembers conv) usr
  key <- mkKey cnv
  c <-
    Data.lookupCode key ReusableCode
      >>= ifNothing (errorDescriptionTypeToWai @CodeNotFound)
  returnCode c

returnCode :: Code -> Galley r Public.ConversationCode
returnCode c = do
  urlPrefix <- view $ options . optSettings . setConversationCodeURI
  pure $ Public.mkConversationCode (codeKey c) (codeValue c) urlPrefix

checkReusableCodeH :: JsonRequest Public.ConversationCode -> Galley r Response
checkReusableCodeH req = do
  convCode <- fromJsonBody req
  checkReusableCode convCode
  pure empty

checkReusableCode :: Public.ConversationCode -> Galley r ()
checkReusableCode convCode =
  void $ verifyReusableCode convCode

joinConversationByReusableCodeH ::
  Members '[BrigAccess, FederatorAccess, ExternalAccess, GundeckAccess] r =>
  UserId ::: ConnId ::: JsonRequest Public.ConversationCode ->
  Galley r Response
joinConversationByReusableCodeH (zusr ::: zcon ::: req) = do
  convCode <- fromJsonBody req
  handleUpdateResult <$> joinConversationByReusableCode zusr zcon convCode

joinConversationByReusableCode ::
  Members '[BrigAccess, FederatorAccess, ExternalAccess, GundeckAccess] r =>
  UserId ->
  ConnId ->
  Public.ConversationCode ->
  Galley r (UpdateResult Event)
joinConversationByReusableCode zusr zcon convCode = do
  c <- verifyReusableCode convCode
  joinConversation zusr zcon (codeConversation c) CodeAccess

joinConversationByIdH ::
  Members '[BrigAccess, FederatorAccess, ExternalAccess, GundeckAccess] r =>
  UserId ::: ConnId ::: ConvId ::: JSON ->
  Galley r Response
joinConversationByIdH (zusr ::: zcon ::: cnv ::: _) =
  handleUpdateResult <$> joinConversationById zusr zcon cnv

joinConversationById ::
  Members '[BrigAccess, FederatorAccess, ExternalAccess, GundeckAccess] r =>
  UserId ->
  ConnId ->
  ConvId ->
  Galley r (UpdateResult Event)
joinConversationById zusr zcon cnv =
  joinConversation zusr zcon cnv LinkAccess

joinConversation ::
  Members '[BrigAccess, FederatorAccess, ExternalAccess, GundeckAccess] r =>
  UserId ->
  ConnId ->
  ConvId ->
  Access ->
  Galley r (UpdateResult Event)
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
  MaybeT (Galley r) (BotsAndMembers, ConversationAction)
addMembersToLocalConversation lcnv users role = do
  (lmems, rmems) <- lift $ Data.addMembers lcnv (fmap (,role) users)
  neUsers <- maybe mzero pure . nonEmpty . ulAll lcnv $ users
  let action = ConversationActionAddMembers neUsers role
  pure (bmFromMembers lmems rmems, action)

performAddMemberAction ::
  forall r.
  Members UpdateConversationActions r =>
  Qualified UserId ->
  Data.Conversation ->
  NonEmpty (Qualified UserId) ->
  RoleName ->
  MaybeT (Galley r) (BotsAndMembers, ConversationAction)
performAddMemberAction qusr conv invited role = do
  lcnv <- lift $ qualifyLocal (Data.convId conv)
  let newMembers = ulNewMembers lcnv conv . toUserList lcnv $ invited
  lift $ do
    ensureMemberLimit (toList (Data.convLocalMembers conv)) newMembers
    ensureAccess conv InviteAccess
    checkLocals lcnv (Data.convTeam conv) (ulLocals newMembers)
    checkRemotes (ulRemotes newMembers)
    checkLHPolicyConflictsLocal lcnv (ulLocals newMembers)
    checkLHPolicyConflictsRemote (FutureWork (ulRemotes newMembers))
  addMembersToLocalConversation lcnv newMembers role
  where
    userIsMember u = (^. userId . to (== u))

    checkLocals :: Local ConvId -> Maybe TeamId -> [UserId] -> Galley r ()
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

    checkRemotes :: [Remote UserId] -> Galley r ()
    checkRemotes remotes = do
      -- if federator is not configured, we fail early, so we avoid adding
      -- remote members to the database
      unless (null remotes) $ do
        endpoint <- federatorEndpoint
        when (isNothing endpoint) $
          throwM federationNotConfigured

      loc <- qualifyLocal ()
      foldQualified
        loc
        ensureConnectedToRemotes
        (\_ _ -> throwM federationNotImplemented)
        qusr
        remotes

    checkLHPolicyConflictsLocal :: Local ConvId -> [UserId] -> Galley r ()
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
                    ConversationActionRemoveMembers (pure qvictim)
          else throwErrorDescriptionType @MissingLegalholdConsent

    checkLHPolicyConflictsRemote :: FutureWork 'LegalholdPlusFederationNotImplemented [Remote UserId] -> Galley r ()
    checkLHPolicyConflictsRemote _remotes = pure ()

addMembersUnqualified ::
  Members UpdateConversationActions r =>
  UserId ->
  ConnId ->
  ConvId ->
  Public.Invite ->
  Galley r (UpdateResult Event)
addMembersUnqualified zusr zcon cnv (Public.Invite users role) = do
  qusers <- traverse (fmap qUntagged . qualifyLocal) (toNonEmpty users)
  addMembers zusr zcon cnv (Public.InviteQualified qusers role)

addMembers ::
  Members UpdateConversationActions r =>
  UserId ->
  ConnId ->
  ConvId ->
  Public.InviteQualified ->
  Galley r (UpdateResult Event)
addMembers zusr zcon cnv (Public.InviteQualified users role) = do
  lusr <- qualifyLocal zusr
  lcnv <- qualifyLocal cnv
  getUpdateResult $
    updateLocalConversation lcnv (qUntagged lusr) (Just zcon) $
      ConversationActionAddMembers users role

updateSelfMember ::
  Members '[GundeckAccess, ExternalAccess] r =>
  UserId ->
  ConnId ->
  Qualified ConvId ->
  Public.MemberUpdate ->
  Galley r ()
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

updateUnqualifiedSelfMember ::
  Members UpdateConversationActions r =>
  UserId ->
  ConnId ->
  ConvId ->
  Public.MemberUpdate ->
  Galley r ()
updateUnqualifiedSelfMember zusr zcon cnv update = do
  lcnv <- qualifyLocal cnv
  updateSelfMember zusr zcon (qUntagged lcnv) update

updateOtherMemberUnqualified ::
  Members UpdateConversationActions r =>
  UserId ->
  ConnId ->
  ConvId ->
  UserId ->
  Public.OtherMemberUpdate ->
  Galley r ()
updateOtherMemberUnqualified zusr zcon cnv victim update = do
  lusr <- qualifyLocal zusr
  lcnv <- qualifyLocal cnv
  lvictim <- qualifyLocal victim
  updateOtherMemberLocalConv lcnv lusr zcon (qUntagged lvictim) update

updateOtherMember ::
  Members UpdateConversationActions r =>
  UserId ->
  ConnId ->
  Qualified ConvId ->
  Qualified UserId ->
  Public.OtherMemberUpdate ->
  Galley r ()
updateOtherMember zusr zcon qcnv qvictim update = do
  lusr <- qualifyLocal zusr
  let doUpdate = foldQualified lusr updateOtherMemberLocalConv updateOtherMemberRemoteConv
  doUpdate qcnv lusr zcon qvictim update

updateOtherMemberLocalConv ::
  Members UpdateConversationActions r =>
  Local ConvId ->
  Local UserId ->
  ConnId ->
  Qualified UserId ->
  Public.OtherMemberUpdate ->
  Galley r ()
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
  Galley r ()
updateOtherMemberRemoteConv _ _ _ _ _ = throwM federationNotImplemented

removeMemberUnqualified ::
  Members UpdateConversationActions r =>
  UserId ->
  ConnId ->
  ConvId ->
  UserId ->
  Galley r RemoveFromConversationResponse
removeMemberUnqualified zusr con cnv victim = do
  lcnv <- qualifyLocal cnv
  lvictim <- qualifyLocal victim
  removeMemberQualified zusr con (qUntagged lcnv) (qUntagged lvictim)

removeMemberQualified ::
  Members UpdateConversationActions r =>
  UserId ->
  ConnId ->
  Qualified ConvId ->
  Qualified UserId ->
  Galley r RemoveFromConversationResponse
removeMemberQualified zusr con qcnv victim = do
  lusr <- qualifyLocal zusr
  foldQualified lusr removeMemberFromLocalConv removeMemberFromRemoteConv qcnv lusr (Just con) victim

removeMemberFromRemoteConv ::
  Member FederatorAccess r =>
  Remote ConvId ->
  Local UserId ->
  Maybe ConnId ->
  Qualified UserId ->
  Galley r RemoveFromConversationResponse
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
  [Qualified UserId] ->
  MaybeT (Galley r) ()
performRemoveMemberAction conv victims = do
  loc <- qualifyLocal ()
  let presentVictims = filter (isConvMember loc conv) victims
  guard . not . null $ presentVictims

  let (lvictims, rvictims) = partitionQualified loc presentVictims
  traverse_ (lift . Data.removeLocalMembersFromLocalConv (Data.convId conv)) (nonEmpty lvictims)
  traverse_ (lift . Data.removeRemoteMembersFromLocalConv (Data.convId conv)) (nonEmpty rvictims)

-- | Remove a member from a local conversation.
removeMemberFromLocalConv ::
  Members UpdateConversationActions r =>
  Local ConvId ->
  Local UserId ->
  Maybe ConnId ->
  Qualified UserId ->
  Galley r RemoveFromConversationResponse
removeMemberFromLocalConv lcnv lusr con victim =
  -- FUTUREWORK: actually return errors as part of the response instead of throwing
  fmap (maybe (Left RemoveFromConversationErrorUnchanged) Right)
    . runMaybeT
    . updateLocalConversation lcnv (qUntagged lusr) con
    . ConversationActionRemoveMembers
    . pure
    $ victim

-- OTR

data OtrResult
  = OtrSent !Public.ClientMismatch
  | OtrMissingRecipients !Public.ClientMismatch
  | OtrUnknownClient !Public.UnknownClient
  | OtrConversationNotFound !Public.ConvNotFound

handleOtrResult :: OtrResult -> Galley r Response
handleOtrResult = \case
  OtrSent m -> pure $ json m & setStatus status201
  OtrMissingRecipients m -> pure $ json m & setStatus status412
  OtrUnknownClient _ -> throwErrorDescriptionType @UnknownClient
  OtrConversationNotFound _ -> throwErrorDescriptionType @ConvNotFound

postBotMessageH ::
  Members '[BotAccess, BrigAccess, FederatorAccess, GundeckAccess, ExternalAccess] r =>
  BotId ::: ConvId ::: Public.OtrFilterMissing ::: JsonRequest Public.NewOtrMessage ::: JSON ->
  Galley r Response
postBotMessageH (zbot ::: zcnv ::: val ::: req ::: _) = do
  message <- fromJsonBody req
  let val' = allowOtrFilterMissingInBody val message
  handleOtrResult =<< postBotMessage zbot zcnv val' message

postBotMessage ::
  Members '[BotAccess, BrigAccess, FederatorAccess, GundeckAccess, ExternalAccess] r =>
  BotId ->
  ConvId ->
  Public.OtrFilterMissing ->
  Public.NewOtrMessage ->
  Galley r OtrResult
postBotMessage zbot zcnv val message =
  postNewOtrMessage Bot (botUserId zbot) Nothing zcnv val message

postProteusMessage ::
  Members '[BotAccess, BrigAccess, FederatorAccess, GundeckAccess, ExternalAccess] r =>
  UserId ->
  ConnId ->
  Qualified ConvId ->
  RawProto Public.QualifiedNewOtrMessage ->
  Galley r (Public.PostOtrResponse Public.MessageSendingStatus)
postProteusMessage zusr zcon conv msg = do
  localDomain <- viewFederationDomain
  let sender = Qualified zusr localDomain
  if localDomain /= qDomain conv
    then postRemoteOtrMessage sender conv (rpRaw msg)
    else postQualifiedOtrMessage User sender (Just zcon) (qUnqualified conv) (rpValue msg)

postOtrMessageUnqualified ::
  Members '[BotAccess, BrigAccess, FederatorAccess, GundeckAccess, ExternalAccess] r =>
  UserId ->
  ConnId ->
  ConvId ->
  Maybe Public.IgnoreMissing ->
  Maybe Public.ReportMissing ->
  Public.NewOtrMessage ->
  Galley r (Public.PostOtrResponse Public.ClientMismatch)
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

postProtoOtrBroadcastH ::
  Members '[BrigAccess, GundeckAccess] r =>
  UserId ::: ConnId ::: Public.OtrFilterMissing ::: Request ::: JSON ->
  Galley r Response
postProtoOtrBroadcastH (zusr ::: zcon ::: val ::: req ::: _) = do
  message <- Public.protoToNewOtrMessage <$> fromProtoBody req
  let val' = allowOtrFilterMissingInBody val message
  handleOtrResult =<< postOtrBroadcast zusr zcon val' message

postOtrBroadcastH ::
  Members '[BrigAccess, GundeckAccess] r =>
  UserId ::: ConnId ::: Public.OtrFilterMissing ::: JsonRequest Public.NewOtrMessage ->
  Galley r Response
postOtrBroadcastH (zusr ::: zcon ::: val ::: req) = do
  message <- fromJsonBody req
  let val' = allowOtrFilterMissingInBody val message
  handleOtrResult =<< postOtrBroadcast zusr zcon val' message

postOtrBroadcast ::
  Members '[BrigAccess, GundeckAccess] r =>
  UserId ->
  ConnId ->
  Public.OtrFilterMissing ->
  Public.NewOtrMessage ->
  Galley r OtrResult
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
postNewOtrBroadcast ::
  Members '[BrigAccess, GundeckAccess] r =>
  UserId ->
  Maybe ConnId ->
  OtrFilterMissing ->
  NewOtrMessage ->
  Galley r OtrResult
postNewOtrBroadcast usr con val msg = do
  localDomain <- viewFederationDomain
  let qusr = Qualified usr localDomain
      sender = newOtrSender msg
      recvrs = newOtrRecipients msg
  now <- liftIO getCurrentTime
  withValidOtrBroadcastRecipients usr sender recvrs val now $ \rs -> do
    let (_, toUsers) = foldr (newMessage qusr con Nothing msg now) ([], []) rs
    pushSome (catMaybes toUsers)

postNewOtrMessage ::
  Members '[BotAccess, BrigAccess, ExternalAccess, GundeckAccess] r =>
  UserType ->
  UserId ->
  Maybe ConnId ->
  ConvId ->
  OtrFilterMissing ->
  NewOtrMessage ->
  Galley r OtrResult
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
    External.deliverAndDeleteAsync cnv toBots

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
  Members UpdateConversationActions r =>
  UserId ->
  ConnId ->
  Qualified ConvId ->
  Public.ConversationRename ->
  Galley r (Maybe Public.Event)
updateConversationName zusr zcon qcnv convRename = do
  lusr <- qualifyLocal zusr
  foldQualified
    lusr
    (updateLocalConversationName lusr zcon)
    (\_ _ -> throwM federationNotImplemented)
    qcnv
    convRename

updateUnqualifiedConversationName ::
  Members UpdateConversationActions r =>
  UserId ->
  ConnId ->
  ConvId ->
  Public.ConversationRename ->
  Galley r (Maybe Public.Event)
updateUnqualifiedConversationName zusr zcon cnv rename = do
  lusr <- qualifyLocal zusr
  lcnv <- qualifyLocal cnv
  updateLocalConversationName lusr zcon lcnv rename

updateLocalConversationName ::
  Members UpdateConversationActions r =>
  Local UserId ->
  ConnId ->
  Local ConvId ->
  Public.ConversationRename ->
  Galley r (Maybe Public.Event)
updateLocalConversationName lusr zcon lcnv convRename = do
  alive <- Data.isConvAlive (tUnqualified lcnv)
  if alive
    then updateLiveLocalConversationName lusr zcon lcnv convRename
    else Nothing <$ Data.deleteConversation (tUnqualified lcnv)

updateLiveLocalConversationName ::
  Members UpdateConversationActions r =>
  Local UserId ->
  ConnId ->
  Local ConvId ->
  Public.ConversationRename ->
  Galley r (Maybe Public.Event)
updateLiveLocalConversationName lusr con lcnv rename =
  runMaybeT $
    updateLocalConversation lcnv (qUntagged lusr) (Just con) $
      ConversationActionRename rename

notifyConversationMetadataUpdate ::
  Members '[FederatorAccess, ExternalAccess, GundeckAccess] r =>
  Qualified UserId ->
  Maybe ConnId ->
  Local ConvId ->
  BotsAndMembers ->
  ConversationAction ->
  Galley r Event
notifyConversationMetadataUpdate quid con (qUntagged -> qcnv) targets action = do
  localDomain <- viewFederationDomain
  now <- liftIO getCurrentTime
  let e = conversationActionToEvent now quid qcnv action

  -- notify remote participants
  runFederatedConcurrently_ (toList (bmRemotes targets)) $ \ruids ->
    FederatedGalley.onConversationUpdated FederatedGalley.clientRoutes localDomain $
      FederatedGalley.ConversationUpdate now quid (qUnqualified qcnv) (tUnqualified ruids) action

  -- notify local participants and bots
  pushConversationEvent con e (bmLocals targets) (bmBots targets) $> e

isTypingH ::
  Member GundeckAccess r =>
  UserId ::: ConnId ::: ConvId ::: JsonRequest Public.TypingData ->
  Galley r Response
isTypingH (zusr ::: zcon ::: cnv ::: req) = do
  typingData <- fromJsonBody req
  isTyping zusr zcon cnv typingData
  pure empty

isTyping ::
  Member GundeckAccess r =>
  UserId ->
  ConnId ->
  ConvId ->
  Public.TypingData ->
  Galley r ()
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

addServiceH :: JsonRequest Service -> Galley r Response
addServiceH req = do
  Data.insertService =<< fromJsonBody req
  return empty

rmServiceH :: JsonRequest ServiceRef -> Galley r Response
rmServiceH req = do
  Data.deleteService =<< fromJsonBody req
  return empty

addBotH ::
  Members '[ExternalAccess, GundeckAccess] r =>
  UserId ::: ConnId ::: JsonRequest AddBot ->
  Galley r Response
addBotH (zusr ::: zcon ::: req) = do
  bot <- fromJsonBody req
  json <$> addBot zusr zcon bot

addBot ::
  Members '[ExternalAccess, GundeckAccess] r =>
  UserId ->
  ConnId ->
  AddBot ->
  Galley r Event
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
  External.deliverAsync ((bm : bots) `zip` repeat e)
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

rmBotH ::
  Members '[ExternalAccess, GundeckAccess] r =>
  UserId ::: Maybe ConnId ::: JsonRequest RemoveBot ->
  Galley r Response
rmBotH (zusr ::: zcon ::: req) = do
  bot <- fromJsonBody req
  handleUpdateResult <$> rmBot zusr zcon bot

rmBot ::
  Members '[ExternalAccess, GundeckAccess] r =>
  UserId ->
  Maybe ConnId ->
  RemoveBot ->
  Galley r (UpdateResult Event)
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
      External.deliverAsync (bots `zip` repeat e)
      pure $ Updated e

-------------------------------------------------------------------------------
-- Helpers

ensureMemberLimit :: Foldable f => [LocalMember] -> f a -> Galley r ()
ensureMemberLimit old new = do
  o <- view options
  let maxSize = fromIntegral (o ^. optSettings . setMaxConvSize)
  when (length old + length new > maxSize) $
    throwM tooManyMembers

ensureConvMember :: [LocalMember] -> UserId -> Galley r ()
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
  Member BrigAccess r =>
  UserId ->
  ClientId ->
  OtrRecipients ->
  OtrFilterMissing ->
  UTCTime ->
  ([(LocalMember, ClientId, Text)] -> Galley r ()) ->
  Galley r OtrResult
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
  Member BrigAccess r =>
  UserType ->
  UserId ->
  ClientId ->
  ConvId ->
  OtrRecipients ->
  OtrFilterMissing ->
  UTCTime ->
  ([(LocalMember, ClientId, Text)] -> Galley r ()) ->
  Galley r OtrResult
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
  Member BrigAccess r =>
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
  ([(LocalMember, ClientId, Text)] -> Galley r ()) ->
  Galley r OtrResult
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
withBindingTeam :: UserId -> (TeamId -> Galley r b) -> Galley r b
withBindingTeam zusr callback = do
  tid <- Data.oneUserTeam zusr >>= ifNothing teamNotFound
  binding <- Data.teamBinding tid >>= ifNothing teamNotFound
  case binding of
    Binding -> callback tid
    NonBinding -> throwM nonBindingTeam
