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

import Control.Error.Util (hush)
import Control.Lens
import Control.Monad.State
import Data.Code
import Data.Id
import Data.Json.Util (fromBase64TextLenient, toUTCTimeMillis)
import Data.List1
import qualified Data.Map.Strict as Map
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
import Data.Time
import Galley.API.Action
import Galley.API.Error
import Galley.API.LegalHold.Conflicts
import Galley.API.Mapping
import Galley.API.Message
import qualified Galley.API.Query as Query
import Galley.API.Util
import qualified Galley.Data.Conversation as Data
import Galley.Data.Services as Data
import Galley.Data.Types hiding (Conversation)
import Galley.Effects
import qualified Galley.Effects.BrigAccess as E
import qualified Galley.Effects.ClientStore as E
import qualified Galley.Effects.CodeStore as E
import qualified Galley.Effects.ConversationStore as E
import qualified Galley.Effects.ExternalAccess as E
import qualified Galley.Effects.FederatorAccess as E
import qualified Galley.Effects.GundeckAccess as E
import qualified Galley.Effects.MemberStore as E
import qualified Galley.Effects.ServiceStore as E
import qualified Galley.Effects.TeamStore as E
import Galley.Effects.WaiRoutes
import Galley.Intra.Push
import Galley.Options
import Galley.Types
import Galley.Types.Bot hiding (addBot)
import Galley.Types.Clients (Clients)
import qualified Galley.Types.Clients as Clients
import Galley.Types.Conversations.Members (newMember)
import Galley.Types.Conversations.Roles (Action (..), roleNameWireMember)
import Galley.Types.Teams hiding (Event, EventData (..), EventType (..), self)
import Galley.Types.UserList
import Gundeck.Types.Push.V2 (RecipientClients (..))
import Imports hiding (forkIO)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (Error, and, failure, setStatus, _1, _2)
import Network.Wai.Utilities hiding (Error)
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog
import qualified Wire.API.Conversation as Public
import qualified Wire.API.Conversation.Code as Public
import Wire.API.Conversation.Role (roleNameWireAdmin)
import Wire.API.ErrorDescription
import qualified Wire.API.Event.Conversation as Public
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import qualified Wire.API.Message as Public
import Wire.API.Routes.Public.Util (UpdateResult (..))
import Wire.API.ServantProto (RawProto (..))
import Wire.API.User.Client

acceptConvH ::
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error InternalError,
       GundeckAccess,
       Input (Local ()),
       Input UTCTime,
       MemberStore,
       TinyLog
     ]
    r =>
  UserId ::: Maybe ConnId ::: ConvId ->
  Sem r Response
acceptConvH (usr ::: conn ::: cnv) = do
  lusr <- qualifyLocal usr
  setStatus status200 . json <$> acceptConv lusr conn cnv

acceptConv ::
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error InternalError,
       GundeckAccess,
       Input UTCTime,
       MemberStore,
       TinyLog
     ]
    r =>
  Local UserId ->
  Maybe ConnId ->
  ConvId ->
  Sem r Conversation
acceptConv lusr conn cnv = do
  conv <-
    E.getConversation cnv >>= note ConvNotFound
  conv' <- acceptOne2One lusr conv conn
  conversationView lusr conv'

blockConvH ::
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       MemberStore
     ]
    r =>
  UserId ::: ConvId ->
  Sem r Response
blockConvH (zusr ::: cnv) =
  empty <$ blockConv zusr cnv

blockConv ::
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       MemberStore
     ]
    r =>
  UserId ->
  ConvId ->
  Sem r ()
blockConv zusr cnv = do
  conv <- E.getConversation cnv >>= note ConvNotFound
  unless (Data.convType conv `elem` [ConnectConv, One2OneConv]) $
    throw . InvalidOp . Data.convType $ conv
  let mems = Data.convLocalMembers conv
  when (zusr `isMember` mems) $
    E.deleteMembers cnv (UserList [zusr] [])

unblockConvH ::
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error InternalError,
       GundeckAccess,
       Input (Local ()),
       Input UTCTime,
       MemberStore,
       TinyLog
     ]
    r =>
  UserId ::: Maybe ConnId ::: ConvId ->
  Sem r Response
unblockConvH (usr ::: conn ::: cnv) = do
  lusr <- qualifyLocal usr
  setStatus status200 . json <$> unblockConv lusr conn cnv

unblockConv ::
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error InternalError,
       GundeckAccess,
       Input UTCTime,
       MemberStore,
       TinyLog
     ]
    r =>
  Local UserId ->
  Maybe ConnId ->
  ConvId ->
  Sem r Conversation
unblockConv lusr conn cnv = do
  conv <-
    E.getConversation cnv >>= note ConvNotFound
  unless (Data.convType conv `elem` [ConnectConv, One2OneConv]) $
    throw . InvalidOp . Data.convType $ conv
  conv' <- acceptOne2One lusr conv conn
  conversationView lusr conv'

-- conversation updates

handleUpdateResult :: UpdateResult Event -> Response
handleUpdateResult = \case
  Updated ev -> json ev & setStatus status200
  Unchanged -> empty & setStatus status204

updateConversationAccess ::
  Members
    '[ BotAccess,
       BrigAccess,
       CodeStore,
       ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error FederationError,
       Error InvalidInput,
       ExternalAccess,
       FederatorAccess,
       FireAndForget,
       GundeckAccess,
       Input UTCTime,
       MemberStore,
       TeamStore
     ]
    r =>
  Local UserId ->
  ConnId ->
  Qualified ConvId ->
  Public.ConversationAccessData ->
  Sem r (UpdateResult Event)
updateConversationAccess lusr con qcnv update = do
  let doUpdate =
        foldQualified
          lusr
          updateLocalConversationAccess
          updateRemoteConversationAccess
  doUpdate qcnv lusr con update

updateConversationAccessUnqualified ::
  Members
    '[ BotAccess,
       BrigAccess,
       CodeStore,
       ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error InvalidInput,
       ExternalAccess,
       FederatorAccess,
       FireAndForget,
       GundeckAccess,
       Input UTCTime,
       MemberStore,
       TeamStore
     ]
    r =>
  Local UserId ->
  ConnId ->
  ConvId ->
  Public.ConversationAccessData ->
  Sem r (UpdateResult Event)
updateConversationAccessUnqualified lusr zcon cnv update = do
  let lcnv = qualifyAs lusr cnv
  updateLocalConversationAccess lcnv lusr zcon update

updateLocalConversationAccess ::
  Members
    '[ BotAccess,
       BrigAccess,
       CodeStore,
       ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error InvalidInput,
       ExternalAccess,
       FederatorAccess,
       FireAndForget,
       GundeckAccess,
       Input UTCTime,
       MemberStore,
       TeamStore
     ]
    r =>
  Local ConvId ->
  Local UserId ->
  ConnId ->
  Public.ConversationAccessData ->
  Sem r (UpdateResult Event)
updateLocalConversationAccess lcnv lusr con =
  getUpdateResult
    . updateLocalConversation lcnv (qUntagged lusr) (Just con)

updateRemoteConversationAccess ::
  Member (Error FederationError) r =>
  Remote ConvId ->
  Local UserId ->
  ConnId ->
  Public.ConversationAccessData ->
  Sem r (UpdateResult Event)
updateRemoteConversationAccess _ _ _ _ = throw FederationNotImplemented

updateConversationReceiptMode ::
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error InvalidInput,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input UTCTime
     ]
    r =>
  Members '[Error FederationError] r =>
  Local UserId ->
  ConnId ->
  Qualified ConvId ->
  Public.ConversationReceiptModeUpdate ->
  Sem r (UpdateResult Event)
updateConversationReceiptMode lusr zcon qcnv update = do
  let doUpdate =
        foldQualified
          lusr
          updateLocalConversationReceiptMode
          updateRemoteConversationReceiptMode
  doUpdate qcnv lusr zcon update

updateConversationReceiptModeUnqualified ::
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error InvalidInput,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input UTCTime
     ]
    r =>
  Local UserId ->
  ConnId ->
  ConvId ->
  Public.ConversationReceiptModeUpdate ->
  Sem r (UpdateResult Event)
updateConversationReceiptModeUnqualified lusr zcon cnv update = do
  let lcnv = qualifyAs lusr cnv
  updateLocalConversationReceiptMode lcnv lusr zcon update

updateLocalConversationReceiptMode ::
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error InvalidInput,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input UTCTime
     ]
    r =>
  Local ConvId ->
  Local UserId ->
  ConnId ->
  Public.ConversationReceiptModeUpdate ->
  Sem r (UpdateResult Event)
updateLocalConversationReceiptMode lcnv lusr con update =
  getUpdateResult $
    updateLocalConversation lcnv (qUntagged lusr) (Just con) update

updateRemoteConversationReceiptMode ::
  Member (Error FederationError) r =>
  Remote ConvId ->
  Local UserId ->
  ConnId ->
  Public.ConversationReceiptModeUpdate ->
  Sem r (UpdateResult Event)
updateRemoteConversationReceiptMode _ _ _ _ = throw FederationNotImplemented

updateConversationMessageTimerUnqualified ::
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error InvalidInput,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input UTCTime
     ]
    r =>
  Local UserId ->
  ConnId ->
  ConvId ->
  Public.ConversationMessageTimerUpdate ->
  Sem r (UpdateResult Event)
updateConversationMessageTimerUnqualified lusr zcon cnv update = do
  let lcnv = qualifyAs lusr cnv
  updateLocalConversationMessageTimer lusr zcon lcnv update

updateConversationMessageTimer ::
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error FederationError,
       Error InvalidInput,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input UTCTime
     ]
    r =>
  Local UserId ->
  ConnId ->
  Qualified ConvId ->
  Public.ConversationMessageTimerUpdate ->
  Sem r (UpdateResult Event)
updateConversationMessageTimer lusr zcon qcnv update = do
  foldQualified
    lusr
    (updateLocalConversationMessageTimer lusr zcon)
    (\_ _ -> throw FederationNotImplemented)
    qcnv
    update

updateLocalConversationMessageTimer ::
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error InvalidInput,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input UTCTime
     ]
    r =>
  Local UserId ->
  ConnId ->
  Local ConvId ->
  Public.ConversationMessageTimerUpdate ->
  Sem r (UpdateResult Event)
updateLocalConversationMessageTimer lusr con lcnv update =
  getUpdateResult $
    updateLocalConversation lcnv (qUntagged lusr) (Just con) update

deleteLocalConversation ::
  Members
    '[ CodeStore,
       ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error FederationError,
       Error InvalidInput,
       Error NotATeamMember,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input UTCTime,
       TeamStore
     ]
    r =>
  Local UserId ->
  ConnId ->
  Local ConvId ->
  Sem r (UpdateResult Event)
deleteLocalConversation lusr con lcnv =
  getUpdateResult $
    updateLocalConversation lcnv (qUntagged lusr) (Just con) ConversationDelete

getUpdateResult :: Sem (Error NoChanges ': r) a -> Sem r (UpdateResult a)
getUpdateResult = fmap (either (const Unchanged) Updated) . runError

addCodeH ::
  forall r.
  ( Member CodeStore r,
    Member ConversationStore r,
    Member (Error ConversationError) r,
    Member ExternalAccess r,
    Member GundeckAccess r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (Input Opts) r,
    Member TeamFeatureStore r
  ) =>
  UserId ::: ConnId ::: ConvId ->
  Sem r Response
addCodeH (usr ::: zcon ::: cnv) = do
  lusr <- qualifyLocal usr
  lcnv <- qualifyLocal cnv
  addCode lusr zcon lcnv <&> \case
    CodeAdded event -> json event & setStatus status201
    CodeAlreadyExisted conversationCode -> json conversationCode & setStatus status200

data AddCodeResult
  = CodeAdded Public.Event
  | CodeAlreadyExisted Public.ConversationCode

addCode ::
  forall r.
  ( Member CodeStore r,
    Member ConversationStore r,
    Member (Error ConversationError) r,
    Member ExternalAccess r,
    Member GundeckAccess r,
    Member (Input UTCTime) r,
    Member (Input Opts) r,
    Member TeamFeatureStore r
  ) =>
  Local UserId ->
  ConnId ->
  Local ConvId ->
  Sem r AddCodeResult
addCode lusr zcon lcnv = do
  conv <- E.getConversation (tUnqualified lcnv) >>= note ConvNotFound
  Query.ensureGuestLinksEnabled conv
  ensureConvMember (Data.convLocalMembers conv) (tUnqualified lusr)
  ensureAccess conv CodeAccess
  let (bots, users) = localBotsAndUsers $ Data.convLocalMembers conv
  key <- E.makeKey (tUnqualified lcnv)
  mCode <- E.getCode key ReusableCode
  case mCode of
    Nothing -> do
      code <- E.generateCode (tUnqualified lcnv) ReusableCode (Timeout 3600 * 24 * 365) -- one year FUTUREWORK: configurable
      E.createCode code
      now <- input
      conversationCode <- createCode code
      let event = Event ConvCodeUpdate (qUntagged lcnv) (qUntagged lusr) now (EdConvCodeUpdate conversationCode)
      pushConversationEvent (Just zcon) event (qualifyAs lusr (map lmId users)) bots
      pure $ CodeAdded event
    Just code -> do
      conversationCode <- createCode code
      pure $ CodeAlreadyExisted conversationCode
  where
    createCode :: Code -> Sem r ConversationCode
    createCode code = do
      mkConversationCode (codeKey code) (codeValue code) <$> E.getConversationCodeURI

rmCodeH ::
  Members
    '[ CodeStore,
       ConversationStore,
       Error ConversationError,
       ExternalAccess,
       GundeckAccess,
       Input (Local ()),
       Input UTCTime
     ]
    r =>
  UserId ::: ConnId ::: ConvId ->
  Sem r Response
rmCodeH (usr ::: zcon ::: cnv) = do
  lusr <- qualifyLocal usr
  lcnv <- qualifyLocal cnv
  setStatus status200 . json <$> rmCode lusr zcon lcnv

rmCode ::
  Members
    '[ CodeStore,
       ConversationStore,
       Error ConversationError,
       ExternalAccess,
       GundeckAccess,
       Input UTCTime
     ]
    r =>
  Local UserId ->
  ConnId ->
  Local ConvId ->
  Sem r Public.Event
rmCode lusr zcon lcnv = do
  conv <-
    E.getConversation (tUnqualified lcnv) >>= note ConvNotFound
  ensureConvMember (Data.convLocalMembers conv) (tUnqualified lusr)
  ensureAccess conv CodeAccess
  let (bots, users) = localBotsAndUsers $ Data.convLocalMembers conv
  key <- E.makeKey (tUnqualified lcnv)
  E.deleteCode key ReusableCode
  now <- input
  let event = Event ConvCodeDelete (qUntagged lcnv) (qUntagged lusr) now EdConvCodeDelete
  pushConversationEvent (Just zcon) event (qualifyAs lusr (map lmId users)) bots
  pure event

getCodeH ::
  forall r.
  ( Member CodeStore r,
    Member ConversationStore r,
    Member (Error CodeError) r,
    Member (Error ConversationError) r,
    Member (Input Opts) r,
    Member TeamFeatureStore r
  ) =>
  UserId ::: ConvId ->
  Sem r Response
getCodeH (usr ::: cnv) =
  setStatus status200 . json <$> getCode usr cnv

getCode ::
  forall r.
  ( Member CodeStore r,
    Member ConversationStore r,
    Member (Error CodeError) r,
    Member (Error ConversationError) r,
    Member (Input Opts) r,
    Member TeamFeatureStore r
  ) =>
  UserId ->
  ConvId ->
  Sem r Public.ConversationCode
getCode usr cnv = do
  conv <-
    E.getConversation cnv >>= note ConvNotFound
  Query.ensureGuestLinksEnabled conv
  ensureAccess conv CodeAccess
  ensureConvMember (Data.convLocalMembers conv) usr
  key <- E.makeKey cnv
  c <- E.getCode key ReusableCode >>= note CodeNotFound
  returnCode c

returnCode :: Member CodeStore r => Code -> Sem r Public.ConversationCode
returnCode c = do
  Public.mkConversationCode (codeKey c) (codeValue c) <$> E.getConversationCodeURI

checkReusableCodeH ::
  Members '[CodeStore, Error CodeError, WaiRoutes] r =>
  JsonRequest Public.ConversationCode ->
  Sem r Response
checkReusableCodeH req = do
  convCode <- fromJsonBody req
  checkReusableCode convCode
  pure empty

checkReusableCode ::
  Members '[CodeStore, Error CodeError] r =>
  Public.ConversationCode ->
  Sem r ()
checkReusableCode convCode =
  void $ verifyReusableCode convCode

joinConversationByReusableCodeH ::
  Members
    '[ BrigAccess,
       CodeStore,
       ConversationStore,
       FederatorAccess,
       Error ActionError,
       Error CodeError,
       Error ConversationError,
       Error NotATeamMember,
       ExternalAccess,
       GundeckAccess,
       Input (Local ()),
       Input Opts,
       Input UTCTime,
       MemberStore,
       TeamStore,
       WaiRoutes
     ]
    r =>
  UserId ::: ConnId ::: JsonRequest Public.ConversationCode ->
  Sem r Response
joinConversationByReusableCodeH (zusr ::: zcon ::: req) = do
  lusr <- qualifyLocal zusr
  convCode <- fromJsonBody req
  handleUpdateResult <$> joinConversationByReusableCode lusr zcon convCode

joinConversationByReusableCode ::
  Members
    '[ BrigAccess,
       CodeStore,
       ConversationStore,
       Error ActionError,
       Error CodeError,
       Error ConversationError,
       Error NotATeamMember,
       FederatorAccess,
       ExternalAccess,
       GundeckAccess,
       Input Opts,
       Input UTCTime,
       MemberStore,
       TeamStore
     ]
    r =>
  Local UserId ->
  ConnId ->
  Public.ConversationCode ->
  Sem r (UpdateResult Event)
joinConversationByReusableCode lusr zcon convCode = do
  c <- verifyReusableCode convCode
  joinConversation lusr zcon (codeConversation c) CodeAccess

joinConversationByIdH ::
  Members
    '[ BrigAccess,
       FederatorAccess,
       ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error NotATeamMember,
       ExternalAccess,
       GundeckAccess,
       Input (Local ()),
       Input Opts,
       Input UTCTime,
       MemberStore,
       TeamStore
     ]
    r =>
  UserId ::: ConnId ::: ConvId ::: JSON ->
  Sem r Response
joinConversationByIdH (zusr ::: zcon ::: cnv ::: _) = do
  lusr <- qualifyLocal zusr
  handleUpdateResult <$> joinConversationById lusr zcon cnv

joinConversationById ::
  Members
    '[ BrigAccess,
       FederatorAccess,
       ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error NotATeamMember,
       ExternalAccess,
       GundeckAccess,
       Input Opts,
       Input UTCTime,
       MemberStore,
       TeamStore
     ]
    r =>
  Local UserId ->
  ConnId ->
  ConvId ->
  Sem r (UpdateResult Event)
joinConversationById lusr zcon cnv =
  joinConversation lusr zcon cnv LinkAccess

joinConversation ::
  Members
    '[ BrigAccess,
       ConversationStore,
       FederatorAccess,
       Error ActionError,
       Error ConversationError,
       Error NotATeamMember,
       ExternalAccess,
       GundeckAccess,
       Input Opts,
       Input UTCTime,
       MemberStore,
       TeamStore
     ]
    r =>
  Local UserId ->
  ConnId ->
  ConvId ->
  Access ->
  Sem r (UpdateResult Event)
joinConversation lusr zcon cnv access = do
  let lcnv = qualifyAs lusr cnv
  conv <- ensureConversationAccess (tUnqualified lusr) cnv access
  ensureGroupConversation $ conv
  -- FUTUREWORK: remote users?
  ensureMemberLimit (toList $ Data.convLocalMembers conv) [tUnqualified lusr]
  getUpdateResult $ do
    -- NOTE: When joining conversations, all users become members
    -- as this is our desired behavior for these types of conversations
    -- where there is no way to control who joins, etc.
    let users = filter (notIsConvMember lusr conv) [tUnqualified lusr]
    (extraTargets, action) <-
      addMembersToLocalConversation lcnv (UserList users []) roleNameWireMember
    notifyConversationAction
      (qUntagged lusr)
      (Just zcon)
      lcnv
      (convBotsAndMembers conv <> extraTargets)
      (conversationAction action)

addMembersUnqualified ::
  Members
    '[ BrigAccess,
       ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error FederationError,
       Error InvalidInput,
       Error LegalHoldError,
       Error NotATeamMember,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input Opts,
       Input UTCTime,
       LegalHoldStore,
       MemberStore,
       TeamStore
     ]
    r =>
  Local UserId ->
  ConnId ->
  ConvId ->
  Public.Invite ->
  Sem r (UpdateResult Event)
addMembersUnqualified lusr zcon cnv (Public.Invite users role) = do
  let qusers = fmap (qUntagged . qualifyAs lusr) (toNonEmpty users)
  addMembers lusr zcon cnv (Public.InviteQualified qusers role)

addMembers ::
  Members
    '[ BrigAccess,
       ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error FederationError,
       Error InvalidInput,
       Error LegalHoldError,
       Error NotATeamMember,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input Opts,
       Input UTCTime,
       LegalHoldStore,
       MemberStore,
       TeamStore
     ]
    r =>
  Local UserId ->
  ConnId ->
  ConvId ->
  Public.InviteQualified ->
  Sem r (UpdateResult Event)
addMembers lusr zcon cnv (Public.InviteQualified users role) = do
  let lcnv = qualifyAs lusr cnv
  getUpdateResult $
    updateLocalConversation lcnv (qUntagged lusr) (Just zcon) $
      ConversationJoin users role

updateSelfMember ::
  Members
    '[ ConversationStore,
       Error ConversationError,
       ExternalAccess,
       GundeckAccess,
       Input UTCTime,
       MemberStore
     ]
    r =>
  Local UserId ->
  ConnId ->
  Qualified ConvId ->
  Public.MemberUpdate ->
  Sem r ()
updateSelfMember lusr zcon qcnv update = do
  exists <- foldQualified lusr checkLocalMembership checkRemoteMembership qcnv
  unless exists . throw $ ConvNotFound
  E.setSelfMember qcnv lusr update
  now <- input
  let e = Event MemberStateUpdate qcnv (qUntagged lusr) now (EdMemberUpdate (updateData lusr))
  pushConversationEvent (Just zcon) e (fmap pure lusr) []
  where
    checkLocalMembership ::
      Members '[MemberStore] r =>
      Local ConvId ->
      Sem r Bool
    checkLocalMembership lcnv =
      isMember (tUnqualified lusr)
        <$> E.getLocalMembers (tUnqualified lcnv)
    checkRemoteMembership ::
      Members '[ConversationStore] r =>
      Remote ConvId ->
      Sem r Bool
    checkRemoteMembership rcnv =
      isJust . Map.lookup rcnv
        <$> E.getRemoteConversationStatus (tUnqualified lusr) [rcnv]
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
  Members
    '[ ConversationStore,
       Error ConversationError,
       ExternalAccess,
       GundeckAccess,
       Input UTCTime,
       MemberStore
     ]
    r =>
  Local UserId ->
  ConnId ->
  ConvId ->
  Public.MemberUpdate ->
  Sem r ()
updateUnqualifiedSelfMember lusr zcon cnv update = do
  let lcnv = qualifyAs lusr cnv
  updateSelfMember lusr zcon (qUntagged lcnv) update

updateOtherMemberUnqualified ::
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error InvalidInput,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input UTCTime,
       MemberStore
     ]
    r =>
  Local UserId ->
  ConnId ->
  ConvId ->
  UserId ->
  Public.OtherMemberUpdate ->
  Sem r ()
updateOtherMemberUnqualified lusr zcon cnv victim update = do
  let lcnv = qualifyAs lusr cnv
  let lvictim = qualifyAs lusr victim
  updateOtherMemberLocalConv lcnv lusr zcon (qUntagged lvictim) update

updateOtherMember ::
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error FederationError,
       Error InvalidInput,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input UTCTime,
       MemberStore
     ]
    r =>
  Local UserId ->
  ConnId ->
  Qualified ConvId ->
  Qualified UserId ->
  Public.OtherMemberUpdate ->
  Sem r ()
updateOtherMember lusr zcon qcnv qvictim update = do
  let doUpdate = foldQualified lusr updateOtherMemberLocalConv updateOtherMemberRemoteConv
  doUpdate qcnv lusr zcon qvictim update

updateOtherMemberLocalConv ::
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error InvalidInput,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input UTCTime,
       MemberStore
     ]
    r =>
  Local ConvId ->
  Local UserId ->
  ConnId ->
  Qualified UserId ->
  Public.OtherMemberUpdate ->
  Sem r ()
updateOtherMemberLocalConv lcnv lusr con qvictim update = void . getUpdateResult $ do
  when (qUntagged lusr == qvictim) $
    throw InvalidTargetUserOp
  updateLocalConversation lcnv (qUntagged lusr) (Just con) $
    ConversationMemberUpdate qvictim update

updateOtherMemberRemoteConv ::
  Member (Error FederationError) r =>
  Remote ConvId ->
  Local UserId ->
  ConnId ->
  Qualified UserId ->
  Public.OtherMemberUpdate ->
  Sem r ()
updateOtherMemberRemoteConv _ _ _ _ _ = throw FederationNotImplemented

removeMemberUnqualified ::
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error InvalidInput,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input UTCTime,
       MemberStore
     ]
    r =>
  Local UserId ->
  ConnId ->
  ConvId ->
  UserId ->
  Sem r (Maybe Event)
removeMemberUnqualified lusr con cnv victim = do
  let lvictim = qualifyAs lusr victim
      lcnv = qualifyAs lusr cnv
  removeMemberQualified lusr con (qUntagged lcnv) (qUntagged lvictim)

removeMemberQualified ::
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error InvalidInput,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input UTCTime,
       MemberStore
     ]
    r =>
  Local UserId ->
  ConnId ->
  Qualified ConvId ->
  Qualified UserId ->
  Sem r (Maybe Event)
removeMemberQualified lusr con =
  foldQualified
    lusr
    (\lcnv -> removeMemberFromLocalConv lcnv lusr (Just con))
    (\rcnv -> removeMemberFromRemoteConv rcnv lusr)

removeMemberFromRemoteConv ::
  Members
    '[ FederatorAccess,
       Error ActionError,
       Error ConversationError,
       Input UTCTime
     ]
    r =>
  Remote ConvId ->
  Local UserId ->
  Qualified UserId ->
  Sem r (Maybe Event)
removeMemberFromRemoteConv cnv lusr victim
  | qUntagged lusr == victim = do
    let lc = LeaveConversationRequest (tUnqualified cnv) (qUnqualified victim)
    let rpc = fedClient @'Galley @VL @"leave-conversation" lc
    (either handleError handleSuccess =<<) . fmap leaveResponse $
      E.runFederated cnv rpc
  | otherwise = throw (ActionDenied RemoveConversationMember)
  where
    handleError ::
      Members '[Error ActionError, Error ConversationError] r =>
      RemoveFromConversationError ->
      Sem r (Maybe Event)
    handleError RemoveFromConversationErrorRemovalNotAllowed =
      throw (ActionDenied RemoveConversationMember)
    handleError RemoveFromConversationErrorNotFound = throw ConvNotFound
    handleError RemoveFromConversationErrorUnchanged = pure Nothing

    handleSuccess :: Member (Input UTCTime) r => () -> Sem r (Maybe Event)
    handleSuccess _ = do
      t <- input
      pure . Just $
        Event MemberLeave (qUntagged cnv) (qUntagged lusr) t $
          EdMembersLeave (QualifiedUserIdList [victim])

-- | Remove a member from a local conversation.
removeMemberFromLocalConv ::
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error InvalidInput,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input UTCTime,
       MemberStore
     ]
    r =>
  Local ConvId ->
  Local UserId ->
  Maybe ConnId ->
  Qualified UserId ->
  Sem r (Maybe Event)
removeMemberFromLocalConv lcnv lusr con =
  fmap hush
    . runError @NoChanges
    . updateLocalConversation lcnv (qUntagged lusr) con
    . ConversationLeave
    . pure

-- OTR

data OtrResult
  = OtrSent !Public.ClientMismatch
  | OtrMissingRecipients !Public.ClientMismatch
  | OtrUnknownClient !UnknownClient
  | OtrConversationNotFound !ConvNotFound

handleOtrResult ::
  Members
    '[ Error ClientError,
       Error ConversationError
     ]
    r =>
  OtrResult ->
  Sem r Response
handleOtrResult =
  \case
    OtrSent m -> pure $ json m & setStatus status201
    OtrMissingRecipients m -> pure $ json m & setStatus status412
    OtrUnknownClient _ -> throw UnknownClient
    OtrConversationNotFound _ -> throw ConvNotFound

postBotMessageH ::
  Members
    '[ BrigAccess,
       ClientStore,
       ConversationStore,
       Error ClientError,
       Error ConversationError,
       Error LegalHoldError,
       GundeckAccess,
       ExternalAccess,
       Input (Local ()),
       Input Opts,
       Input UTCTime,
       MemberStore,
       TeamStore,
       TinyLog,
       WaiRoutes
     ]
    r =>
  BotId ::: ConvId ::: Public.OtrFilterMissing ::: JsonRequest Public.NewOtrMessage ::: JSON ->
  Sem r Response
postBotMessageH (zbot ::: cnv ::: val ::: req ::: _) = do
  lbot <- qualifyLocal zbot
  lcnv <- qualifyLocal cnv
  message <- fromJsonBody req
  let val' = allowOtrFilterMissingInBody val message
  handleOtrResult =<< postBotMessage lbot lcnv val' message

postBotMessage ::
  Members
    '[ BrigAccess,
       ClientStore,
       ConversationStore,
       Error LegalHoldError,
       ExternalAccess,
       GundeckAccess,
       Input Opts,
       Input UTCTime,
       MemberStore,
       TeamStore,
       TinyLog
     ]
    r =>
  Local BotId ->
  Local ConvId ->
  Public.OtrFilterMissing ->
  Public.NewOtrMessage ->
  Sem r OtrResult
postBotMessage zbot = postNewOtrMessage Bot (fmap botUserId zbot) Nothing

postProteusMessage ::
  Members
    '[ BotAccess,
       BrigAccess,
       ClientStore,
       ConversationStore,
       FederatorAccess,
       GundeckAccess,
       ExternalAccess,
       Input Opts,
       Input UTCTime,
       MemberStore,
       TeamStore,
       TinyLog
     ]
    r =>
  Local UserId ->
  ConnId ->
  Qualified ConvId ->
  RawProto Public.QualifiedNewOtrMessage ->
  Sem r (Public.PostOtrResponse Public.MessageSendingStatus)
postProteusMessage sender zcon conv msg = runLocalInput sender $ do
  foldQualified
    sender
    (\c -> postQualifiedOtrMessage User (qUntagged sender) (Just zcon) c (rpValue msg))
    (\c -> postRemoteOtrMessage (qUntagged sender) c (rpRaw msg))
    conv

postOtrMessageUnqualified ::
  Members
    '[ BotAccess,
       BrigAccess,
       ClientStore,
       ConversationStore,
       FederatorAccess,
       GundeckAccess,
       ExternalAccess,
       MemberStore,
       Input Opts,
       Input UTCTime,
       TeamStore,
       TinyLog
     ]
    r =>
  Local UserId ->
  ConnId ->
  ConvId ->
  Maybe Public.IgnoreMissing ->
  Maybe Public.ReportMissing ->
  Public.NewOtrMessage ->
  Sem r (Public.PostOtrResponse Public.ClientMismatch)
postOtrMessageUnqualified sender zcon cnv ignoreMissing reportMissing message = do
  let lcnv = qualifyAs sender cnv
      localDomain = tDomain sender
  let qualifiedRecipients =
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
  runLocalInput sender $
    unqualify localDomain
      <$> postQualifiedOtrMessage User (qUntagged sender) (Just zcon) lcnv qualifiedMessage

postProtoOtrBroadcastH ::
  Members
    '[ BrigAccess,
       ClientStore,
       Error ActionError,
       Error ClientError,
       Error ConversationError,
       Error LegalHoldError,
       Error TeamError,
       GundeckAccess,
       Input (Local ()),
       Input Opts,
       Input UTCTime,
       TeamStore,
       TinyLog,
       WaiRoutes
     ]
    r =>
  UserId ::: ConnId ::: Public.OtrFilterMissing ::: Request ::: JSON ->
  Sem r Response
postProtoOtrBroadcastH (zusr ::: zcon ::: val ::: req ::: _) = do
  lusr <- qualifyLocal zusr
  message <- Public.protoToNewOtrMessage <$> fromProtoBody req
  let val' = allowOtrFilterMissingInBody val message
  handleOtrResult =<< postOtrBroadcast lusr zcon val' message

postOtrBroadcastH ::
  Members
    '[ BrigAccess,
       ClientStore,
       Error ActionError,
       Error ClientError,
       Error ConversationError,
       Error LegalHoldError,
       Error TeamError,
       GundeckAccess,
       Input (Local ()),
       Input Opts,
       Input UTCTime,
       TeamStore,
       TinyLog,
       WaiRoutes
     ]
    r =>
  UserId ::: ConnId ::: Public.OtrFilterMissing ::: JsonRequest Public.NewOtrMessage ->
  Sem r Response
postOtrBroadcastH (zusr ::: zcon ::: val ::: req) = do
  lusr <- qualifyLocal zusr
  message <- fromJsonBody req
  let val' = allowOtrFilterMissingInBody val message
  handleOtrResult =<< postOtrBroadcast lusr zcon val' message

postOtrBroadcast ::
  Members
    '[ BrigAccess,
       ClientStore,
       Error ActionError,
       Error LegalHoldError,
       Error TeamError,
       GundeckAccess,
       Input Opts,
       Input UTCTime,
       TeamStore,
       TinyLog
     ]
    r =>
  Local UserId ->
  ConnId ->
  Public.OtrFilterMissing ->
  Public.NewOtrMessage ->
  Sem r OtrResult
postOtrBroadcast lusr zcon = postNewOtrBroadcast lusr (Just zcon)

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
  Members
    '[ BrigAccess,
       ClientStore,
       Error ActionError,
       Error LegalHoldError,
       Error TeamError,
       Input Opts,
       Input UTCTime,
       GundeckAccess,
       TeamStore,
       TinyLog
     ]
    r =>
  Local UserId ->
  Maybe ConnId ->
  OtrFilterMissing ->
  NewOtrMessage ->
  Sem r OtrResult
postNewOtrBroadcast lusr con val msg = do
  let sender = newOtrSender msg
      recvrs = newOtrRecipients msg
  now <- input
  withValidOtrBroadcastRecipients (tUnqualified lusr) sender recvrs val now $ \rs -> do
    let (_, toUsers) = foldr (newMessage (qUntagged lusr) con Nothing msg now) ([], []) rs
    E.push (catMaybes toUsers)

postNewOtrMessage ::
  Members
    '[ BrigAccess,
       ClientStore,
       ConversationStore,
       Error LegalHoldError,
       ExternalAccess,
       GundeckAccess,
       Input Opts,
       Input UTCTime,
       MemberStore,
       TeamStore,
       TinyLog
     ]
    r =>
  UserType ->
  Local UserId ->
  Maybe ConnId ->
  Local ConvId ->
  OtrFilterMissing ->
  NewOtrMessage ->
  Sem r OtrResult
postNewOtrMessage utype lusr con lcnv val msg = do
  let sender = newOtrSender msg
      recvrs = newOtrRecipients msg
  now <- input
  withValidOtrRecipients utype (tUnqualified lusr) sender (tUnqualified lcnv) recvrs val now $ \rs -> do
    let (toBots, toUsers) = foldr (newMessage (qUntagged lusr) con (Just (qUntagged lcnv)) msg now) ([], []) rs
    E.push (catMaybes toUsers)
    E.deliverAndDeleteAsync (tUnqualified lcnv) toBots

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
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error FederationError,
       Error InvalidInput,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input UTCTime
     ]
    r =>
  Local UserId ->
  ConnId ->
  Qualified ConvId ->
  Public.ConversationRename ->
  Sem r (Maybe Public.Event)
updateConversationName lusr zcon qcnv convRename = do
  foldQualified
    lusr
    (updateLocalConversationName lusr zcon)
    (\_ _ -> throw FederationNotImplemented)
    qcnv
    convRename

updateUnqualifiedConversationName ::
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error InvalidInput,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input UTCTime
     ]
    r =>
  Local UserId ->
  ConnId ->
  ConvId ->
  Public.ConversationRename ->
  Sem r (Maybe Public.Event)
updateUnqualifiedConversationName lusr zcon cnv rename = do
  let lcnv = qualifyAs lusr cnv
  updateLocalConversationName lusr zcon lcnv rename

updateLocalConversationName ::
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error InvalidInput,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input UTCTime
     ]
    r =>
  Local UserId ->
  ConnId ->
  Local ConvId ->
  Public.ConversationRename ->
  Sem r (Maybe Public.Event)
updateLocalConversationName lusr zcon lcnv convRename = do
  alive <- E.isConversationAlive (tUnqualified lcnv)
  if alive
    then updateLiveLocalConversationName lusr zcon lcnv convRename
    else Nothing <$ E.deleteConversation (tUnqualified lcnv)

updateLiveLocalConversationName ::
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error InvalidInput,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input UTCTime
     ]
    r =>
  Local UserId ->
  ConnId ->
  Local ConvId ->
  Public.ConversationRename ->
  Sem r (Maybe Public.Event)
updateLiveLocalConversationName lusr con lcnv rename =
  fmap hush . runError @NoChanges $
    updateLocalConversation lcnv (qUntagged lusr) (Just con) rename

isTypingH ::
  Members
    '[ Error ConversationError,
       GundeckAccess,
       Input (Local ()),
       Input UTCTime,
       MemberStore,
       WaiRoutes
     ]
    r =>
  UserId ::: ConnId ::: ConvId ::: JsonRequest Public.TypingData ->
  Sem r Response
isTypingH (zusr ::: zcon ::: cnv ::: req) = do
  lusr <- qualifyLocal zusr
  lcnv <- qualifyLocal cnv
  typingData <- fromJsonBody req
  isTyping lusr zcon lcnv typingData
  pure empty

isTyping ::
  Members '[Error ConversationError, GundeckAccess, Input UTCTime, MemberStore] r =>
  Local UserId ->
  ConnId ->
  Local ConvId ->
  Public.TypingData ->
  Sem r ()
isTyping lusr zcon lcnv typingData = do
  mm <- E.getLocalMembers (tUnqualified lcnv)
  unless (tUnqualified lusr `isMember` mm) . throw $ ConvNotFound
  now <- input
  let e = Event Typing (qUntagged lcnv) (qUntagged lusr) now (EdTyping typingData)
  for_ (newPushLocal ListComplete (tUnqualified lusr) (ConvEvent e) (recipient <$> mm)) $ \p ->
    E.push1 $
      p
        & pushConn ?~ zcon
        & pushRoute .~ RouteDirect
        & pushTransient .~ True

addServiceH ::
  Members
    '[ ServiceStore,
       WaiRoutes
     ]
    r =>
  JsonRequest Service ->
  Sem r Response
addServiceH req = do
  E.createService =<< fromJsonBody req
  return empty

rmServiceH ::
  Members '[ServiceStore, WaiRoutes] r =>
  JsonRequest ServiceRef ->
  Sem r Response
rmServiceH req = do
  E.deleteService =<< fromJsonBody req
  return empty

addBotH ::
  Members
    '[ ClientStore,
       ConversationStore,
       Error ActionError,
       Error InvalidInput,
       Error ConversationError,
       ExternalAccess,
       GundeckAccess,
       Input (Local ()),
       Input Opts,
       Input UTCTime,
       MemberStore,
       TeamStore,
       WaiRoutes
     ]
    r =>
  UserId ::: ConnId ::: JsonRequest AddBot ->
  Sem r Response
addBotH (zusr ::: zcon ::: req) = do
  bot <- fromJsonBody req
  lusr <- qualifyLocal zusr
  json <$> addBot lusr zcon bot

addBot ::
  forall r.
  Members
    '[ ClientStore,
       ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error InvalidInput,
       ExternalAccess,
       GundeckAccess,
       Input Opts,
       Input UTCTime,
       MemberStore,
       TeamStore
     ]
    r =>
  Local UserId ->
  ConnId ->
  AddBot ->
  Sem r Event
addBot lusr zcon b = do
  c <-
    E.getConversation (b ^. addBotConv) >>= note ConvNotFound
  -- Check some preconditions on adding bots to a conversation
  for_ (Data.convTeam c) $ teamConvChecks (b ^. addBotConv)
  (bots, users) <- regularConvChecks c
  t <- input
  E.createClient (botUserId (b ^. addBotId)) (b ^. addBotClient)
  bm <- E.createBotMember (b ^. addBotService) (b ^. addBotId) (b ^. addBotConv)
  let e =
        Event
          MemberJoin
          (qUntagged (qualifyAs lusr (b ^. addBotConv)))
          (qUntagged lusr)
          t
          ( EdMembersJoin
              ( SimpleMembers
                  [ SimpleMember
                      (qUntagged (qualifyAs lusr (botUserId (botMemId bm))))
                      roleNameWireAdmin
                  ]
              )
          )
  for_ (newPushLocal ListComplete (tUnqualified lusr) (ConvEvent e) (recipient <$> users)) $ \p ->
    E.push1 $ p & pushConn ?~ zcon
  E.deliverAsync ((bm : bots) `zip` repeat e)
  pure e
  where
    regularConvChecks c = do
      let (bots, users) = localBotsAndUsers (Data.convLocalMembers c)
      unless (tUnqualified lusr `isMember` users) . throw $ ConvNotFound
      ensureGroupConversation c
      self <- getSelfMemberFromLocals (tUnqualified lusr) users
      ensureActionAllowed AddConversationMember self
      unless (any ((== b ^. addBotId) . botMemId) bots) $ do
        let botId = qualifyAs lusr (botUserId (b ^. addBotId))
        ensureMemberLimit (toList $ Data.convLocalMembers c) [qUntagged botId]
      return (bots, users)
    teamConvChecks :: ConvId -> TeamId -> Sem r ()
    teamConvChecks cid tid = do
      tcv <- E.getTeamConversation tid cid
      when (maybe True (view managedConversation) tcv) $
        throw NoAddToManaged

rmBotH ::
  Members
    '[ ClientStore,
       ConversationStore,
       Error ConversationError,
       ExternalAccess,
       GundeckAccess,
       Input (Local ()),
       Input UTCTime,
       MemberStore,
       WaiRoutes
     ]
    r =>
  UserId ::: Maybe ConnId ::: JsonRequest RemoveBot ->
  Sem r Response
rmBotH (zusr ::: zcon ::: req) = do
  lusr <- qualifyLocal zusr
  bot <- fromJsonBody req
  handleUpdateResult <$> rmBot lusr zcon bot

rmBot ::
  Members
    '[ ClientStore,
       ConversationStore,
       Error ConversationError,
       ExternalAccess,
       GundeckAccess,
       Input UTCTime,
       MemberStore
     ]
    r =>
  Local UserId ->
  Maybe ConnId ->
  RemoveBot ->
  Sem r (UpdateResult Event)
rmBot lusr zcon b = do
  c <-
    E.getConversation (b ^. rmBotConv) >>= note ConvNotFound
  let lcnv = qualifyAs lusr (Data.convId c)
  unless (tUnqualified lusr `isMember` Data.convLocalMembers c) $
    throw ConvNotFound
  let (bots, users) = localBotsAndUsers (Data.convLocalMembers c)
  if not (any ((== b ^. rmBotId) . botMemId) bots)
    then pure Unchanged
    else do
      t <- input
      do
        let evd = EdMembersLeave (QualifiedUserIdList [qUntagged (qualifyAs lusr (botUserId (b ^. rmBotId)))])
        let e = Event MemberLeave (qUntagged lcnv) (qUntagged lusr) t evd
        for_ (newPushLocal ListComplete (tUnqualified lusr) (ConvEvent e) (recipient <$> users)) $ \p ->
          E.push1 $ p & pushConn .~ zcon
        E.deleteMembers (Data.convId c) (UserList [botUserId (b ^. rmBotId)] [])
        E.deleteClients (botUserId (b ^. rmBotId))
        E.deliverAsync (bots `zip` repeat e)
        pure $ Updated e

-------------------------------------------------------------------------------
-- Helpers

ensureConvMember :: Member (Error ConversationError) r => [LocalMember] -> UserId -> Sem r ()
ensureConvMember users usr =
  unless (usr `isMember` users) $ throw ConvNotFound

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
  forall r.
  Members
    '[ BrigAccess,
       ClientStore,
       Error ActionError,
       Error LegalHoldError,
       Error TeamError,
       Input Opts,
       TeamStore,
       TinyLog
     ]
    r =>
  UserId ->
  ClientId ->
  OtrRecipients ->
  OtrFilterMissing ->
  UTCTime ->
  ([(LocalMember, ClientId, Text)] -> Sem r ()) ->
  Sem r OtrResult
withValidOtrBroadcastRecipients usr clt rcps val now go = withBindingTeam usr $ \tid -> do
  limit <- fromIntegral . fromRange <$> E.fanoutLimit
  -- If we are going to fan this out to more than limit, we want to fail early
  unless (Map.size (userClientMap (otrRecipientsMap rcps)) <= limit) $
    throw BroadcastLimitExceeded
  -- In large teams, we may still use the broadcast endpoint but only if `report_missing`
  -- is used and length `report_missing` < limit since we cannot fetch larger teams than
  -- that.
  tMembers <-
    fmap (view userId) <$> case val of
      OtrReportMissing us -> maybeFetchLimitedTeamMemberList limit tid us
      _ -> maybeFetchAllMembersInTeam tid
  contacts <- E.getContactList usr
  let users = Set.toList $ Set.union (Set.fromList tMembers) (Set.fromList contacts)
  isInternal <- E.useIntraClientListing
  clts <-
    if isInternal
      then Clients.fromUserClients <$> E.lookupClients users
      else E.getClients users
  let membs = newMember <$> users
  handleOtrResponse User usr clt rcps membs clts val now go
  where
    maybeFetchLimitedTeamMemberList limit tid uListInFilter = do
      -- Get the users in the filter (remote ids are not in a local team)
      let localUserIdsInFilter = toList uListInFilter
      let localUserIdsInRcps = Map.keys $ userClientMap (otrRecipientsMap rcps)
      let localUserIdsToLookup = Set.toList $ Set.union (Set.fromList localUserIdsInFilter) (Set.fromList localUserIdsInRcps)
      unless (length localUserIdsToLookup <= limit) $
        throw BroadcastLimitExceeded
      E.selectTeamMembers tid localUserIdsToLookup
    maybeFetchAllMembersInTeam :: TeamId -> Sem r [TeamMember]
    maybeFetchAllMembersInTeam tid = do
      mems <- getTeamMembersForFanout tid
      when (mems ^. teamMemberListType == ListTruncated) $
        throw BroadcastLimitExceeded
      pure (mems ^. teamMembers)

withValidOtrRecipients ::
  Members
    '[ BrigAccess,
       ClientStore,
       ConversationStore,
       Error LegalHoldError,
       Input Opts,
       MemberStore,
       TeamStore,
       TinyLog
     ]
    r =>
  UserType ->
  UserId ->
  ClientId ->
  ConvId ->
  OtrRecipients ->
  OtrFilterMissing ->
  UTCTime ->
  ([(LocalMember, ClientId, Text)] -> Sem r ()) ->
  Sem r OtrResult
withValidOtrRecipients utype usr clt cnv rcps val now go = do
  alive <- E.isConversationAlive cnv
  if not alive
    then do
      E.deleteConversation cnv
      pure $ OtrConversationNotFound mkErrorDescription
    else do
      localMembers <- E.getLocalMembers cnv
      let localMemberIds = lmId <$> localMembers
      isInternal <- E.useIntraClientListing
      clts <-
        if isInternal
          then Clients.fromUserClients <$> E.lookupClients localMemberIds
          else E.getClients localMemberIds
      handleOtrResponse utype usr clt rcps localMembers clts val now go

handleOtrResponse ::
  Members '[BrigAccess, Error LegalHoldError, Input Opts, TeamStore, TinyLog] r =>
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
  ([(LocalMember, ClientId, Text)] -> Sem r ()) ->
  Sem r OtrResult
handleOtrResponse utype usr clt rcps membs clts val now go = case checkOtrRecipients usr clt rcps membs clts val now of
  ValidOtrRecipients m r -> go r >> pure (OtrSent m)
  MissingOtrRecipients m -> mapError @LegalholdConflicts (const MissingLegalholdConsent) $ do
    guardLegalholdPolicyConflicts (userToProtectee utype usr) (missingClients m)
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
withBindingTeam ::
  Members
    '[ Error TeamError,
       TeamStore
     ]
    r =>
  UserId ->
  (TeamId -> Sem r b) ->
  Sem r b
withBindingTeam zusr callback = do
  tid <- E.getOneUserTeam zusr >>= note TeamNotFound
  binding <- E.getTeamBinding tid >>= note TeamNotFound
  case binding of
    Binding -> callback tid
    NonBinding -> throw NotABindingTeamMember
