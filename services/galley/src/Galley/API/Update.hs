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

import Control.Lens
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Code
import Data.Either.Extra (mapRight)
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
import Galley.API.LegalHold.Conflicts (guardLegalholdPolicyConflicts)
import Galley.API.Mapping
import Galley.API.Message
import Galley.API.Util
import Galley.App
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
import qualified Wire.API.Conversation as Public
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
import Wire.API.Federation.Client
import qualified Wire.API.Message as Public
import Wire.API.Routes.Public.Galley.Responses
import Wire.API.Routes.Public.Util (UpdateResult (..))
import Wire.API.ServantProto (RawProto (..))
import Wire.API.User.Client

acceptConvH ::
  Members '[ConversationStore, GundeckAccess, MemberStore, WaiError] r =>
  UserId ::: Maybe ConnId ::: ConvId ->
  Galley r Response
acceptConvH (usr ::: conn ::: cnv) =
  setStatus status200 . json <$> acceptConv usr conn cnv

acceptConv ::
  Members '[ConversationStore, GundeckAccess, MemberStore, WaiError] r =>
  UserId ->
  Maybe ConnId ->
  ConvId ->
  Galley r Conversation
acceptConv usr conn cnv = do
  conv <-
    liftSem (E.getConversation cnv)
      >>= ifNothing (errorDescriptionTypeToWai @ConvNotFound)
  conv' <- acceptOne2One usr conv conn
  conversationView usr conv'

blockConvH ::
  Members '[ConversationStore, MemberStore, WaiError] r =>
  UserId ::: ConvId ->
  Galley r Response
blockConvH (zusr ::: cnv) =
  empty <$ blockConv zusr cnv

blockConv ::
  Members '[ConversationStore, MemberStore, WaiError] r =>
  UserId ->
  ConvId ->
  Galley r ()
blockConv zusr cnv = do
  conv <-
    liftSem (E.getConversation cnv)
      >>= ifNothing (errorDescriptionTypeToWai @ConvNotFound)
  unless (Data.convType conv `elem` [ConnectConv, One2OneConv]) $
    liftSem . throw $
      invalidOp "block: invalid conversation type"
  let mems = Data.convLocalMembers conv
  when (zusr `isMember` mems) . liftSem $
    E.deleteMembers cnv (UserList [zusr] [])

unblockConvH ::
  Members '[ConversationStore, GundeckAccess, MemberStore, WaiError] r =>
  UserId ::: Maybe ConnId ::: ConvId ->
  Galley r Response
unblockConvH (usr ::: conn ::: cnv) =
  setStatus status200 . json <$> unblockConv usr conn cnv

unblockConv ::
  Members '[ConversationStore, GundeckAccess, MemberStore, WaiError] r =>
  UserId ->
  Maybe ConnId ->
  ConvId ->
  Galley r Conversation
unblockConv usr conn cnv = do
  conv <-
    liftSem (E.getConversation cnv)
      >>= ifNothing (errorDescriptionTypeToWai @ConvNotFound)
  unless (Data.convType conv `elem` [ConnectConv, One2OneConv]) $
    liftSem . throw $
      invalidOp "unblock: invalid conversation type"
  conv' <- acceptOne2One usr conv conn
  conversationView usr conv'

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
       ExternalAccess,
       FederatorAccess,
       FireAndForget,
       GundeckAccess,
       MemberStore,
       TeamStore
     ]
    r =>
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
  Members
    '[ BotAccess,
       BrigAccess,
       CodeStore,
       ConversationStore,
       Error ActionError,
       Error ConversationError,
       ExternalAccess,
       FederatorAccess,
       FireAndForget,
       GundeckAccess,
       MemberStore,
       TeamStore
     ]
    r =>
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
  Members
    '[ BotAccess,
       BrigAccess,
       CodeStore,
       ConversationStore,
       Error ActionError,
       Error ConversationError,
       ExternalAccess,
       FederatorAccess,
       FireAndForget,
       GundeckAccess,
       MemberStore,
       TeamStore
     ]
    r =>
  Local ConvId ->
  Local UserId ->
  ConnId ->
  Public.ConversationAccessData ->
  Galley r (UpdateResult Event)
updateLocalConversationAccess lcnv lusr con =
  getUpdateResult
    . updateLocalConversation lcnv (qUntagged lusr) (Just con)

updateRemoteConversationAccess ::
  Member (Error FederationError) r =>
  Remote ConvId ->
  Local UserId ->
  ConnId ->
  Public.ConversationAccessData ->
  Galley r (UpdateResult Event)
updateRemoteConversationAccess _ _ _ _ = liftSem $ throw FederationNotImplemented

updateConversationReceiptMode ::
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess
     ]
    r =>
  Members '[Error FederationError] r =>
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
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess
     ]
    r =>
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
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess
     ]
    r =>
  Local ConvId ->
  Local UserId ->
  ConnId ->
  Public.ConversationReceiptModeUpdate ->
  Galley r (UpdateResult Event)
updateLocalConversationReceiptMode lcnv lusr con update =
  getUpdateResult $
    updateLocalConversation lcnv (qUntagged lusr) (Just con) update

updateRemoteConversationReceiptMode ::
  Member (Error FederationError) r =>
  Remote ConvId ->
  Local UserId ->
  ConnId ->
  Public.ConversationReceiptModeUpdate ->
  Galley r (UpdateResult Event)
updateRemoteConversationReceiptMode _ _ _ _ = liftSem $ throw FederationNotImplemented

updateConversationMessageTimerUnqualified ::
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess
     ]
    r =>
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
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error FederationError,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess
     ]
    r =>
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
    (\_ _ -> liftSem (throw FederationNotImplemented))
    qcnv
    update

updateLocalConversationMessageTimer ::
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess
     ]
    r =>
  Local UserId ->
  ConnId ->
  Local ConvId ->
  Public.ConversationMessageTimerUpdate ->
  Galley r (UpdateResult Event)
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
       Error TeamError,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       TeamStore
     ]
    r =>
  Members '[] r =>
  Local UserId ->
  ConnId ->
  Local ConvId ->
  Galley r (UpdateResult Event)
deleteLocalConversation lusr con lcnv =
  getUpdateResult $
    updateLocalConversation lcnv (qUntagged lusr) (Just con) ConversationDelete

getUpdateResult :: Functor m => MaybeT m a -> m (UpdateResult a)
getUpdateResult = fmap (maybe Unchanged Updated) . runMaybeT

addCodeH ::
  Members
    '[ CodeStore,
       ConversationStore,
       Error ConversationError,
       ExternalAccess,
       GundeckAccess,
       WaiError
     ]
    r =>
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
  Members
    '[ CodeStore,
       ConversationStore,
       Error ConversationError,
       ExternalAccess,
       GundeckAccess,
       WaiError
     ]
    r =>
  UserId ->
  ConnId ->
  ConvId ->
  Galley r AddCodeResult
addCode usr zcon cnv = do
  localDomain <- viewFederationDomain
  let qcnv = Qualified cnv localDomain
      qusr = Qualified usr localDomain
  conv <-
    liftSem (E.getConversation cnv)
      >>= ifNothing (errorDescriptionTypeToWai @ConvNotFound)
  ensureConvMember (Data.convLocalMembers conv) usr
  liftSem $ ensureAccess conv CodeAccess
  let (bots, users) = localBotsAndUsers $ Data.convLocalMembers conv
  key <- mkKey cnv
  mCode <- liftSem $ E.getCode key ReusableCode
  case mCode of
    Nothing -> do
      code <- generate cnv ReusableCode (Timeout 3600 * 24 * 365) -- one year TODO: configurable
      liftSem $ E.createCode code
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
  Members
    '[ CodeStore,
       ConversationStore,
       ExternalAccess,
       Error ConversationError,
       GundeckAccess,
       WaiError
     ]
    r =>
  UserId ::: ConnId ::: ConvId ->
  Galley r Response
rmCodeH (usr ::: zcon ::: cnv) =
  setStatus status200 . json <$> rmCode usr zcon cnv

rmCode ::
  Members
    '[ CodeStore,
       ConversationStore,
       Error ConversationError,
       ExternalAccess,
       GundeckAccess,
       WaiError
     ]
    r =>
  UserId ->
  ConnId ->
  ConvId ->
  Galley r Public.Event
rmCode usr zcon cnv = do
  localDomain <- viewFederationDomain
  let qcnv = Qualified cnv localDomain
      qusr = Qualified usr localDomain
  conv <-
    liftSem (E.getConversation cnv)
      >>= ifNothing (errorDescriptionTypeToWai @ConvNotFound)
  ensureConvMember (Data.convLocalMembers conv) usr
  liftSem $ ensureAccess conv CodeAccess
  let (bots, users) = localBotsAndUsers $ Data.convLocalMembers conv
  key <- mkKey cnv
  liftSem $ E.deleteCode key ReusableCode
  now <- liftIO getCurrentTime
  let event = Event ConvCodeDelete qcnv qusr now EdConvCodeDelete
  pushConversationEvent (Just zcon) event (map lmId users) bots
  pure event

getCodeH ::
  Members
    '[ CodeStore,
       ConversationStore,
       Error ConversationError,
       WaiError
     ]
    r =>
  UserId ::: ConvId ->
  Galley r Response
getCodeH (usr ::: cnv) =
  setStatus status200 . json <$> getCode usr cnv

getCode ::
  Members
    '[ CodeStore,
       ConversationStore,
       Error ConversationError,
       WaiError
     ]
    r =>
  UserId ->
  ConvId ->
  Galley r Public.ConversationCode
getCode usr cnv = do
  conv <-
    liftSem (E.getConversation cnv)
      >>= ifNothing (errorDescriptionTypeToWai @ConvNotFound)
  liftSem $ ensureAccess conv CodeAccess
  ensureConvMember (Data.convLocalMembers conv) usr
  key <- mkKey cnv
  c <-
    liftSem (E.getCode key ReusableCode)
      >>= ifNothing (errorDescriptionTypeToWai @CodeNotFound)
  returnCode c

returnCode :: Code -> Galley r Public.ConversationCode
returnCode c = do
  urlPrefix <- view $ options . optSettings . setConversationCodeURI
  pure $ Public.mkConversationCode (codeKey c) (codeValue c) urlPrefix

checkReusableCodeH ::
  Members '[CodeStore, WaiError] r =>
  JsonRequest Public.ConversationCode ->
  Galley r Response
checkReusableCodeH req = do
  convCode <- fromJsonBody req
  checkReusableCode convCode
  pure empty

checkReusableCode ::
  Members '[CodeStore, WaiError] r =>
  Public.ConversationCode ->
  Galley r ()
checkReusableCode convCode =
  void $ verifyReusableCode convCode

joinConversationByReusableCodeH ::
  Members
    '[ BrigAccess,
       CodeStore,
       ConversationStore,
       FederatorAccess,
       Error ActionError,
       Error ConversationError,
       Error FederationError,
       Error TeamError,
       ExternalAccess,
       GundeckAccess,
       MemberStore,
       TeamStore,
       WaiError
     ]
    r =>
  UserId ::: ConnId ::: JsonRequest Public.ConversationCode ->
  Galley r Response
joinConversationByReusableCodeH (zusr ::: zcon ::: req) = do
  convCode <- fromJsonBody req
  handleUpdateResult <$> joinConversationByReusableCode zusr zcon convCode

joinConversationByReusableCode ::
  Members
    '[ BrigAccess,
       CodeStore,
       ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error FederationError,
       Error TeamError,
       FederatorAccess,
       ExternalAccess,
       GundeckAccess,
       MemberStore,
       TeamStore,
       WaiError
     ]
    r =>
  UserId ->
  ConnId ->
  Public.ConversationCode ->
  Galley r (UpdateResult Event)
joinConversationByReusableCode zusr zcon convCode = do
  c <- verifyReusableCode convCode
  joinConversation zusr zcon (codeConversation c) CodeAccess

joinConversationByIdH ::
  Members
    '[ BrigAccess,
       ConversationStore,
       FederatorAccess,
       Error ActionError,
       Error ConversationError,
       Error FederationError,
       Error TeamError,
       ExternalAccess,
       GundeckAccess,
       MemberStore,
       TeamStore,
       WaiError
     ]
    r =>
  UserId ::: ConnId ::: ConvId ::: JSON ->
  Galley r Response
joinConversationByIdH (zusr ::: zcon ::: cnv ::: _) =
  handleUpdateResult <$> joinConversationById zusr zcon cnv

joinConversationById ::
  Members
    '[ BrigAccess,
       FederatorAccess,
       ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error FederationError,
       Error TeamError,
       ExternalAccess,
       GundeckAccess,
       MemberStore,
       TeamStore,
       WaiError
     ]
    r =>
  UserId ->
  ConnId ->
  ConvId ->
  Galley r (UpdateResult Event)
joinConversationById zusr zcon cnv =
  joinConversation zusr zcon cnv LinkAccess

joinConversation ::
  Members
    '[ BrigAccess,
       ConversationStore,
       FederatorAccess,
       Error ActionError,
       Error ConversationError,
       Error FederationError,
       Error TeamError,
       ExternalAccess,
       GundeckAccess,
       MemberStore,
       TeamStore,
       WaiError
     ]
    r =>
  UserId ->
  ConnId ->
  ConvId ->
  Access ->
  Galley r (UpdateResult Event)
joinConversation zusr zcon cnv access = do
  lusr <- qualifyLocal zusr
  lcnv <- qualifyLocal cnv
  conv <- ensureConversationAccess zusr cnv access
  liftSem . mapError toWai $ ensureGroupConversation conv
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
       Error LegalHoldError,
       Error TeamError,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       LegalHoldStore,
       MemberStore,
       TeamStore
     ]
    r =>
  UserId ->
  ConnId ->
  ConvId ->
  Public.Invite ->
  Galley r (UpdateResult Event)
addMembersUnqualified zusr zcon cnv (Public.Invite users role) = do
  qusers <- traverse (fmap qUntagged . qualifyLocal) (toNonEmpty users)
  addMembers zusr zcon cnv (Public.InviteQualified qusers role)

addMembers ::
  Members
    '[ BrigAccess,
       ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error FederationError,
       Error LegalHoldError,
       Error TeamError,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       LegalHoldStore,
       MemberStore,
       TeamStore
     ]
    r =>
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
      ConversationJoin users role

updateSelfMember ::
  Members '[ConversationStore, GundeckAccess, ExternalAccess, MemberStore, WaiError] r =>
  UserId ->
  ConnId ->
  Qualified ConvId ->
  Public.MemberUpdate ->
  Galley r ()
updateSelfMember zusr zcon qcnv update = do
  lusr <- qualifyLocal zusr
  exists <- liftSem $ foldQualified lusr checkLocalMembership checkRemoteMembership qcnv lusr
  liftSem $ unless exists (throwErrorDescriptionType @ConvNotFound)
  liftSem $ E.setSelfMember qcnv lusr update
  now <- liftIO getCurrentTime
  let e = Event MemberStateUpdate qcnv (qUntagged lusr) now (EdMemberUpdate (updateData lusr))
  pushConversationEvent (Just zcon) e [zusr] []
  where
    checkLocalMembership ::
      Members '[MemberStore] r =>
      Local ConvId ->
      Local UserId ->
      Sem r Bool
    checkLocalMembership lcnv lusr =
      isMember (tUnqualified lusr)
        <$> E.getLocalMembers (tUnqualified lcnv)
    checkRemoteMembership ::
      Members '[ConversationStore] r =>
      Remote ConvId ->
      Local UserId ->
      Sem r Bool
    checkRemoteMembership rcnv lusr =
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
  Members '[ConversationStore, GundeckAccess, ExternalAccess, MemberStore, WaiError] r =>
  UserId ->
  ConnId ->
  ConvId ->
  Public.MemberUpdate ->
  Galley r ()
updateUnqualifiedSelfMember zusr zcon cnv update = do
  lcnv <- qualifyLocal cnv
  updateSelfMember zusr zcon (qUntagged lcnv) update

updateOtherMemberUnqualified ::
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       MemberStore
     ]
    r =>
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
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       Error FederationError,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       MemberStore
     ]
    r =>
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
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       MemberStore
     ]
    r =>
  Local ConvId ->
  Local UserId ->
  ConnId ->
  Qualified UserId ->
  Public.OtherMemberUpdate ->
  Galley r ()
updateOtherMemberLocalConv lcnv lusr con qvictim update = void . getUpdateResult $ do
  lift . liftSem . when (qUntagged lusr == qvictim) $
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
  Galley r ()
updateOtherMemberRemoteConv _ _ _ _ _ = liftSem $ throw FederationNotImplemented

removeMemberUnqualified ::
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       MemberStore
     ]
    r =>
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
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       MemberStore
     ]
    r =>
  UserId ->
  ConnId ->
  Qualified ConvId ->
  Qualified UserId ->
  Galley r RemoveFromConversationResponse
removeMemberQualified zusr con qcnv victim = do
  lusr <- qualifyLocal zusr
  foldQualified lusr removeMemberFromLocalConv removeMemberFromRemoteConv qcnv lusr (Just con) victim

removeMemberFromRemoteConv ::
  Members '[FederatorAccess] r =>
  Remote ConvId ->
  Local UserId ->
  Maybe ConnId ->
  Qualified UserId ->
  Galley r RemoveFromConversationResponse
removeMemberFromRemoteConv cnv lusr _ victim
  | qUntagged lusr == victim =
    do
      let lc = FederatedGalley.LeaveConversationRequest (tUnqualified cnv) (qUnqualified victim)
      let rpc =
            FederatedGalley.leaveConversation
              FederatedGalley.clientRoutes
              (qDomain victim)
              lc
      t <- liftIO getCurrentTime
      let successEvent =
            Event MemberLeave (qUntagged cnv) (qUntagged lusr) t $
              EdMembersLeave (QualifiedUserIdList [victim])
      liftSem $
        mapRight (const successEvent) . FederatedGalley.leaveResponse
          <$> E.runFederated cnv rpc
  | otherwise = pure . Left $ RemoveFromConversationErrorRemovalNotAllowed

-- | Remove a member from a local conversation.
removeMemberFromLocalConv ::
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       MemberStore
     ]
    r =>
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
    . ConversationLeave
    . pure
    $ victim

-- OTR

data OtrResult
  = OtrSent !Public.ClientMismatch
  | OtrMissingRecipients !Public.ClientMismatch
  | OtrUnknownClient !Public.UnknownClient
  | OtrConversationNotFound !Public.ConvNotFound

handleOtrResult :: Member WaiError r => OtrResult -> Galley r Response
handleOtrResult =
  liftSem . \case
    OtrSent m -> pure $ json m & setStatus status201
    OtrMissingRecipients m -> pure $ json m & setStatus status412
    OtrUnknownClient _ -> throwErrorDescriptionType @UnknownClient
    OtrConversationNotFound _ -> throwErrorDescriptionType @ConvNotFound

postBotMessageH ::
  Members
    '[ BotAccess,
       BrigAccess,
       ClientStore,
       ConversationStore,
       FederatorAccess,
       GundeckAccess,
       ExternalAccess,
       MemberStore,
       TeamStore,
       WaiError
     ]
    r =>
  BotId ::: ConvId ::: Public.OtrFilterMissing ::: JsonRequest Public.NewOtrMessage ::: JSON ->
  Galley r Response
postBotMessageH (zbot ::: zcnv ::: val ::: req ::: _) = do
  message <- fromJsonBody req
  let val' = allowOtrFilterMissingInBody val message
  handleOtrResult =<< postBotMessage zbot zcnv val' message

postBotMessage ::
  Members
    '[ BotAccess,
       BrigAccess,
       ClientStore,
       ConversationStore,
       FederatorAccess,
       GundeckAccess,
       ExternalAccess,
       MemberStore,
       TeamStore,
       WaiError
     ]
    r =>
  BotId ->
  ConvId ->
  Public.OtrFilterMissing ->
  Public.NewOtrMessage ->
  Galley r OtrResult
postBotMessage zbot = postNewOtrMessage Bot (botUserId zbot) Nothing

postProteusMessage ::
  Members
    '[ BotAccess,
       BrigAccess,
       ClientStore,
       ConversationStore,
       FederatorAccess,
       GundeckAccess,
       ExternalAccess,
       MemberStore,
       TeamStore,
       WaiError
     ]
    r =>
  UserId ->
  ConnId ->
  Qualified ConvId ->
  RawProto Public.QualifiedNewOtrMessage ->
  Galley r (Public.PostOtrResponse Public.MessageSendingStatus)
postProteusMessage zusr zcon conv msg = do
  sender <- qualifyLocal zusr
  foldQualified
    sender
    (\c -> postQualifiedOtrMessage User (qUntagged sender) (Just zcon) (tUnqualified c) (rpValue msg))
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
       TeamStore,
       WaiError
     ]
    r =>
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
  Members '[BrigAccess, ClientStore, GundeckAccess, TeamStore, WaiError] r =>
  UserId ::: ConnId ::: Public.OtrFilterMissing ::: Request ::: JSON ->
  Galley r Response
postProtoOtrBroadcastH (zusr ::: zcon ::: val ::: req ::: _) = do
  message <- Public.protoToNewOtrMessage <$> fromProtoBody req
  let val' = allowOtrFilterMissingInBody val message
  handleOtrResult =<< postOtrBroadcast zusr zcon val' message

postOtrBroadcastH ::
  Members '[BrigAccess, ClientStore, GundeckAccess, TeamStore, WaiError] r =>
  UserId ::: ConnId ::: Public.OtrFilterMissing ::: JsonRequest Public.NewOtrMessage ->
  Galley r Response
postOtrBroadcastH (zusr ::: zcon ::: val ::: req) = do
  message <- fromJsonBody req
  let val' = allowOtrFilterMissingInBody val message
  handleOtrResult =<< postOtrBroadcast zusr zcon val' message

postOtrBroadcast ::
  Members '[BrigAccess, ClientStore, GundeckAccess, TeamStore, WaiError] r =>
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
  Members '[BrigAccess, ClientStore, GundeckAccess, TeamStore, WaiError] r =>
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
    liftSem $ E.push (catMaybes toUsers)

postNewOtrMessage ::
  Members
    '[ BotAccess,
       BrigAccess,
       ClientStore,
       ConversationStore,
       ExternalAccess,
       GundeckAccess,
       MemberStore,
       TeamStore,
       WaiError
     ]
    r =>
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
  withValidOtrRecipients utype usr sender cnv recvrs val now $ \rs -> liftSem $ do
    let (toBots, toUsers) = foldr (newMessage qusr con (Just qcnv) msg now) ([], []) rs
    E.push (catMaybes toUsers)
    E.deliverAndDeleteAsync cnv toBots

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
       ExternalAccess,
       FederatorAccess,
       GundeckAccess
     ]
    r =>
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
    (\_ _ -> liftSem (throw FederationNotImplemented))
    qcnv
    convRename

updateUnqualifiedConversationName ::
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess
     ]
    r =>
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
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess
     ]
    r =>
  Local UserId ->
  ConnId ->
  Local ConvId ->
  Public.ConversationRename ->
  Galley r (Maybe Public.Event)
updateLocalConversationName lusr zcon lcnv convRename = do
  alive <- liftSem $ E.isConversationAlive (tUnqualified lcnv)
  if alive
    then updateLiveLocalConversationName lusr zcon lcnv convRename
    else liftSem $ Nothing <$ E.deleteConversation (tUnqualified lcnv)

updateLiveLocalConversationName ::
  Members
    '[ ConversationStore,
       Error ActionError,
       Error ConversationError,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess
     ]
    r =>
  Local UserId ->
  ConnId ->
  Local ConvId ->
  Public.ConversationRename ->
  Galley r (Maybe Public.Event)
updateLiveLocalConversationName lusr con lcnv rename =
  runMaybeT $
    updateLocalConversation lcnv (qUntagged lusr) (Just con) rename

isTypingH ::
  Members '[GundeckAccess, MemberStore, WaiError] r =>
  UserId ::: ConnId ::: ConvId ::: JsonRequest Public.TypingData ->
  Galley r Response
isTypingH (zusr ::: zcon ::: cnv ::: req) = do
  typingData <- fromJsonBody req
  isTyping zusr zcon cnv typingData
  pure empty

isTyping ::
  Members '[GundeckAccess, MemberStore, WaiError] r =>
  UserId ->
  ConnId ->
  ConvId ->
  Public.TypingData ->
  Galley r ()
isTyping zusr zcon cnv typingData = do
  localDomain <- viewFederationDomain
  let qcnv = Qualified cnv localDomain
      qusr = Qualified zusr localDomain
  mm <- liftSem $ E.getLocalMembers cnv
  liftSem . unless (zusr `isMember` mm) $
    throwErrorDescriptionType @ConvNotFound
  now <- liftIO getCurrentTime
  let e = Event Typing qcnv qusr now (EdTyping typingData)
  for_ (newPushLocal ListComplete zusr (ConvEvent e) (recipient <$> mm)) $ \p ->
    liftSem . E.push1 $
      p
        & pushConn ?~ zcon
        & pushRoute .~ RouteDirect
        & pushTransient .~ True

addServiceH :: Members '[ServiceStore, WaiError] r => JsonRequest Service -> Galley r Response
addServiceH req = do
  liftSem . E.createService =<< fromJsonBody req
  return empty

rmServiceH :: Members '[ServiceStore, WaiError] r => JsonRequest ServiceRef -> Galley r Response
rmServiceH req = do
  liftSem . E.deleteService =<< fromJsonBody req
  return empty

addBotH ::
  Members
    '[ ClientStore,
       ConversationStore,
       Error ActionError,
       Error ConversationError,
       ExternalAccess,
       GundeckAccess,
       MemberStore,
       TeamStore,
       WaiError
     ]
    r =>
  UserId ::: ConnId ::: JsonRequest AddBot ->
  Galley r Response
addBotH (zusr ::: zcon ::: req) = do
  bot <- fromJsonBody req
  json <$> addBot zusr zcon bot

addBot ::
  forall r.
  Members
    '[ ClientStore,
       ConversationStore,
       Error ActionError,
       Error ConversationError,
       ExternalAccess,
       GundeckAccess,
       MemberStore,
       TeamStore,
       WaiError
     ]
    r =>
  UserId ->
  ConnId ->
  AddBot ->
  Galley r Event
addBot zusr zcon b = do
  lusr <- qualifyLocal zusr
  c <-
    liftSem (E.getConversation (b ^. addBotConv))
      >>= ifNothing (errorDescriptionTypeToWai @ConvNotFound)
  -- Check some preconditions on adding bots to a conversation
  for_ (Data.convTeam c) $ teamConvChecks (b ^. addBotConv)
  (bots, users) <- regularConvChecks lusr c
  t <- liftIO getCurrentTime
  liftSem $ E.createClient (botUserId (b ^. addBotId)) (b ^. addBotClient)
  bm <- liftSem $ E.createBotMember (b ^. addBotService) (b ^. addBotId) (b ^. addBotConv)
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
  for_ (newPushLocal ListComplete zusr (ConvEvent e) (recipient <$> users)) $ \p ->
    liftSem . E.push1 $ p & pushConn ?~ zcon
  liftSem $ E.deliverAsync ((bm : bots) `zip` repeat e)
  pure e
  where
    regularConvChecks lusr c = do
      let (bots, users) = localBotsAndUsers (Data.convLocalMembers c)
      liftSem . unless (zusr `isMember` users) $
        throwErrorDescriptionType @ConvNotFound
      liftSem . mapError toWai $ ensureGroupConversation c
      self <- getSelfMemberFromLocalsLegacy zusr users
      ensureActionAllowed AddConversationMember self
      unless (any ((== b ^. addBotId) . botMemId) bots) $ do
        let botId = qualifyAs lusr (botUserId (b ^. addBotId))
        ensureMemberLimit (toList $ Data.convLocalMembers c) [qUntagged botId]
      return (bots, users)
    teamConvChecks :: ConvId -> TeamId -> Galley r ()
    teamConvChecks cid tid = do
      tcv <- liftSem $ E.getTeamConversation tid cid
      liftSem $
        when (maybe True (view managedConversation) tcv) $
          throw noAddToManaged

rmBotH ::
  Members '[ClientStore, ConversationStore, ExternalAccess, GundeckAccess, MemberStore, WaiError] r =>
  UserId ::: Maybe ConnId ::: JsonRequest RemoveBot ->
  Galley r Response
rmBotH (zusr ::: zcon ::: req) = do
  bot <- fromJsonBody req
  handleUpdateResult <$> rmBot zusr zcon bot

rmBot ::
  Members
    '[ ClientStore,
       ConversationStore,
       ExternalAccess,
       GundeckAccess,
       MemberStore,
       WaiError
     ]
    r =>
  UserId ->
  Maybe ConnId ->
  RemoveBot ->
  Galley r (UpdateResult Event)
rmBot zusr zcon b = do
  c <-
    liftSem (E.getConversation (b ^. rmBotConv))
      >>= ifNothing (errorDescriptionTypeToWai @ConvNotFound)
  localDomain <- viewFederationDomain
  let qcnv = Qualified (Data.convId c) localDomain
      qusr = Qualified zusr localDomain
  liftSem . unless (zusr `isMember` Data.convLocalMembers c) $
    throwErrorDescriptionType @ConvNotFound
  let (bots, users) = localBotsAndUsers (Data.convLocalMembers c)
  if not (any ((== b ^. rmBotId) . botMemId) bots)
    then pure Unchanged
    else do
      t <- liftIO getCurrentTime
      liftSem $ do
        let evd = EdMembersLeave (QualifiedUserIdList [Qualified (botUserId (b ^. rmBotId)) localDomain])
        let e = Event MemberLeave qcnv qusr t evd
        for_ (newPushLocal ListComplete zusr (ConvEvent e) (recipient <$> users)) $ \p ->
          E.push1 $ p & pushConn .~ zcon
        E.deleteMembers (Data.convId c) (UserList [botUserId (b ^. rmBotId)] [])
        E.deleteClients (botUserId (b ^. rmBotId))
        E.deliverAsync (bots `zip` repeat e)
        pure $ Updated e

-------------------------------------------------------------------------------
-- Helpers

ensureConvMember :: Member WaiError r => [LocalMember] -> UserId -> Galley r ()
ensureConvMember users usr =
  liftSem $
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
  forall r.
  Members '[BrigAccess, ClientStore, TeamStore, WaiError] r =>
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
  liftSem . unless (Map.size (userClientMap (otrRecipientsMap rcps)) <= limit) $
    throw broadcastLimitExceeded
  -- In large teams, we may still use the broadcast endpoint but only if `report_missing`
  -- is used and length `report_missing` < limit since we cannot fetch larger teams than
  -- that.
  tMembers <-
    fmap (view userId) <$> case val of
      OtrReportMissing us -> maybeFetchLimitedTeamMemberList limit tid us
      _ -> maybeFetchAllMembersInTeam tid
  contacts <- liftSem $ E.getContactList usr
  let users = Set.toList $ Set.union (Set.fromList tMembers) (Set.fromList contacts)
  isInternal <- view $ options . optSettings . setIntraListing
  clts <-
    liftSem $
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
      liftSem . unless (length localUserIdsToLookup <= limit) $
        throw broadcastLimitExceeded
      liftSem $ E.selectTeamMembers tid localUserIdsToLookup
    maybeFetchAllMembersInTeam :: TeamId -> Galley r [TeamMember]
    maybeFetchAllMembersInTeam tid = do
      mems <- getTeamMembersForFanout tid
      liftSem . when (mems ^. teamMemberListType == ListTruncated) $
        throw broadcastLimitExceeded
      pure (mems ^. teamMembers)

withValidOtrRecipients ::
  Members '[BrigAccess, ClientStore, ConversationStore, MemberStore, TeamStore, WaiError] r =>
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
  alive <- liftSem $ E.isConversationAlive cnv
  if not alive
    then do
      liftSem $ E.deleteConversation cnv
      pure $ OtrConversationNotFound mkErrorDescription
    else do
      localMembers <- liftSem $ E.getLocalMembers cnv
      let localMemberIds = lmId <$> localMembers
      isInternal <- view $ options . optSettings . setIntraListing
      clts <-
        liftSem $
          if isInternal
            then Clients.fromUserClients <$> E.lookupClients localMemberIds
            else E.getClients localMemberIds
      handleOtrResponse utype usr clt rcps localMembers clts val now go

handleOtrResponse ::
  Members '[BrigAccess, TeamStore, WaiError] r =>
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
      >>= either (const (liftSem (throwErrorDescriptionType @MissingLegalholdConsent))) pure
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
withBindingTeam :: Members '[TeamStore, WaiError] r => UserId -> (TeamId -> Galley r b) -> Galley r b
withBindingTeam zusr callback = do
  tid <- liftSem (E.getOneUserTeam zusr) >>= ifNothing teamNotFound
  binding <- liftSem (E.getTeamBinding tid) >>= ifNothing teamNotFound
  case binding of
    Binding -> callback tid
    NonBinding -> liftSem $ throw nonBindingTeam
