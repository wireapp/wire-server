-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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
    checkReusableCode,
    joinConversationByReusableCode,
    joinConversationById,
    addCodeUnqualified,
    rmCodeUnqualified,
    getCode,
    updateUnqualifiedConversationName,
    updateConversationName,
    updateConversationReceiptModeUnqualified,
    updateConversationReceiptMode,
    updateConversationMessageTimerUnqualified,
    updateConversationMessageTimer,
    updateConversationAccessUnqualified,
    updateConversationAccess,
    deleteLocalConversation,
    updateRemoteConversation,

    -- * Managing Members
    addMembersUnqualified,
    addMembersUnqualifiedV2,
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
    postProteusBroadcast,
    postOtrBroadcastUnqualified,
    isTypingUnqualified,

    -- * External Services
    addServiceH,
    rmServiceH,
    Galley.API.Update.addBotH,
    rmBotH,
    postBotMessageUnqualified,
  )
where

import Control.Error.Util (hush)
import Control.Lens
import Control.Monad.State
import Data.Code
import Data.Id
import Data.Json.Util
import Data.List1
import qualified Data.Map.Strict as Map
import Data.Qualified
import qualified Data.Set as Set
import Data.Singletons
import Data.Time
import Galley.API.Action
import Galley.API.Error
import Galley.API.Federation (onConversationUpdated)
import Galley.API.Mapping
import Galley.API.Message
import qualified Galley.API.Query as Query
import Galley.API.Util
import Galley.App
import qualified Galley.Data.Conversation as Data
import Galley.Data.Services as Data
import Galley.Data.Types hiding (Conversation)
import Galley.Effects
import qualified Galley.Effects.ClientStore as E
import qualified Galley.Effects.CodeStore as E
import qualified Galley.Effects.ConversationStore as E
import qualified Galley.Effects.ExternalAccess as E
import qualified Galley.Effects.FederatorAccess as E
import qualified Galley.Effects.GundeckAccess as E
import qualified Galley.Effects.MemberStore as E
import Galley.Effects.ProposalStore
import qualified Galley.Effects.ServiceStore as E
import Galley.Effects.TeamFeatureStore (FeaturePersistentConstraint)
import Galley.Effects.WaiRoutes
import Galley.Intra.Push
import Galley.Options
import Galley.Types.Bot hiding (addBot)
import Galley.Types.Bot.Service (Service)
import Galley.Types.Conversations.Members (LocalMember (..))
import Galley.Types.UserList
import Imports hiding (forkIO)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (Error, and, failure, setStatus, _1, _2)
import Network.Wai.Utilities hiding (Error)
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Action
import Wire.API.Conversation.Code
import Wire.API.Conversation.Role
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import Wire.API.Message
import Wire.API.Provider.Service (ServiceRef)
import Wire.API.Routes.Public.Galley
import Wire.API.Routes.Public.Util (UpdateResult (..))
import Wire.API.ServantProto (RawProto (..))
import Wire.API.Team.Feature hiding (setStatus)
import Wire.API.Team.Member
import Wire.API.User.Client

acceptConvH ::
  Members
    '[ ConversationStore,
       Error InternalError,
       ErrorS 'ConvNotFound,
       ErrorS 'InvalidOperation,
       ErrorS 'NotConnected,
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
       Error InternalError,
       ErrorS 'ConvNotFound,
       ErrorS 'InvalidOperation,
       ErrorS 'NotConnected,
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
    E.getConversation cnv >>= noteS @'ConvNotFound
  conv' <- acceptOne2One lusr conv conn
  conversationView lusr conv'

blockConvH ::
  Members
    '[ ConversationStore,
       ErrorS 'ConvNotFound,
       ErrorS 'InvalidOperation,
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
       ErrorS 'ConvNotFound,
       ErrorS 'InvalidOperation,
       MemberStore
     ]
    r =>
  UserId ->
  ConvId ->
  Sem r ()
blockConv zusr cnv = do
  conv <- E.getConversation cnv >>= noteS @'ConvNotFound
  unless (Data.convType conv `elem` [ConnectConv, One2OneConv]) $
    throwS @'InvalidOperation
  let mems = Data.convLocalMembers conv
  when (zusr `isMember` mems) $
    E.deleteMembers cnv (UserList [zusr] [])

unblockConvH ::
  Members
    '[ ConversationStore,
       Error InternalError,
       ErrorS 'ConvNotFound,
       ErrorS 'InvalidOperation,
       ErrorS 'NotConnected,
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
       Error InternalError,
       ErrorS 'ConvNotFound,
       ErrorS 'InvalidOperation,
       ErrorS 'NotConnected,
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
    E.getConversation cnv >>= noteS @'ConvNotFound
  unless (Data.convType conv `elem` [ConnectConv, One2OneConv]) $
    throwS @'InvalidOperation
  conv' <- acceptOne2One lusr conv conn
  conversationView lusr conv'

-- conversation updates

handleUpdateResult :: UpdateResult Event -> Response
handleUpdateResult = \case
  Updated ev -> json ev & setStatus status200
  Unchanged -> empty & setStatus status204

type UpdateConversationAccessEffects =
  '[ BotAccess,
     BrigAccess,
     CodeStore,
     ConversationStore,
     Error FederationError,
     Error InternalError,
     Error InvalidInput,
     ErrorS ('ActionDenied 'ModifyConversationAccess),
     ErrorS ('ActionDenied 'RemoveConversationMember),
     ErrorS 'ConvNotFound,
     ErrorS 'InvalidOperation,
     ErrorS 'InvalidTargetAccess,
     ExternalAccess,
     FederatorAccess,
     FireAndForget,
     GundeckAccess,
     Input Env,
     Input UTCTime,
     MemberStore,
     ProposalStore,
     TeamStore,
     TinyLog
   ]

updateConversationAccess ::
  Members UpdateConversationAccessEffects r =>
  Local UserId ->
  ConnId ->
  Qualified ConvId ->
  ConversationAccessData ->
  Sem r (UpdateResult Event)
updateConversationAccess lusr con qcnv update = do
  lcnv <- ensureLocal lusr qcnv
  getUpdateResult . fmap lcuEvent $
    updateLocalConversation @'ConversationAccessDataTag lcnv (qUntagged lusr) (Just con) update

updateConversationAccessUnqualified ::
  Members UpdateConversationAccessEffects r =>
  Local UserId ->
  ConnId ->
  ConvId ->
  ConversationAccessData ->
  Sem r (UpdateResult Event)
updateConversationAccessUnqualified lusr con cnv update =
  getUpdateResult . fmap lcuEvent $
    updateLocalConversation @'ConversationAccessDataTag
      (qualifyAs lusr cnv)
      (qUntagged lusr)
      (Just con)
      update

updateConversationReceiptMode ::
  Members
    '[ BrigAccess,
       ConversationStore,
       Error FederationError,
       ErrorS ('ActionDenied 'ModifyConversationReceiptMode),
       ErrorS 'ConvNotFound,
       ErrorS 'InvalidOperation,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input (Local ()),
       Input Env,
       Input UTCTime,
       MemberStore,
       TinyLog
     ]
    r =>
  Local UserId ->
  ConnId ->
  Qualified ConvId ->
  ConversationReceiptModeUpdate ->
  Sem r (UpdateResult Event)
updateConversationReceiptMode lusr zcon qcnv update =
  foldQualified
    lusr
    ( \lcnv ->
        getUpdateResult . fmap lcuEvent $
          updateLocalConversation
            @'ConversationReceiptModeUpdateTag
            lcnv
            (qUntagged lusr)
            (Just zcon)
            update
    )
    (\rcnv -> updateRemoteConversation @'ConversationReceiptModeUpdateTag rcnv lusr zcon update)
    qcnv

updateRemoteConversation ::
  forall tag r.
  ( Members
      '[ BrigAccess,
         Error FederationError,
         ExternalAccess,
         FederatorAccess,
         GundeckAccess,
         Input (Local ()),
         MemberStore,
         TinyLog
       ]
      r,
    Members (HasConversationActionGalleyErrors tag) r,
    RethrowErrors (HasConversationActionGalleyErrors tag) (Error NoChanges : r),
    SingI tag
  ) =>
  Remote ConvId ->
  Local UserId ->
  ConnId ->
  ConversationAction tag ->
  Sem r (UpdateResult Event)
updateRemoteConversation rcnv lusr conn action = getUpdateResult $ do
  let updateRequest =
        ConversationUpdateRequest
          { curUser = tUnqualified lusr,
            curConvId = tUnqualified rcnv,
            curAction = SomeConversationAction (sing @tag) action
          }
  response <- E.runFederated rcnv (fedClient @'Galley @"update-conversation" updateRequest)
  convUpdate <- case response of
    ConversationUpdateResponseNoChanges -> throw NoChanges
    ConversationUpdateResponseError err' -> rethrowErrors @(HasConversationActionGalleyErrors tag) err'
    ConversationUpdateResponseUpdate convUpdate -> pure convUpdate

  onConversationUpdated (tDomain rcnv) convUpdate
  notifyRemoteConversationAction lusr (qualifyAs rcnv convUpdate) (Just conn)

updateConversationReceiptModeUnqualified ::
  Members
    '[ BrigAccess,
       ConversationStore,
       Error FederationError,
       ErrorS ('ActionDenied 'ModifyConversationReceiptMode),
       ErrorS 'ConvNotFound,
       ErrorS 'InvalidOperation,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input (Local ()),
       Input Env,
       Input UTCTime,
       MemberStore,
       TinyLog
     ]
    r =>
  Local UserId ->
  ConnId ->
  ConvId ->
  ConversationReceiptModeUpdate ->
  Sem r (UpdateResult Event)
updateConversationReceiptModeUnqualified lusr zcon cnv = updateConversationReceiptMode lusr zcon (qUntagged (qualifyAs lusr cnv))

updateConversationMessageTimer ::
  Members
    '[ ConversationStore,
       ErrorS ('ActionDenied 'ModifyConversationMessageTimer),
       ErrorS 'ConvNotFound,
       ErrorS 'InvalidOperation,
       Error FederationError,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input Env,
       Input UTCTime
     ]
    r =>
  Local UserId ->
  ConnId ->
  Qualified ConvId ->
  ConversationMessageTimerUpdate ->
  Sem r (UpdateResult Event)
updateConversationMessageTimer lusr zcon qcnv update =
  getUpdateResult $
    foldQualified
      lusr
      ( \lcnv ->
          lcuEvent
            <$> updateLocalConversation
              @'ConversationMessageTimerUpdateTag
              lcnv
              (qUntagged lusr)
              (Just zcon)
              update
      )
      (\_ -> throw FederationNotImplemented)
      qcnv

updateConversationMessageTimerUnqualified ::
  Members
    '[ ConversationStore,
       ErrorS ('ActionDenied 'ModifyConversationMessageTimer),
       ErrorS 'ConvNotFound,
       ErrorS 'InvalidOperation,
       Error FederationError,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input Env,
       Input UTCTime
     ]
    r =>
  Local UserId ->
  ConnId ->
  ConvId ->
  ConversationMessageTimerUpdate ->
  Sem r (UpdateResult Event)
updateConversationMessageTimerUnqualified lusr zcon cnv = updateConversationMessageTimer lusr zcon (qUntagged (qualifyAs lusr cnv))

deleteLocalConversation ::
  Members
    '[ CodeStore,
       ConversationStore,
       Error FederationError,
       ErrorS 'NotATeamMember,
       ErrorS ('ActionDenied 'DeleteConversation),
       ErrorS 'ConvNotFound,
       ErrorS 'InvalidOperation,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input Env,
       Input UTCTime,
       TeamStore
     ]
    r =>
  Local UserId ->
  ConnId ->
  Local ConvId ->
  Sem r (UpdateResult Event)
deleteLocalConversation lusr con lcnv =
  getUpdateResult . fmap lcuEvent $
    updateLocalConversation @'ConversationDeleteTag lcnv (qUntagged lusr) (Just con) ()

getUpdateResult :: Sem (Error NoChanges ': r) a -> Sem r (UpdateResult a)
getUpdateResult = fmap (either (const Unchanged) Updated) . runError

addCodeUnqualified ::
  forall db r.
  ( Member CodeStore r,
    Member ConversationStore r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'GuestLinksDisabled) r,
    Member ExternalAccess r,
    Member GundeckAccess r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (Input Opts) r,
    Member (TeamFeatureStore db) r,
    FeaturePersistentConstraint db GuestLinksConfig
  ) =>
  UserId ->
  ConnId ->
  ConvId ->
  Sem r AddCodeResult
addCodeUnqualified usr zcon cnv = do
  lusr <- qualifyLocal usr
  lcnv <- qualifyLocal cnv
  addCode @db lusr zcon lcnv

addCode ::
  forall db r.
  ( Member CodeStore r,
    Member ConversationStore r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'GuestLinksDisabled) r,
    Member ExternalAccess r,
    Member GundeckAccess r,
    Member (Input UTCTime) r,
    Member (Input Opts) r,
    Member (TeamFeatureStore db) r,
    FeaturePersistentConstraint db GuestLinksConfig
  ) =>
  Local UserId ->
  ConnId ->
  Local ConvId ->
  Sem r AddCodeResult
addCode lusr zcon lcnv = do
  conv <- E.getConversation (tUnqualified lcnv) >>= noteS @'ConvNotFound
  Query.ensureGuestLinksEnabled @db (Data.convTeam conv)
  Query.ensureConvAdmin (Data.convLocalMembers conv) (tUnqualified lusr)
  ensureAccess conv CodeAccess
  ensureGuestsOrNonTeamMembersAllowed conv
  let (bots, users) = localBotsAndUsers $ Data.convLocalMembers conv
  key <- E.makeKey (tUnqualified lcnv)
  mCode <- E.getCode key ReusableCode
  case mCode of
    Nothing -> do
      code <- E.generateCode (tUnqualified lcnv) ReusableCode (Timeout 3600 * 24 * 365) -- one year FUTUREWORK: configurable
      E.createCode code
      now <- input
      conversationCode <- createCode code
      let event = Event (qUntagged lcnv) (qUntagged lusr) now (EdConvCodeUpdate conversationCode)
      pushConversationEvent (Just zcon) event (qualifyAs lusr (map lmId users)) bots
      pure $ CodeAdded event
    Just code -> do
      conversationCode <- createCode code
      pure $ CodeAlreadyExisted conversationCode
  where
    createCode :: Code -> Sem r ConversationCode
    createCode code = do
      mkConversationCode (codeKey code) (codeValue code) <$> E.getConversationCodeURI
    ensureGuestsOrNonTeamMembersAllowed :: Data.Conversation -> Sem r ()
    ensureGuestsOrNonTeamMembersAllowed conv =
      unless
        ( GuestAccessRole `Set.member` Data.convAccessRoles conv
            || NonTeamMemberAccessRole `Set.member` Data.convAccessRoles conv
        )
        $ throwS @'ConvAccessDenied

rmCodeUnqualified ::
  Members
    '[ CodeStore,
       ConversationStore,
       ErrorS 'ConvNotFound,
       ErrorS 'ConvAccessDenied,
       ExternalAccess,
       GundeckAccess,
       Input (Local ()),
       Input UTCTime
     ]
    r =>
  Local UserId ->
  ConnId ->
  ConvId ->
  Sem r Event
rmCodeUnqualified lusr zcon cnv = do
  lcnv <- qualifyLocal cnv
  rmCode lusr zcon lcnv

rmCode ::
  Members
    '[ CodeStore,
       ConversationStore,
       ErrorS 'ConvAccessDenied,
       ErrorS 'ConvNotFound,
       ExternalAccess,
       GundeckAccess,
       Input UTCTime
     ]
    r =>
  Local UserId ->
  ConnId ->
  Local ConvId ->
  Sem r Event
rmCode lusr zcon lcnv = do
  conv <-
    E.getConversation (tUnqualified lcnv) >>= noteS @'ConvNotFound
  Query.ensureConvAdmin (Data.convLocalMembers conv) (tUnqualified lusr)
  ensureAccess conv CodeAccess
  let (bots, users) = localBotsAndUsers $ Data.convLocalMembers conv
  key <- E.makeKey (tUnqualified lcnv)
  E.deleteCode key ReusableCode
  now <- input
  let event = Event (qUntagged lcnv) (qUntagged lusr) now EdConvCodeDelete
  pushConversationEvent (Just zcon) event (qualifyAs lusr (map lmId users)) bots
  pure event

getCode ::
  forall db r.
  ( Member CodeStore r,
    Member ConversationStore r,
    Member (ErrorS 'CodeNotFound) r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'GuestLinksDisabled) r,
    Member (Input Opts) r,
    Member (TeamFeatureStore db) r,
    FeaturePersistentConstraint db GuestLinksConfig
  ) =>
  Local UserId ->
  ConvId ->
  Sem r ConversationCode
getCode lusr cnv = do
  conv <-
    E.getConversation cnv >>= noteS @'ConvNotFound
  Query.ensureGuestLinksEnabled @db (Data.convTeam conv)
  ensureAccess conv CodeAccess
  ensureConvMember (Data.convLocalMembers conv) (tUnqualified lusr)
  key <- E.makeKey cnv
  c <- E.getCode key ReusableCode >>= noteS @'CodeNotFound
  returnCode c

returnCode :: Member CodeStore r => Code -> Sem r ConversationCode
returnCode c = do
  mkConversationCode (codeKey c) (codeValue c) <$> E.getConversationCodeURI

checkReusableCode ::
  forall db r.
  ( Members
      '[ CodeStore,
         ConversationStore,
         TeamFeatureStore db,
         ErrorS 'CodeNotFound,
         ErrorS 'ConvNotFound,
         Input Opts
       ]
      r,
    FeaturePersistentConstraint db GuestLinksConfig
  ) =>
  ConversationCode ->
  Sem r ()
checkReusableCode convCode = do
  code <- verifyReusableCode convCode
  conv <- E.getConversation (codeConversation code) >>= noteS @'ConvNotFound
  mapErrorS @'GuestLinksDisabled @'CodeNotFound $
    Query.ensureGuestLinksEnabled @db (Data.convTeam conv)

joinConversationByReusableCode ::
  forall db r.
  ( Members
      '[ BrigAccess,
         CodeStore,
         ConversationStore,
         ErrorS 'CodeNotFound,
         ErrorS 'ConvAccessDenied,
         ErrorS 'ConvNotFound,
         ErrorS 'GuestLinksDisabled,
         ErrorS 'InvalidOperation,
         ErrorS 'NotATeamMember,
         ErrorS 'TooManyMembers,
         FederatorAccess,
         ExternalAccess,
         GundeckAccess,
         Input Opts,
         Input UTCTime,
         MemberStore,
         TeamStore,
         TeamFeatureStore db
       ]
      r,
    FeaturePersistentConstraint db GuestLinksConfig
  ) =>
  Local UserId ->
  ConnId ->
  ConversationCode ->
  Sem r (UpdateResult Event)
joinConversationByReusableCode lusr zcon convCode = do
  c <- verifyReusableCode convCode
  conv <- E.getConversation (codeConversation c) >>= noteS @'ConvNotFound
  Query.ensureGuestLinksEnabled @db (Data.convTeam conv)
  joinConversation @db lusr zcon conv CodeAccess

joinConversationById ::
  forall db r.
  ( Members
      '[ BrigAccess,
         FederatorAccess,
         ConversationStore,
         ErrorS 'ConvAccessDenied,
         ErrorS 'ConvNotFound,
         ErrorS 'InvalidOperation,
         ErrorS 'NotATeamMember,
         ErrorS 'TooManyMembers,
         ExternalAccess,
         GundeckAccess,
         Input Opts,
         Input UTCTime,
         MemberStore,
         TeamStore,
         TeamFeatureStore db
       ]
      r
  ) =>
  Local UserId ->
  ConnId ->
  ConvId ->
  Sem r (UpdateResult Event)
joinConversationById lusr zcon cnv = do
  conv <- E.getConversation cnv >>= noteS @'ConvNotFound
  joinConversation @db lusr zcon conv LinkAccess

joinConversation ::
  Members
    '[ BrigAccess,
       ConversationStore,
       FederatorAccess,
       ErrorS 'ConvAccessDenied,
       ErrorS 'InvalidOperation,
       ErrorS 'NotATeamMember,
       ErrorS 'TooManyMembers,
       ExternalAccess,
       GundeckAccess,
       Input Opts,
       Input UTCTime,
       MemberStore,
       TeamStore,
       TeamFeatureStore db
     ]
    r =>
  Local UserId ->
  ConnId ->
  Data.Conversation ->
  Access ->
  Sem r (UpdateResult Event)
joinConversation lusr zcon conv access = do
  let lcnv = qualifyAs lusr (convId conv)
  ensureConversationAccess (tUnqualified lusr) conv access
  ensureGroupConversation conv
  -- FUTUREWORK: remote users?
  ensureMemberLimit (toList $ Data.convLocalMembers conv) [tUnqualified lusr]
  getUpdateResult $ do
    -- NOTE: When joining conversations, all users become members
    -- as this is our desired behavior for these types of conversations
    -- where there is no way to control who joins, etc.
    let users = filter (notIsConvMember lusr conv) [tUnqualified lusr]
    (extraTargets, action) <-
      addMembersToLocalConversation lcnv (UserList users []) roleNameWireMember
    lcuEvent
      <$> notifyConversationAction
        (sing @'ConversationJoinTag)
        (qUntagged lusr)
        False
        (Just zcon)
        (qualifyAs lusr conv)
        (convBotsAndMembers conv <> extraTargets)
        action

addMembers ::
  Members
    '[ BrigAccess,
       ConversationStore,
       Error FederationError,
       Error InternalError,
       ErrorS ('ActionDenied 'AddConversationMember),
       ErrorS ('ActionDenied 'LeaveConversation),
       ErrorS 'ConvAccessDenied,
       ErrorS 'ConvNotFound,
       ErrorS 'InvalidOperation,
       ErrorS 'NotConnected,
       ErrorS 'NotATeamMember,
       ErrorS 'TooManyMembers,
       ErrorS 'MissingLegalholdConsent,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input Env,
       Input Opts,
       Input UTCTime,
       LegalHoldStore,
       MemberStore,
       ProposalStore,
       TeamStore,
       TinyLog
     ]
    r =>
  Local UserId ->
  ConnId ->
  Qualified ConvId ->
  InviteQualified ->
  Sem r (UpdateResult Event)
addMembers lusr zcon qcnv (InviteQualified users role) = do
  lcnv <- ensureLocal lusr qcnv
  getUpdateResult . fmap lcuEvent $
    updateLocalConversation @'ConversationJoinTag lcnv (qUntagged lusr) (Just zcon) $
      ConversationJoin users role

addMembersUnqualifiedV2 ::
  Members
    '[ BrigAccess,
       ConversationStore,
       Error FederationError,
       Error InternalError,
       ErrorS ('ActionDenied 'AddConversationMember),
       ErrorS ('ActionDenied 'LeaveConversation),
       ErrorS 'ConvAccessDenied,
       ErrorS 'ConvNotFound,
       ErrorS 'InvalidOperation,
       ErrorS 'NotConnected,
       ErrorS 'NotATeamMember,
       ErrorS 'TooManyMembers,
       ErrorS 'MissingLegalholdConsent,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input Env,
       Input Opts,
       Input UTCTime,
       LegalHoldStore,
       MemberStore,
       ProposalStore,
       TeamStore,
       TinyLog
     ]
    r =>
  Local UserId ->
  ConnId ->
  ConvId ->
  InviteQualified ->
  Sem r (UpdateResult Event)
addMembersUnqualifiedV2 lusr zcon cnv (InviteQualified users role) = do
  let lcnv = qualifyAs lusr cnv
  getUpdateResult . fmap lcuEvent $
    updateLocalConversation @'ConversationJoinTag lcnv (qUntagged lusr) (Just zcon) $
      ConversationJoin users role

addMembersUnqualified ::
  Members
    '[ BrigAccess,
       ConversationStore,
       Error FederationError,
       Error InternalError,
       ErrorS ('ActionDenied 'AddConversationMember),
       ErrorS ('ActionDenied 'LeaveConversation),
       ErrorS 'ConvAccessDenied,
       ErrorS 'ConvNotFound,
       ErrorS 'InvalidOperation,
       ErrorS 'NotConnected,
       ErrorS 'NotATeamMember,
       ErrorS 'TooManyMembers,
       ErrorS 'MissingLegalholdConsent,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input Env,
       Input Opts,
       Input UTCTime,
       LegalHoldStore,
       MemberStore,
       ProposalStore,
       TeamStore,
       TinyLog
     ]
    r =>
  Local UserId ->
  ConnId ->
  ConvId ->
  Invite ->
  Sem r (UpdateResult Event)
addMembersUnqualified lusr zcon cnv (Invite users role) = do
  let qusers = fmap (qUntagged . qualifyAs lusr) (toNonEmpty users)
  addMembers lusr zcon (qUntagged (qualifyAs lusr cnv)) (InviteQualified qusers role)

updateSelfMember ::
  Members
    '[ ConversationStore,
       ErrorS 'ConvNotFound,
       ExternalAccess,
       GundeckAccess,
       Input UTCTime,
       MemberStore
     ]
    r =>
  Local UserId ->
  ConnId ->
  Qualified ConvId ->
  MemberUpdate ->
  Sem r ()
updateSelfMember lusr zcon qcnv update = do
  exists <- foldQualified lusr checkLocalMembership checkRemoteMembership qcnv
  unless exists $ throwS @'ConvNotFound
  E.setSelfMember qcnv lusr update
  now <- input
  let e = Event qcnv (qUntagged lusr) now (EdMemberUpdate (updateData lusr))
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
       ErrorS 'ConvNotFound,
       ExternalAccess,
       GundeckAccess,
       Input UTCTime,
       MemberStore
     ]
    r =>
  Local UserId ->
  ConnId ->
  ConvId ->
  MemberUpdate ->
  Sem r ()
updateUnqualifiedSelfMember lusr zcon cnv update = do
  let lcnv = qualifyAs lusr cnv
  updateSelfMember lusr zcon (qUntagged lcnv) update

updateOtherMemberLocalConv ::
  Members
    '[ ConversationStore,
       ErrorS ('ActionDenied 'ModifyOtherConversationMember),
       ErrorS 'InvalidTarget,
       ErrorS 'InvalidOperation,
       ErrorS 'ConvNotFound,
       ErrorS 'ConvMemberNotFound,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input Env,
       Input UTCTime,
       MemberStore
     ]
    r =>
  Local ConvId ->
  Local UserId ->
  ConnId ->
  Qualified UserId ->
  OtherMemberUpdate ->
  Sem r ()
updateOtherMemberLocalConv lcnv lusr con qvictim update = void . getUpdateResult . fmap lcuEvent $ do
  when (qUntagged lusr == qvictim) $
    throwS @'InvalidTarget
  updateLocalConversation @'ConversationMemberUpdateTag lcnv (qUntagged lusr) (Just con) $
    ConversationMemberUpdate qvictim update

updateOtherMemberUnqualified ::
  Members
    '[ ConversationStore,
       ErrorS ('ActionDenied 'ModifyOtherConversationMember),
       ErrorS 'InvalidTarget,
       ErrorS 'InvalidOperation,
       ErrorS 'ConvNotFound,
       ErrorS 'ConvMemberNotFound,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input Env,
       Input UTCTime,
       MemberStore
     ]
    r =>
  Local UserId ->
  ConnId ->
  ConvId ->
  UserId ->
  OtherMemberUpdate ->
  Sem r ()
updateOtherMemberUnqualified lusr zcon cnv victim update = do
  let lcnv = qualifyAs lusr cnv
  let lvictim = qualifyAs lusr victim
  updateOtherMemberLocalConv lcnv lusr zcon (qUntagged lvictim) update

updateOtherMember ::
  Members
    '[ ConversationStore,
       Error FederationError,
       ErrorS ('ActionDenied 'ModifyOtherConversationMember),
       ErrorS 'InvalidTarget,
       ErrorS 'InvalidOperation,
       ErrorS 'ConvNotFound,
       ErrorS 'ConvMemberNotFound,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input Env,
       Input UTCTime,
       MemberStore
     ]
    r =>
  Local UserId ->
  ConnId ->
  Qualified ConvId ->
  Qualified UserId ->
  OtherMemberUpdate ->
  Sem r ()
updateOtherMember lusr zcon qcnv qvictim update = do
  let doUpdate = foldQualified lusr updateOtherMemberLocalConv updateOtherMemberRemoteConv
  doUpdate qcnv lusr zcon qvictim update

updateOtherMemberRemoteConv ::
  Member (Error FederationError) r =>
  Remote ConvId ->
  Local UserId ->
  ConnId ->
  Qualified UserId ->
  OtherMemberUpdate ->
  Sem r ()
updateOtherMemberRemoteConv _ _ _ _ _ = throw FederationNotImplemented

removeMemberUnqualified ::
  Members
    '[ ConversationStore,
       Error InternalError,
       ErrorS ('ActionDenied 'RemoveConversationMember),
       ErrorS 'ConvNotFound,
       ErrorS 'InvalidOperation,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input Env,
       Input UTCTime,
       MemberStore,
       ProposalStore,
       TinyLog
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
       Error InternalError,
       ErrorS ('ActionDenied 'RemoveConversationMember),
       ErrorS 'ConvNotFound,
       ErrorS 'InvalidOperation,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input Env,
       Input UTCTime,
       MemberStore,
       ProposalStore,
       TinyLog
     ]
    r =>
  Local UserId ->
  ConnId ->
  Qualified ConvId ->
  Qualified UserId ->
  Sem r (Maybe Event)
removeMemberQualified lusr con qcnv victim =
  mapErrorS @('ActionDenied 'LeaveConversation) @('ActionDenied 'RemoveConversationMember) $
    foldQualified
      lusr
      (\lcnv -> removeMemberFromLocalConv lcnv lusr (Just con))
      (\rcnv -> removeMemberFromRemoteConv rcnv lusr)
      qcnv
      victim

removeMemberFromRemoteConv ::
  Members
    '[ FederatorAccess,
       ErrorS ('ActionDenied 'RemoveConversationMember),
       ErrorS 'ConvNotFound,
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
      let rpc = fedClient @'Galley @"leave-conversation" lc
      (either handleError handleSuccess . leaveResponse =<<) $
        E.runFederated cnv rpc
  | otherwise = throwS @('ActionDenied 'RemoveConversationMember)
  where
    handleError ::
      Members '[ErrorS ('ActionDenied 'RemoveConversationMember), ErrorS 'ConvNotFound] r =>
      RemoveFromConversationError ->
      Sem r (Maybe Event)
    handleError RemoveFromConversationErrorRemovalNotAllowed =
      throwS @('ActionDenied 'RemoveConversationMember)
    handleError RemoveFromConversationErrorNotFound = throwS @'ConvNotFound
    handleError RemoveFromConversationErrorUnchanged = pure Nothing

    handleSuccess :: Member (Input UTCTime) r => () -> Sem r (Maybe Event)
    handleSuccess _ = do
      t <- input
      pure . Just $
        Event (qUntagged cnv) (qUntagged lusr) t $
          EdMembersLeave (QualifiedUserIdList [victim])

-- | Remove a member from a local conversation.
removeMemberFromLocalConv ::
  Members
    '[ ConversationStore,
       Error InternalError,
       ErrorS ('ActionDenied 'LeaveConversation),
       ErrorS ('ActionDenied 'RemoveConversationMember),
       ErrorS 'ConvNotFound,
       ErrorS 'InvalidOperation,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input Env,
       Input UTCTime,
       MemberStore,
       ProposalStore,
       TinyLog
     ]
    r =>
  Local ConvId ->
  Local UserId ->
  Maybe ConnId ->
  Qualified UserId ->
  Sem r (Maybe Event)
removeMemberFromLocalConv lcnv lusr con victim
  | qUntagged lusr == victim =
      fmap (fmap lcuEvent . hush)
        . runError @NoChanges
        . updateLocalConversation @'ConversationLeaveTag lcnv (qUntagged lusr) con
        $ ()
  | otherwise =
      fmap (fmap lcuEvent . hush)
        . runError @NoChanges
        . updateLocalConversation @'ConversationRemoveMembersTag lcnv (qUntagged lusr) con
        . pure
        $ victim

-- OTR

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
  RawProto QualifiedNewOtrMessage ->
  Sem r (PostOtrResponse MessageSendingStatus)
postProteusMessage sender zcon conv msg = runLocalInput sender $ do
  foldQualified
    sender
    (\c -> postQualifiedOtrMessage User (qUntagged sender) (Just zcon) c (rpValue msg))
    (\c -> postRemoteOtrMessage (qUntagged sender) c (rpRaw msg))
    conv

postProteusBroadcast ::
  Members
    '[ BotAccess,
       BrigAccess,
       ClientStore,
       ConversationStore,
       ErrorS 'TeamNotFound,
       ErrorS 'NonBindingTeam,
       ErrorS 'BroadcastLimitExceeded,
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
  QualifiedNewOtrMessage ->
  Sem r (PostOtrResponse MessageSendingStatus)
postProteusBroadcast sender zcon = postBroadcast sender (Just zcon)

unqualifyEndpoint ::
  Functor f =>
  Local x ->
  (QualifiedNewOtrMessage -> f (PostOtrResponse MessageSendingStatus)) ->
  Maybe IgnoreMissing ->
  Maybe ReportMissing ->
  NewOtrMessage ->
  f (PostOtrResponse ClientMismatch)
unqualifyEndpoint loc f ignoreMissing reportMissing message = do
  let qualifiedRecipients =
        QualifiedOtrRecipients
          . QualifiedUserClientMap
          . Map.singleton (tDomain loc)
          . userClientMap
          . fmap fromBase64TextLenient
          . otrRecipientsMap
          . newOtrRecipients
          $ message
      clientMismatchStrategy = legacyClientMismatchStrategy (tDomain loc) (newOtrReportMissing message) ignoreMissing reportMissing
      qualifiedMessage =
        QualifiedNewOtrMessage
          { qualifiedNewOtrSender = newOtrSender message,
            qualifiedNewOtrRecipients = qualifiedRecipients,
            qualifiedNewOtrNativePush = newOtrNativePush message,
            qualifiedNewOtrTransient = newOtrTransient message,
            qualifiedNewOtrNativePriority = newOtrNativePriority message,
            qualifiedNewOtrData = foldMap fromBase64TextLenient (newOtrData message),
            qualifiedNewOtrClientMismatchStrategy = clientMismatchStrategy
          }
  unqualify (tDomain loc) <$> f qualifiedMessage

postBotMessageUnqualified ::
  ( Members
      '[ BrigAccess,
         ClientStore,
         ConversationStore,
         ErrorS 'ConvNotFound,
         ExternalAccess,
         FederatorAccess,
         GundeckAccess,
         Input (Local ()),
         Input Opts,
         MemberStore,
         TeamStore,
         TinyLog,
         Input UTCTime
       ]
      r
  ) =>
  BotId ->
  ConvId ->
  Maybe IgnoreMissing ->
  Maybe ReportMissing ->
  NewOtrMessage ->
  Sem r (PostOtrResponse ClientMismatch)
postBotMessageUnqualified sender cnv ignoreMissing reportMissing message = do
  lusr <- qualifyLocal (botUserId sender)
  lcnv <- qualifyLocal cnv
  unqualifyEndpoint
    lusr
    (runLocalInput lusr . postQualifiedOtrMessage Bot (qUntagged lusr) Nothing lcnv)
    ignoreMissing
    reportMissing
    message

postOtrBroadcastUnqualified ::
  Members
    '[ BrigAccess,
       ClientStore,
       ErrorS 'TeamNotFound,
       ErrorS 'NonBindingTeam,
       ErrorS 'BroadcastLimitExceeded,
       GundeckAccess,
       Input Opts,
       Input UTCTime,
       TeamStore,
       TinyLog
     ]
    r =>
  Local UserId ->
  ConnId ->
  Maybe IgnoreMissing ->
  Maybe ReportMissing ->
  NewOtrMessage ->
  Sem r (PostOtrResponse ClientMismatch)
postOtrBroadcastUnqualified sender zcon =
  unqualifyEndpoint
    sender
    (postBroadcast sender (Just zcon))

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
  Maybe IgnoreMissing ->
  Maybe ReportMissing ->
  NewOtrMessage ->
  Sem r (PostOtrResponse ClientMismatch)
postOtrMessageUnqualified sender zcon cnv =
  let lcnv = qualifyAs sender cnv
   in unqualifyEndpoint
        sender
        (runLocalInput sender . postQualifiedOtrMessage User (qUntagged sender) (Just zcon) lcnv)

updateConversationName ::
  Members
    '[ ConversationStore,
       Error FederationError,
       Error InvalidInput,
       ErrorS ('ActionDenied 'ModifyConversationName),
       ErrorS 'ConvNotFound,
       ErrorS 'InvalidOperation,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input Env,
       Input UTCTime
     ]
    r =>
  Local UserId ->
  ConnId ->
  Qualified ConvId ->
  ConversationRename ->
  Sem r (UpdateResult Event)
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
       Error InvalidInput,
       ErrorS ('ActionDenied 'ModifyConversationName),
       ErrorS 'ConvNotFound,
       ErrorS 'InvalidOperation,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input Env,
       Input UTCTime
     ]
    r =>
  Local UserId ->
  ConnId ->
  ConvId ->
  ConversationRename ->
  Sem r (UpdateResult Event)
updateUnqualifiedConversationName lusr zcon cnv rename = do
  let lcnv = qualifyAs lusr cnv
  updateLocalConversationName lusr zcon lcnv rename

updateLocalConversationName ::
  Members
    '[ ConversationStore,
       Error InvalidInput,
       ErrorS ('ActionDenied 'ModifyConversationName),
       ErrorS 'ConvNotFound,
       ErrorS 'InvalidOperation,
       ExternalAccess,
       FederatorAccess,
       GundeckAccess,
       Input Env,
       Input UTCTime
     ]
    r =>
  Local UserId ->
  ConnId ->
  Local ConvId ->
  ConversationRename ->
  Sem r (UpdateResult Event)
updateLocalConversationName lusr zcon lcnv rename =
  getUpdateResult . fmap lcuEvent $
    updateLocalConversation @'ConversationRenameTag lcnv (qUntagged lusr) (Just zcon) rename

isTypingUnqualified ::
  Members
    '[ GundeckAccess,
       ErrorS 'ConvNotFound,
       Input (Local ()),
       Input UTCTime,
       MemberStore,
       WaiRoutes
     ]
    r =>
  Local UserId ->
  ConnId ->
  ConvId ->
  TypingData ->
  Sem r ()
isTypingUnqualified lusr zcon cnv typingData = do
  lcnv <- qualifyLocal cnv
  isTyping lusr zcon lcnv typingData

isTyping ::
  Members '[ErrorS 'ConvNotFound, GundeckAccess, Input UTCTime, MemberStore] r =>
  Local UserId ->
  ConnId ->
  Local ConvId ->
  TypingData ->
  Sem r ()
isTyping lusr zcon lcnv typingData = do
  mm <- E.getLocalMembers (tUnqualified lcnv)
  unless (tUnqualified lusr `isMember` mm) $ throwS @'ConvNotFound
  now <- input
  let e = Event (qUntagged lcnv) (qUntagged lusr) now (EdTyping typingData)
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
  pure empty

rmServiceH ::
  Members '[ServiceStore, WaiRoutes] r =>
  JsonRequest ServiceRef ->
  Sem r Response
rmServiceH req = do
  E.deleteService =<< fromJsonBody req
  pure empty

addBotH ::
  Members
    '[ ClientStore,
       ConversationStore,
       ErrorS ('ActionDenied 'AddConversationMember),
       ErrorS 'ConvNotFound,
       ErrorS 'InvalidOperation,
       ErrorS 'TooManyMembers,
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
       ErrorS ('ActionDenied 'AddConversationMember),
       ErrorS 'ConvNotFound,
       ErrorS 'InvalidOperation,
       ErrorS 'TooManyMembers,
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
    E.getConversation (b ^. addBotConv) >>= noteS @'ConvNotFound
  -- Check some preconditions on adding bots to a conversation
  (bots, users) <- regularConvChecks c
  t <- input
  E.createClient (botUserId (b ^. addBotId)) (b ^. addBotClient)
  bm <- E.createBotMember (b ^. addBotService) (b ^. addBotId) (b ^. addBotConv)
  let e =
        Event
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
      unless (tUnqualified lusr `isMember` users) $ throwS @'ConvNotFound
      ensureGroupConversation c
      self <- getSelfMemberFromLocals (tUnqualified lusr) users
      -- Note that in brig from where this internal handler is called, we additionally check for conversation admin role.
      -- Remember to change this if we ever want to allow non admins to add bots.
      ensureActionAllowed SAddConversationMember self
      unless (any ((== b ^. addBotId) . botMemId) bots) $ do
        let botId = qualifyAs lusr (botUserId (b ^. addBotId))
        ensureMemberLimit (toList $ Data.convLocalMembers c) [qUntagged botId]
      pure (bots, users)

rmBotH ::
  Members
    '[ ClientStore,
       ConversationStore,
       ErrorS 'ConvNotFound,
       ExternalAccess,
       GundeckAccess,
       Input (Local ()),
       Input UTCTime,
       MemberStore,
       WaiRoutes,
       ErrorS ('ActionDenied 'RemoveConversationMember)
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
       ErrorS 'ConvNotFound,
       ExternalAccess,
       GundeckAccess,
       Input UTCTime,
       MemberStore,
       ErrorS ('ActionDenied 'RemoveConversationMember)
     ]
    r =>
  Local UserId ->
  Maybe ConnId ->
  RemoveBot ->
  Sem r (UpdateResult Event)
rmBot lusr zcon b = do
  c <-
    E.getConversation (b ^. rmBotConv) >>= noteS @'ConvNotFound
  let (bots, users) = localBotsAndUsers (Data.convLocalMembers c)
  unless (tUnqualified lusr `isMember` Data.convLocalMembers c) $
    throwS @'ConvNotFound
  -- A bot can remove itself (which will internally be triggered when a service is deleted),
  -- otherwise we have to check for the correct permissions
  unless (botUserId (b ^. rmBotId) == tUnqualified lusr) $ do
    -- Note that in brig from where this internal handler is called, we additionally check for conversation admin role.
    -- Remember to change this if we ever want to allow non admins to remove bots.
    self <- getSelfMemberFromLocals (tUnqualified lusr) users
    ensureActionAllowed SRemoveConversationMember self
  let lcnv = qualifyAs lusr (Data.convId c)
  if not (any ((== b ^. rmBotId) . botMemId) bots)
    then pure Unchanged
    else do
      t <- input
      do
        let evd = EdMembersLeave (QualifiedUserIdList [qUntagged (qualifyAs lusr (botUserId (b ^. rmBotId)))])
        let e = Event (qUntagged lcnv) (qUntagged lusr) t evd
        for_ (newPushLocal ListComplete (tUnqualified lusr) (ConvEvent e) (recipient <$> users)) $ \p ->
          E.push1 $ p & pushConn .~ zcon
        E.deleteMembers (Data.convId c) (UserList [botUserId (b ^. rmBotId)] [])
        E.deleteClients (botUserId (b ^. rmBotId))
        E.deliverAsync (bots `zip` repeat e)
        pure $ Updated e

-------------------------------------------------------------------------------
-- Helpers

ensureConvMember :: Member (ErrorS 'ConvNotFound) r => [LocalMember] -> UserId -> Sem r ()
ensureConvMember users usr =
  unless (usr `isMember` users) $ throwS @'ConvNotFound
