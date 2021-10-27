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

module Galley.API.Create
  ( createGroupConversation,
    internalCreateManagedConversationH,
    createSelfConversation,
    createOne2OneConversation,
    createConnectConversation,
  )
where

import Control.Lens hiding ((??))
import Control.Monad.Catch
import Data.Id
import Data.List1 (list1)
import Data.Misc (FutureWork (FutureWork))
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
import Data.Time
import qualified Data.UUID.Tagged as U
import Galley.API.Error
import Galley.API.Mapping
import Galley.API.One2One
import Galley.API.Util
import Galley.App
import qualified Galley.Data as Data
import Galley.Effects
import Galley.Intra.Push
import Galley.Types
import Galley.Types.Teams (ListType (..), Perm (..), TeamBinding (Binding), notTeamMember)
import Galley.Types.UserList
import Galley.Validation
import Imports hiding ((\\))
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (setStatus)
import Network.Wai.Utilities
import qualified Wire.API.Conversation as Public
import Wire.API.ErrorDescription (MissingLegalholdConsent)
import Wire.API.Federation.Error (federationNotImplemented)
import Wire.API.Routes.Public.Galley (ConversationResponse)
import Wire.API.Routes.Public.Util
import Wire.API.Team.LegalHold (LegalholdProtectee (LegalholdPlusFederationNotImplemented))

----------------------------------------------------------------------------
-- Group conversations

-- | The public-facing endpoint for creating group conversations.
--
-- See Note [managed conversations].
createGroupConversation ::
  Members '[BrigAccess, FederatorAccess, GundeckAccess] r =>
  UserId ->
  ConnId ->
  Public.NewConvUnmanaged ->
  Galley r ConversationResponse
createGroupConversation user conn wrapped@(Public.NewConvUnmanaged body) =
  case newConvTeam body of
    Nothing -> createRegularGroupConv user conn wrapped
    Just tinfo -> createTeamGroupConv user conn tinfo body

-- | An internal endpoint for creating managed group conversations. Will
-- throw an error for everything else.
internalCreateManagedConversationH ::
  Members '[BrigAccess, FederatorAccess, GundeckAccess] r =>
  UserId ::: ConnId ::: JsonRequest NewConvManaged ->
  Galley r Response
internalCreateManagedConversationH (zusr ::: zcon ::: req) = do
  newConv <- fromJsonBody req
  handleConversationResponse <$> internalCreateManagedConversation zusr zcon newConv

internalCreateManagedConversation ::
  Members '[BrigAccess, FederatorAccess, GundeckAccess] r =>
  UserId ->
  ConnId ->
  NewConvManaged ->
  Galley r ConversationResponse
internalCreateManagedConversation zusr zcon (NewConvManaged body) = do
  case newConvTeam body of
    Nothing -> throwM internalError
    Just tinfo -> createTeamGroupConv zusr zcon tinfo body

ensureNoLegalholdConflicts ::
  [Remote UserId] ->
  [UserId] ->
  Galley r ()
ensureNoLegalholdConflicts remotes locals = do
  let FutureWork _remotes = FutureWork @'LegalholdPlusFederationNotImplemented remotes
  whenM (anyLegalholdActivated locals) $
    unlessM (allLegalholdConsentGiven locals) $
      throwErrorDescriptionType @MissingLegalholdConsent

-- | A helper for creating a regular (non-team) group conversation.
createRegularGroupConv ::
  Members '[BrigAccess, FederatorAccess, GundeckAccess] r =>
  UserId ->
  ConnId ->
  NewConvUnmanaged ->
  Galley r ConversationResponse
createRegularGroupConv zusr zcon (NewConvUnmanaged body) = do
  lusr <- qualifyLocal zusr
  name <- rangeCheckedMaybe (newConvName body)
  let allUsers = newConvMembers lusr body
  checkedUsers <- checkedConvSize allUsers
  ensureConnected lusr allUsers
  ensureNoLegalholdConflicts (ulRemotes allUsers) (ulLocals allUsers)
  c <-
    Data.createConversation
      lusr
      name
      (access body)
      (accessRole body)
      checkedUsers
      (newConvTeam body)
      (newConvMessageTimer body)
      (newConvReceiptMode body)
      (newConvUsersRole body)
  notifyCreatedConversation Nothing zusr (Just zcon) c
  conversationCreated zusr c

-- | A helper for creating a team group conversation, used by the endpoint
-- handlers above. Only supports unmanaged conversations.
createTeamGroupConv ::
  Members '[BrigAccess, FederatorAccess, GundeckAccess] r =>
  UserId ->
  ConnId ->
  Public.ConvTeamInfo ->
  Public.NewConv ->
  Galley r ConversationResponse
createTeamGroupConv zusr zcon tinfo body = do
  lusr <- qualifyLocal zusr
  name <- rangeCheckedMaybe (newConvName body)
  let allUsers = newConvMembers lusr body
      convTeam = cnvTeamId tinfo

  zusrMembership <- Data.teamMember convTeam zusr
  void $ permissionCheck CreateConversation zusrMembership
  checkedUsers <- checkedConvSize allUsers
  convLocalMemberships <- mapM (Data.teamMember convTeam) (ulLocals allUsers)
  ensureAccessRole (accessRole body) (zip (ulLocals allUsers) convLocalMemberships)
  -- In teams we don't have 1:1 conversations, only regular conversations. We want
  -- users without the 'AddRemoveConvMember' permission to still be able to create
  -- regular conversations, therefore we check for 'AddRemoveConvMember' only if
  -- there are going to be more than two users in the conversation.
  -- FUTUREWORK: We keep this permission around because not doing so will break backwards
  -- compatibility in the sense that the team role 'partners' would be able to create group
  -- conversations (which they should not be able to).
  -- Not sure at the moment how to best solve this but it is unlikely
  -- we can ever get rid of the team permission model anyway - the only thing I can
  -- think of is that 'partners' can create convs but not be admins...
  when (length allUsers > 1) $ do
    void $ permissionCheck DoNotUseDeprecatedAddRemoveConvMember zusrMembership
  -- Team members are always considered to be connected, so we only check
  -- 'ensureConnected' for non-team-members.
  ensureConnectedToLocals zusr (notTeamMember (ulLocals allUsers) (catMaybes convLocalMemberships))
  ensureConnectedToRemotes lusr (ulRemotes allUsers)
  ensureNoLegalholdConflicts (ulRemotes allUsers) (ulLocals allUsers)
  conv <-
    Data.createConversation
      lusr
      name
      (access body)
      (accessRole body)
      checkedUsers
      (newConvTeam body)
      (newConvMessageTimer body)
      (newConvReceiptMode body)
      (newConvUsersRole body)
  now <- liftIO getCurrentTime
  -- NOTE: We only send (conversation) events to members of the conversation
  notifyCreatedConversation (Just now) zusr (Just zcon) conv
  conversationCreated zusr conv

----------------------------------------------------------------------------
-- Other kinds of conversations

createSelfConversation :: UserId -> Galley r ConversationResponse
createSelfConversation zusr = do
  lusr <- qualifyLocal zusr
  c <- Data.conversation (Id . toUUID $ zusr)
  maybe (create lusr) (conversationExisted zusr) c
  where
    create lusr = do
      c <- Data.createSelfConversation lusr Nothing
      conversationCreated zusr c

createOne2OneConversation ::
  Members '[BrigAccess, FederatorAccess, GundeckAccess] r =>
  UserId ->
  ConnId ->
  NewConvUnmanaged ->
  Galley r ConversationResponse
createOne2OneConversation zusr zcon (NewConvUnmanaged j) = do
  lusr <- qualifyLocal zusr
  let allUsers = newConvMembers lusr j
  other <- ensureOne (ulAll lusr allUsers)
  when (qUntagged lusr == other) $
    throwM (invalidOp "Cannot create a 1-1 with yourself")
  mtid <- case newConvTeam j of
    Just ti
      | cnvManaged ti -> throwM noManagedTeamConv
      | otherwise -> do
        foldQualified
          lusr
          (\lother -> checkBindingTeamPermissions lusr lother (cnvTeamId ti))
          (const (pure Nothing))
          other
    Nothing -> ensureConnected lusr allUsers $> Nothing
  n <- rangeCheckedMaybe (newConvName j)
  foldQualified
    lusr
    (createLegacyOne2OneConversationUnchecked lusr zcon n mtid)
    (createOne2OneConversationUnchecked lusr zcon n mtid . qUntagged)
    other
  where
    verifyMembership tid u = do
      membership <- Data.teamMember tid u
      when (isNothing membership) $
        throwM noBindingTeamMembers
    checkBindingTeamPermissions lusr lother tid = do
      zusrMembership <- Data.teamMember tid (tUnqualified lusr)
      void $ permissionCheck CreateConversation zusrMembership
      Data.teamBinding tid >>= \case
        Just Binding -> do
          verifyMembership tid (tUnqualified lusr)
          verifyMembership tid (tUnqualified lother)
          pure (Just tid)
        Just _ -> throwM nonBindingTeam
        Nothing -> throwM teamNotFound

createLegacyOne2OneConversationUnchecked ::
  Members '[FederatorAccess, GundeckAccess] r =>
  Local UserId ->
  ConnId ->
  Maybe (Range 1 256 Text) ->
  Maybe TeamId ->
  Local UserId ->
  Galley r ConversationResponse
createLegacyOne2OneConversationUnchecked self zcon name mtid other = do
  lcnv <- localOne2OneConvId self other
  mc <- Data.conversation (tUnqualified lcnv)
  case mc of
    Just c -> conversationExisted (tUnqualified self) c
    Nothing -> do
      (x, y) <- toUUIDs (tUnqualified self) (tUnqualified other)
      c <- Data.createLegacyOne2OneConversation self x y name mtid
      notifyCreatedConversation Nothing (tUnqualified self) (Just zcon) c
      conversationCreated (tUnqualified self) c

createOne2OneConversationUnchecked ::
  Members '[FederatorAccess, GundeckAccess] r =>
  Local UserId ->
  ConnId ->
  Maybe (Range 1 256 Text) ->
  Maybe TeamId ->
  Qualified UserId ->
  Galley r ConversationResponse
createOne2OneConversationUnchecked self zcon name mtid other = do
  let create =
        foldQualified
          self
          createOne2OneConversationLocally
          createOne2OneConversationRemotely
  create (one2OneConvId (qUntagged self) other) self zcon name mtid other

createOne2OneConversationLocally ::
  Members '[FederatorAccess, GundeckAccess] r =>
  Local ConvId ->
  Local UserId ->
  ConnId ->
  Maybe (Range 1 256 Text) ->
  Maybe TeamId ->
  Qualified UserId ->
  Galley r ConversationResponse
createOne2OneConversationLocally lcnv self zcon name mtid other = do
  mc <- Data.conversation (tUnqualified lcnv)
  case mc of
    Just c -> conversationExisted (tUnqualified self) c
    Nothing -> do
      c <- Data.createOne2OneConversation lcnv self other name mtid
      notifyCreatedConversation Nothing (tUnqualified self) (Just zcon) c
      conversationCreated (tUnqualified self) c

createOne2OneConversationRemotely ::
  Remote ConvId ->
  Local UserId ->
  ConnId ->
  Maybe (Range 1 256 Text) ->
  Maybe TeamId ->
  Qualified UserId ->
  Galley r ConversationResponse
createOne2OneConversationRemotely _ _ _ _ _ _ =
  throwM federationNotImplemented

createConnectConversation ::
  Members '[FederatorAccess, GundeckAccess] r =>
  UserId ->
  Maybe ConnId ->
  Connect ->
  Galley r ConversationResponse
createConnectConversation usr conn j = do
  lusr <- qualifyLocal usr
  foldQualified
    lusr
    (\lrcpt -> createLegacyConnectConversation lusr conn lrcpt j)
    (createConnectConversationWithRemote lusr conn)
    (cRecipient j)

createConnectConversationWithRemote ::
  Local UserId ->
  Maybe ConnId ->
  Remote UserId ->
  Galley r ConversationResponse
createConnectConversationWithRemote _ _ _ =
  throwM federationNotImplemented

createLegacyConnectConversation ::
  Members '[FederatorAccess, GundeckAccess] r =>
  Local UserId ->
  Maybe ConnId ->
  Local UserId ->
  Connect ->
  Galley r ConversationResponse
createLegacyConnectConversation lusr conn lrecipient j = do
  (x, y) <- toUUIDs (tUnqualified lusr) (tUnqualified lrecipient)
  n <- rangeCheckedMaybe (cName j)
  conv <- Data.conversation (Data.localOne2OneConvId x y)
  maybe (create x y n) (update n) conv
  where
    create x y n = do
      c <- Data.createConnectConversation lusr x y n
      now <- liftIO getCurrentTime
      let lcid = qualifyAs lusr (Data.convId c)
          e = Event ConvConnect (qUntagged lcid) (qUntagged lusr) now (EdConnect j)
      notifyCreatedConversation Nothing (tUnqualified lusr) conn c
      for_ (newPushLocal ListComplete (tUnqualified lusr) (ConvEvent e) (recipient <$> Data.convLocalMembers c)) $ \p ->
        push1 $
          p
            & pushRoute .~ RouteDirect
            & pushConn .~ conn
      conversationCreated (tUnqualified lusr) c
    update n conv = do
      let mems = Data.convLocalMembers conv
       in conversationExisted (tUnqualified lusr)
            =<< if
                | (tUnqualified lusr) `isMember` mems ->
                  -- we already were in the conversation, maybe also other
                  connect n conv
                | otherwise -> do
                  lcid <- qualifyLocal (Data.convId conv)
                  mm <- Data.addMember lcid lusr
                  let conv' =
                        conv
                          { Data.convLocalMembers = Data.convLocalMembers conv <> toList mm
                          }
                  if null mems
                    then do
                      -- the conversation was empty
                      connect n conv'
                    else do
                      -- we were not in the conversation, but someone else
                      conv'' <- acceptOne2One (tUnqualified lusr) conv' conn
                      if Data.convType conv'' == ConnectConv
                        then connect n conv''
                        else return conv''
    connect n conv
      | Data.convType conv == ConnectConv = do
        localDomain <- viewFederationDomain
        let qconv = Qualified (Data.convId conv) localDomain
        n' <- case n of
          Just x -> do
            Data.updateConversation (Data.convId conv) x
            return . Just $ fromRange x
          Nothing -> return $ Data.convName conv
        t <- liftIO getCurrentTime
        let e = Event ConvConnect qconv (qUntagged lusr) t (EdConnect j)
        for_ (newPushLocal ListComplete (tUnqualified lusr) (ConvEvent e) (recipient <$> Data.convLocalMembers conv)) $ \p ->
          push1 $
            p
              & pushRoute .~ RouteDirect
              & pushConn .~ conn
        return $ conv {Data.convName = n'}
      | otherwise = return conv

-------------------------------------------------------------------------------
-- Helpers

conversationCreated :: UserId -> Data.Conversation -> Galley r ConversationResponse
conversationCreated usr cnv = Created <$> conversationView usr cnv

conversationExisted :: UserId -> Data.Conversation -> Galley r ConversationResponse
conversationExisted usr cnv = Existed <$> conversationView usr cnv

handleConversationResponse :: ConversationResponse -> Response
handleConversationResponse = \case
  Created cnv -> json cnv & setStatus status201 . location (qUnqualified . cnvQualifiedId $ cnv)
  Existed cnv -> json cnv & setStatus status200 . location (qUnqualified . cnvQualifiedId $ cnv)

notifyCreatedConversation ::
  Members '[FederatorAccess, GundeckAccess] r =>
  Maybe UTCTime ->
  UserId ->
  Maybe ConnId ->
  Data.Conversation ->
  Galley r ()
notifyCreatedConversation dtime usr conn c = do
  localDomain <- viewFederationDomain
  now <- maybe (liftIO getCurrentTime) pure dtime
  -- FUTUREWORK: Handle failures in notifying so it does not abort half way
  -- through (either when notifying remotes or locals)
  --
  -- Ask remote server to store conversation membership and notify remote users
  -- of being added to a conversation
  registerRemoteConversationMemberships now localDomain c
  -- Notify local users
  pushSome =<< mapM (toPush localDomain now) (Data.convLocalMembers c)
  where
    route
      | Data.convType c == RegularConv = RouteAny
      | otherwise = RouteDirect
    toPush dom t m = do
      let qconv = Qualified (Data.convId c) dom
          qusr = Qualified usr dom
      c' <- conversationView (lmId m) c
      let e = Event ConvCreate qconv qusr t (EdConversation c')
      return $
        newPushLocal1 ListComplete usr (ConvEvent e) (list1 (recipient m) [])
          & pushConn .~ conn
          & pushRoute .~ route

localOne2OneConvId :: Local UserId -> Local UserId -> Galley r (Local ConvId)
localOne2OneConvId self other = do
  (x, y) <- toUUIDs (tUnqualified self) (tUnqualified other)
  pure . qualifyAs self $ Data.localOne2OneConvId x y

toUUIDs :: UserId -> UserId -> Galley r (U.UUID U.V4, U.UUID U.V4)
toUUIDs a b = do
  a' <- U.fromUUID (toUUID a) & ifNothing invalidUUID4
  b' <- U.fromUUID (toUUID b) & ifNothing invalidUUID4
  return (a', b')

accessRole :: NewConv -> AccessRole
accessRole b = fromMaybe Data.defRole (newConvAccessRole b)

access :: NewConv -> [Access]
access a = case Set.toList (newConvAccess a) of
  [] -> Data.defRegularConvAccess
  (x : xs) -> x : xs

newConvMembers :: Local x -> NewConv -> UserList UserId
newConvMembers loc body =
  UserList (newConvUsers body) []
    <> toUserList loc (newConvQualifiedUsers body)

ensureOne :: [a] -> Galley r a
ensureOne [x] = pure x
ensureOne _ = throwM (invalidRange "One-to-one conversations can only have a single invited member")
