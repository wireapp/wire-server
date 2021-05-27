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
    createConnectConversationH,
    ConversationResponses,
  )
where

import Control.Lens hiding ((??))
import Control.Monad.Catch
import Data.Id
import Data.List1 (list1)
import Data.Qualified (Qualified (..), partitionRemoteOrLocalIds')
import Data.Range
import qualified Data.Set as Set
import Data.Time
import qualified Data.UUID.Tagged as U
import Galley.API.Error
import Galley.API.Mapping
import Galley.API.Util
import Galley.App
import qualified Galley.Data as Data
import Galley.Intra.Push
import Galley.Types
import Galley.Types.Teams (ListType (..), Perm (..), TeamBinding (Binding), notTeamMember, teamMembers, userId)
import Galley.Validation
import Imports hiding ((\\))
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (setStatus)
import Network.Wai.Utilities
import Servant (WithStatus (..))
import qualified Servant
import Servant.API (Union)
import qualified Wire.API.Conversation as Public
import Wire.API.Routes.Public.Galley (ConversationResponses)

-- Servant helpers ------------------------------------------------------

conversationResponse :: ConversationResponse -> Galley (Union ConversationResponses)
conversationResponse (ConversationExisted c) =
  Servant.respond . WithStatus @200 . Servant.addHeader @"Location" (cnvId c) $ c
conversationResponse (ConversationCreated c) =
  Servant.respond . WithStatus @201 . Servant.addHeader @"Location" (cnvId c) $ c

-------------------------------------------------------------------------

----------------------------------------------------------------------------
-- Group conversations

-- | The public-facing endpoint for creating group conversations.
--
-- See Note [managed conversations].
createGroupConversation ::
  UserId ->
  ConnId ->
  Public.NewConvUnmanaged ->
  Galley (Union ConversationResponses)
createGroupConversation user conn wrapped@(Public.NewConvUnmanaged body) =
  conversationResponse =<< case newConvTeam body of
    Nothing -> createRegularGroupConv user conn wrapped
    Just tinfo -> createTeamGroupConv user conn tinfo body

-- | An internal endpoint for creating managed group conversations. Will
-- throw an error for everything else.
internalCreateManagedConversationH :: UserId ::: ConnId ::: JsonRequest NewConvManaged -> Galley Response
internalCreateManagedConversationH (zusr ::: zcon ::: req) = do
  newConv <- fromJsonBody req
  handleConversationResponse <$> internalCreateManagedConversation zusr zcon newConv

internalCreateManagedConversation :: UserId -> ConnId -> NewConvManaged -> Galley ConversationResponse
internalCreateManagedConversation zusr zcon (NewConvManaged body) = do
  case newConvTeam body of
    Nothing -> throwM internalError
    Just tinfo -> createTeamGroupConv zusr zcon tinfo body

-- | A helper for creating a regular (non-team) group conversation.
createRegularGroupConv :: UserId -> ConnId -> NewConvUnmanaged -> Galley ConversationResponse
createRegularGroupConv zusr zcon (NewConvUnmanaged body) = do
  name <- rangeCheckedMaybe (newConvName body)
  localDomain <- viewFederationDomain
  let unqualifiedUserIds = newConvUsers body
      qualifiedUserIds = newConvQualifiedUsers body
  let allUsers = map (`Qualified` localDomain) unqualifiedUserIds <> qualifiedUserIds
  checkedUsers <- checkedConvSize allUsers
  let checkedPartitionedUsers = partitionRemoteOrLocalIds' localDomain <$> checkedUsers
  let (remotes, locals) = fromConvSize checkedPartitionedUsers
  ensureConnected zusr locals
  checkRemoteUsersExist remotes
  -- FUTUREWORK: Implement (2) and (3) as per comments for Update.addMembers.
  c <-
    Data.createConversation
      zusr
      name
      (access body)
      (accessRole body)
      checkedPartitionedUsers
      (newConvTeam body)
      (newConvMessageTimer body)
      (newConvReceiptMode body)
      (newConvUsersRole body)
  notifyCreatedConversation Nothing zusr (Just zcon) c
  conversationCreated zusr c

-- | A helper for creating a team group conversation, used by the endpoint
-- handlers above. Allows both unmanaged and managed conversations.
createTeamGroupConv :: UserId -> ConnId -> Public.ConvTeamInfo -> Public.NewConv -> Galley ConversationResponse
createTeamGroupConv zusr zcon tinfo body = do
  name <- rangeCheckedMaybe (newConvName body)
  localDomain <- viewFederationDomain
  let unqualifiedUserIds = newConvUsers body
      qualifiedUserIds = newConvQualifiedUsers body
      allUserIds = map (`Qualified` localDomain) unqualifiedUserIds <> qualifiedUserIds
  let convTeam = cnvTeamId tinfo
  zusrMembership <- Data.teamMember convTeam zusr
  void $ permissionCheck CreateConversation zusrMembership
  checkedUsers <- checkedConvSize allUserIds
  let checkedPartitionedUsers = partitionRemoteOrLocalIds' localDomain <$> checkedUsers
      (_, localUserIds) = fromConvSize checkedPartitionedUsers
  convMemberships <- mapM (Data.teamMember convTeam) localUserIds
  ensureAccessRole (accessRole body) (zip localUserIds convMemberships)
  checkedPartitionedUsers' <-
    if cnvManaged tinfo
      then do
        -- ConvMaxSize MUST be < than hardlimit so the conv size check is enough
        maybeAllMembers <- Data.teamMembersForFanout convTeam
        let otherConvMems = filter (/= zusr) $ map (view userId) $ (maybeAllMembers ^. teamMembers)
        checkedLocalUsers <- checkedConvSize otherConvMems
        pure (fmap ([],) checkedLocalUsers)
      else do
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
        when (length allUserIds > 1) $ do
          void $ permissionCheck DoNotUseDeprecatedAddRemoveConvMember zusrMembership
        -- Team members are always considered to be connected, so we only check
        -- 'ensureConnected' for non-team-members.
        ensureConnectedToLocals zusr (notTeamMember localUserIds (catMaybes convMemberships))
        pure checkedPartitionedUsers
  conv <- Data.createConversation zusr name (access body) (accessRole body) checkedPartitionedUsers' (newConvTeam body) (newConvMessageTimer body) (newConvReceiptMode body) (newConvUsersRole body)
  now <- liftIO getCurrentTime
  -- NOTE: We only send (conversation) events to members of the conversation
  notifyCreatedConversation (Just now) zusr (Just zcon) conv
  conversationCreated zusr conv

----------------------------------------------------------------------------
-- Other kinds of conversations

createSelfConversation :: UserId -> Galley (Union ConversationResponses)
createSelfConversation zusr = do
  c <- Data.conversation (Id . toUUID $ zusr)
  conversationResponse
    =<< maybe create (conversationExisted zusr) c
  where
    create = do
      c <- Data.createSelfConversation zusr Nothing
      conversationCreated zusr c

createOne2OneConversation :: UserId -> ConnId -> NewConvUnmanaged -> Galley (Union ConversationResponses)
createOne2OneConversation zusr zcon (NewConvUnmanaged j) = do
  otherUserId <- head . fromRange <$> (rangeChecked (newConvUsers j) :: Galley (Range 1 1 [UserId]))
  (x, y) <- toUUIDs zusr otherUserId
  when (x == y) $
    throwM $
      invalidOp "Cannot create a 1-1 with yourself"
  case newConvTeam j of
    Just ti
      | cnvManaged ti -> throwM noManagedTeamConv
      | otherwise ->
        checkBindingTeamPermissions zusr otherUserId (cnvTeamId ti)
    Nothing -> do
      ensureConnected zusr [otherUserId]
  n <- rangeCheckedMaybe (newConvName j)
  c <- Data.conversation (Data.one2OneConvId x y)
  resp <- maybe (create x y n $ newConvTeam j) (conversationExisted zusr) c
  conversationResponse resp
  where
    verifyMembership tid u = do
      membership <- Data.teamMember tid u
      when (isNothing membership) $
        throwM noBindingTeamMembers
    checkBindingTeamPermissions x y tid = do
      zusrMembership <- Data.teamMember tid zusr
      void $ permissionCheck CreateConversation zusrMembership
      Data.teamBinding tid >>= \case
        Just Binding -> do
          verifyMembership tid x
          verifyMembership tid y
        Just _ -> throwM nonBindingTeam
        Nothing -> throwM teamNotFound
    create x y n tinfo = do
      c <- Data.createOne2OneConversation x y n (cnvTeamId <$> tinfo)
      notifyCreatedConversation Nothing zusr (Just zcon) c
      conversationCreated zusr c

createConnectConversationH :: UserId ::: Maybe ConnId ::: JsonRequest Connect -> Galley Response
createConnectConversationH (usr ::: conn ::: req) = do
  j <- fromJsonBody req
  handleConversationResponse <$> createConnectConversation usr conn j

createConnectConversation :: UserId -> Maybe ConnId -> Connect -> Galley ConversationResponse
createConnectConversation usr conn j = do
  (x, y) <- toUUIDs usr (cRecipient j)
  n <- rangeCheckedMaybe (cName j)
  conv <- Data.conversation (Data.one2OneConvId x y)
  maybe (create x y n) (update n) conv
  where
    create x y n = do
      (c, e) <- Data.createConnectConversation x y n j
      notifyCreatedConversation Nothing usr conn c
      for_ (newPush ListComplete (evtFrom e) (ConvEvent e) (recipient <$> Data.convMembers c)) $ \p ->
        push1 $
          p
            & pushRoute .~ RouteDirect
            & pushConn .~ conn
      conversationCreated usr c
    update n conv =
      let mems = Data.convMembers conv
       in conversationExisted usr
            =<< if
                | usr `isMember` mems ->
                  -- we already were in the conversation, maybe also other
                  connect n conv
                | otherwise -> do
                  now <- liftIO getCurrentTime
                  mm <- snd <$> Data.addMember now (Data.convId conv) usr
                  let conv' =
                        conv
                          { Data.convMembers = Data.convMembers conv <> toList mm
                          }
                  if null mems
                    then do
                      -- the conversation was empty
                      connect n conv'
                    else do
                      -- we were not in the conversation, but someone else
                      conv'' <- acceptOne2One usr conv' conn
                      if Data.convType conv'' == ConnectConv
                        then connect n conv''
                        else return conv''
    connect n conv
      | Data.convType conv == ConnectConv = do
        n' <- case n of
          Just x -> do
            Data.updateConversation (Data.convId conv) x
            return . Just $ fromRange x
          Nothing -> return $ Data.convName conv
        t <- liftIO getCurrentTime
        let e = Event ConvConnect (Data.convId conv) usr t (EdConnect j)
        for_ (newPush ListComplete (evtFrom e) (ConvEvent e) (recipient <$> Data.convMembers conv)) $ \p ->
          push1 $
            p
              & pushRoute .~ RouteDirect
              & pushConn .~ conn
        return $ conv {Data.convName = n'}
      | otherwise = return conv

-------------------------------------------------------------------------------
-- Helpers

data ConversationResponse
  = ConversationCreated !Public.Conversation
  | ConversationExisted !Public.Conversation

conversationCreated :: UserId -> Data.Conversation -> Galley ConversationResponse
conversationCreated usr cnv = ConversationCreated <$> conversationView usr cnv

conversationExisted :: UserId -> Data.Conversation -> Galley ConversationResponse
conversationExisted usr cnv = ConversationExisted <$> conversationView usr cnv

handleConversationResponse :: ConversationResponse -> Response
handleConversationResponse = \case
  ConversationCreated cnv -> json cnv & setStatus status201 . location (cnvId cnv)
  ConversationExisted cnv -> json cnv & setStatus status200 . location (cnvId cnv)

notifyCreatedConversation :: Maybe UTCTime -> UserId -> Maybe ConnId -> Data.Conversation -> Galley ()
notifyCreatedConversation dtime usr conn c = do
  now <- maybe (liftIO getCurrentTime) pure dtime
  pushSome =<< mapM (toPush now) (Data.convMembers c)
  where
    route
      | Data.convType c == RegularConv = RouteAny
      | otherwise = RouteDirect
    toPush t m = do
      c' <- conversationView (memId m) c
      let e = Event ConvCreate (Data.convId c) usr t (EdConversation c')
      return $
        newPush1 ListComplete (evtFrom e) (ConvEvent e) (list1 (recipient m) [])
          & pushConn .~ conn
          & pushRoute .~ route

toUUIDs :: UserId -> UserId -> Galley (U.UUID U.V4, U.UUID U.V4)
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
