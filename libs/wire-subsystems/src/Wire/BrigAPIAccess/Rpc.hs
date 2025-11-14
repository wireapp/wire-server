-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.BrigAPIAccess.Rpc where

import Bilge
import Control.Monad.Catch (throwM)
import Data.Aeson
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Conversion
import Data.Id
import Data.Misc
import Data.Qualified
import Data.Set qualified as Set
import Data.Text.Encoding qualified as Text
import Imports
import Network.HTTP.Client (HttpExceptionContent (..))
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types (StdMethod (..))
import Network.HTTP.Types.Status hiding (statusCode)
import Network.Wai.Utilities.Error qualified as Wai
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog
import System.Logger.Message qualified as Logger
import Util.Options
import Web.HttpApiData
import Web.Scim.Filter as Scim
import Wire.API.Connection
import Wire.API.Error.Galley
import Wire.API.MLS.CipherSuite
import Wire.API.Routes.Internal.Brig (CreateGroupInternalRequest (..), GetBy, UpdateGroupInternalRequest (..))
import Wire.API.Routes.Internal.Brig.Connection
import Wire.API.Routes.Internal.Galley.TeamFeatureNoConfigMulti qualified as Multi
import Wire.API.Team.Export
import Wire.API.Team.Feature
import Wire.API.Team.LegalHold.Internal
import Wire.API.Team.Size
import Wire.API.User (UpdateConnectionsInternal, User, UserIds (..), UserSet (..))
import Wire.API.User.Auth.LegalHold
import Wire.API.User.Auth.ReAuth
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.User.Profile (ManagedBy)
import Wire.API.User.RichInfo
import Wire.API.UserGroup (NewUserGroup, UserGroup)
import Wire.API.UserGroup.Pagination
import Wire.BrigAPIAccess (BrigAPIAccess (..), DeleteGroupManagedError (..), OpaqueAuthToken (..))
import Wire.ParseException
import Wire.Rpc

interpretBrigAccess ::
  ( Member TinyLog r,
    Member Rpc r,
    Member (Error ParseException) r
  ) =>
  Endpoint ->
  Sem (BrigAPIAccess ': r) a ->
  Sem r a
interpretBrigAccess brigEndpoint =
  interpret $
    runInputConst brigEndpoint . \case
      GetConnectionsUnqualified uids muids mrel -> do
        getConnectionsUnqualified uids muids mrel
      GetConnections uids mquids mrel -> do
        getConnections uids mquids mrel
      PutConnectionInternal uc -> do
        putConnectionInternal uc
      ReauthUser uid reauth -> do
        reAuthUser uid reauth
      LookupActivatedUsers uids -> do
        lookupActivatedUsers uids
      GetUsers uids -> do
        getUsers uids
      DeleteUser uid -> do
        deleteUser uid
      GetContactList uid -> do
        getContactList uid
      GetRichInfoMultiUser uids -> do
        getRichInfoMultiUser uids
      GetUserExportData uid -> do
        getUserExportData uid
      GetSize tid -> do
        getSize tid
      LookupClients uids -> do
        lookupClients uids
      LookupClientsFull uids -> do
        lookupClientsFull uids
      NotifyClientsAboutLegalHoldRequest self other pk -> do
        notifyClientsAboutLegalHoldRequest self other pk
      GetLegalHoldAuthToken uid mpwd -> do
        getLegalHoldAuthToken uid mpwd
      AddLegalHoldClientToUserEither uid conn pks lpk -> do
        addLegalHoldClientToUser uid conn pks lpk
      RemoveLegalHoldClientFromUser uid -> do
        removeLegalHoldClientFromUser uid
      GetAccountConferenceCallingConfigClient uid -> do
        getAccountConferenceCallingConfigClient uid
      GetLocalMLSClients qusr ss -> do
        getLocalMLSClients qusr ss
      GetLocalMLSClient qusr cid ss -> do
        getLocalMLSClient qusr cid ss
      UpdateSearchVisibilityInbound status -> do
        updateSearchVisibilityInbound status
      DeleteBot convId botId ->
        deleteBot convId botId
      UpdateSearchIndex uid -> updateSearchIndex uid
      GetAccountsBy localGetBy ->
        getAccountsBy localGetBy
      CreateGroupInternal managedBy teamId creatorUserId newGroup ->
        createGroupInternal managedBy teamId creatorUserId newGroup
      GetGroupsInternal tid mbFilter ->
        getGroupsInternal tid mbFilter
      GetGroupInternal tid gid includeChannels ->
        getGroupInternal tid gid includeChannels
      UpdateGroup req ->
        updateGroup req
      DeleteGroupInternal managedBy teamId groupId ->
        deleteGroupInternal managedBy teamId groupId

brigRequest :: (Member Rpc r, Member (Input Endpoint) r) => (Request -> Request) -> Sem r (Response (Maybe LByteString))
brigRequest req = do
  ep <- input
  rpcWithRetries "brig" ep req

decodeBodyOrThrow :: forall a r. (Typeable a, FromJSON a, Member (Error ParseException) r) => Text -> Response (Maybe LByteString) -> Sem r a
decodeBodyOrThrow ctx r = either (throw . ParseException ctx) pure (responseJsonEither r)

-- \| Get statuses of all connections between two groups of users (the usual
-- pattern is to check all connections from one user to several, or from
-- several users to one).
--
-- When a connection does not exist, it is skipped.
-- Calls 'Brig.API.Internal.getConnectionsStatusUnqualified'.
getConnectionsUnqualified ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  [UserId] ->
  Maybe [UserId] ->
  Maybe Relation ->
  Sem r [ConnectionStatus]
getConnectionsUnqualified uFrom uTo rlt = do
  r <-
    brigRequest $
      method POST
        . path "/i/users/connections-status"
        . maybe id rfilter rlt
        . json ConnectionsStatusRequest {csrFrom = uFrom, csrTo = uTo}
        . expect2xx
  decodeBodyOrThrow "brig" r
  where
    rfilter = queryItem "filter" . (BSC.pack . map toLower . show)

-- | Get statuses of all connections between two groups of users (the usual
-- pattern is to check all connections from one user to several, or from
-- several users to one).
--
-- When a connection does not exist, it is skipped.
-- Calls 'Brig.API.Internal.getConnectionsStatus'.
getConnections ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  [UserId] ->
  Maybe [Qualified UserId] ->
  Maybe Relation ->
  Sem r [ConnectionStatusV2]
getConnections [] _ _ = pure []
getConnections uFrom uTo rlt = do
  r <-
    brigRequest $
      method POST
        . path "/i/users/connections-status/v2"
        . json (ConnectionsStatusRequestV2 uFrom uTo rlt)
        . expect2xx
  decodeBodyOrThrow "brig" r

putConnectionInternal ::
  (Member Rpc r, Member (Input Endpoint) r) =>
  UpdateConnectionsInternal ->
  Sem r Status
putConnectionInternal updateConn = do
  response <-
    brigRequest $
      method PUT
        . paths ["/i/connections/connection-update"]
        . json updateConn
  pure $ responseStatus response

deleteBot ::
  (Member Rpc r, Member (Input Endpoint) r) =>
  ConvId ->
  BotId ->
  Sem r ()
deleteBot cid bot = do
  void $
    brigRequest $
      method DELETE
        . path "/bot/self"
        . header "Z-Type" "bot"
        . header "Z-Bot" (toByteString' bot)
        . header "Z-Conversation" (toByteString' cid)
        . expect2xx

-- | Calls 'Brig.User.API.Auth.reAuthUserH'.
reAuthUser ::
  (Member Rpc r, Member (Input Endpoint) r) =>
  UserId ->
  ReAuthUser ->
  Sem r (Either AuthenticationError ())
reAuthUser uid auth = do
  let req =
        method GET
          . paths ["/i/users", toByteString' uid, "reauthenticate"]
          . json auth
  resp <- brigRequest (check [status200, status403, status429] . req)
  pure $ case (statusCode resp, errorLabel resp) of
    (200, _) -> Right ()
    (403, Just "code-authentication-required") -> Left VerificationCodeRequired
    (403, Just "code-authentication-failed") -> Left VerificationCodeAuthFailed
    (403, _) -> Left ReAuthFailed
    (429, _) -> Left RateLimitExceeded
    (_, _) -> Left ReAuthFailed
  where
    errorLabel :: ResponseLBS -> Maybe LText
    errorLabel = fmap Wai.label . responseJsonMaybe

check :: [Status] -> Request -> Request
check allowed r =
  r
    { Http.checkResponse = \rq rs ->
        unless (responseStatus rs `elem` allowed) $
          let ex = StatusCodeException (rs {responseBody = ()}) mempty
           in throwM $ HttpExceptionRequest rq ex
    }

-- | Calls 'Brig.API.listActivatedAccountsH'.
lookupActivatedUsers :: (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) => [UserId] -> Sem r [User]
lookupActivatedUsers = chunkify $ \uids -> do
  let users = BSC.intercalate "," $ toByteString' <$> uids
  r <-
    brigRequest $
      method GET
        . path "/i/users"
        . queryItem "ids" users
        . expect2xx
  decodeBodyOrThrow "brig" r

-- | URLs with more than ~160 uids produce 400 responses, because HAProxy has a
--   URL length limit of ~6500 (determined experimentally). 100 is a
--   conservative setting. A uid contributes about 36+3 characters (+3 for the
--   comma separator) to the overall URL length.
chunkify :: forall m key a. (Monad m, Monoid a) => ([key] -> m a) -> [key] -> m a
chunkify doChunk keys = mconcat <$> (doChunk `mapM` chunks keys)
  where
    maxSize :: Int
    maxSize = 100

    chunks :: [any] -> [[any]]
    chunks [] = []
    chunks uids = case splitAt maxSize uids of (h, t) -> h : chunks t

-- | Calls 'Brig.API.listActivatedAccountsH'.
getUsers ::
  (Member Rpc r, Member (Input Endpoint) r) =>
  [UserId] ->
  Sem r [User]
getUsers = chunkify $ \uids -> do
  resp <-
    brigRequest $
      method GET
        . path "/i/users"
        . queryItem "ids" (BSC.intercalate "," (toByteString' <$> uids))
        . expect2xx
  pure . fromMaybe [] . responseJsonMaybe $ resp

-- | Calls 'Brig.API.deleteUserNoAuthH'.
deleteUser ::
  (Member Rpc r, Member (Input Endpoint) r) =>
  UserId ->
  Sem r ()
deleteUser uid = do
  void $
    brigRequest $
      method DELETE
        . paths ["/i/users", toByteString' uid]
        . expect2xx

-- | Calls 'Brig.API.getContactListH'.
getContactList ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  UserId ->
  Sem r [UserId]
getContactList uid = do
  r <-
    brigRequest $
      method GET
        . paths ["/i/users", toByteString' uid, "contacts"]
        . expect2xx
  cUsers <$> decodeBodyOrThrow "brig" r

-- | Calls 'Brig.API.Internal.getRichInfoMultiH'
getRichInfoMultiUser ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  [UserId] ->
  Sem r [(UserId, RichInfo)]
getRichInfoMultiUser = chunkify $ \uids -> do
  resp <-
    brigRequest $
      method GET
        . paths ["/i/users/rich-info"]
        . queryItem "ids" (toByteString' (List uids))
        . expect2xx
  decodeBodyOrThrow "brig" resp

-- | Calls 'Brig.API.Internal.getUserExportDataH'
getUserExportData ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  UserId ->
  Sem r (Maybe TeamExportUser)
getUserExportData uid = do
  resp <-
    brigRequest $
      method GET
        . paths ["i/users", toByteString' uid, "export-data"]
        . expect2xx
  decodeBodyOrThrow "brig" resp

getAccountConferenceCallingConfigClient ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  UserId ->
  Sem r (Feature ConferenceCallingConfig)
getAccountConferenceCallingConfigClient uid = do
  resp <-
    brigRequest $
      method GET
        . paths ["i", "users", toByteString' uid, "features", "conferenceCalling"]
        . expect2xx
  decodeBodyOrThrow "brig" resp

updateSearchVisibilityInbound ::
  (Member Rpc r, Member (Input Endpoint) r) =>
  Multi.TeamStatus SearchVisibilityInboundConfig ->
  Sem r ()
updateSearchVisibilityInbound update = do
  void . brigRequest $
    method POST
      . paths ["i", "teams"]
      . json update
      . expect2xx

getSize ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  TeamId ->
  Sem r TeamSize
getSize tid = do
  r <-
    brigRequest $
      method GET
        . paths ["/i/teams", toByteString' tid, "size"]
        . expect2xx
  decodeBodyOrThrow "brig" r

-- | Calls 'Brig.API.internalListClientsH'.
lookupClients ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  [UserId] ->
  Sem r UserClients
lookupClients uids = do
  r <-
    brigRequest $
      method POST
        . path "/i/clients"
        . json (UserSet $ Set.fromList uids)
        . expect2xx
  clients <- decodeBodyOrThrow "brig" r
  pure $ filterClients (not . Set.null) clients

-- | Calls 'Brig.API.internalListClientsFullH'.
lookupClientsFull ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  [UserId] ->
  Sem r UserClientsFull
lookupClientsFull uids = do
  r <-
    brigRequest $
      method POST
        . path "/i/clients/full"
        . json (UserSet $ Set.fromList uids)
        . expect2xx
  clients <- decodeBodyOrThrow "brig" r
  pure $ filterClientsFull (not . Set.null) clients

-- | Calls 'Brig.API.legalHoldClientRequestedH'.
notifyClientsAboutLegalHoldRequest ::
  (Member Rpc r, Member (Input Endpoint) r) =>
  UserId ->
  UserId ->
  LastPrekey ->
  Sem r ()
notifyClientsAboutLegalHoldRequest requesterUid targetUid lastPrekey' = do
  void . brigRequest $
    method POST
      . paths ["i", "clients", "legalhold", toByteString' targetUid, "request"]
      . json (LegalHoldClientRequest requesterUid lastPrekey')
      . expect2xx

-- | Calls 'Brig.User.API.Auth.legalHoldLoginH'.
getLegalHoldAuthToken ::
  ( Member TinyLog r,
    Member (Input Endpoint) r,
    Member Rpc r,
    Member (Error ParseException) r
  ) =>
  UserId ->
  Maybe PlainTextPassword6 ->
  Sem r OpaqueAuthToken
getLegalHoldAuthToken uid pw = do
  r <-
    brigRequest $
      method POST
        . path "/i/legalhold-login"
        . queryItem "persist" "true"
        . json (LegalHoldLogin uid pw Nothing)
        . expect2xx
  case getCookieValue "zuid" r of
    Nothing -> do
      warn $ Logger.msg @Text "Response from login missing auth cookie"
      throw $ ParseException "brig" "Response from login missing auth cookie"
    Just c -> pure . OpaqueAuthToken . Text.decodeUtf8 $ c

-- | Calls 'Brig.API.addClientInternalH'.
addLegalHoldClientToUser ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  UserId ->
  ConnId ->
  [Prekey] ->
  LastPrekey ->
  Sem r (Either AuthenticationError ClientId)
addLegalHoldClientToUser uid connId prekeys lastPrekey' = do
  fmap (.clientId) <$> brigAddClient uid connId lhClient
  where
    lhClient =
      NewClient
        prekeys
        lastPrekey'
        LegalHoldClientType
        Nothing
        (Just LegalHoldClient)
        Nothing
        Nothing
        Nothing
        Nothing
        mempty
        Nothing

-- | Calls 'Brig.API.removeLegalHoldClientH'.
removeLegalHoldClientFromUser ::
  (Member Rpc r, Member (Input Endpoint) r) =>
  UserId ->
  Sem r ()
removeLegalHoldClientFromUser targetUid = do
  void . brigRequest $
    method DELETE
      . paths ["i", "clients", "legalhold", toByteString' targetUid]
      . contentJson
      . expect2xx

-- | Calls 'Brig.API.addClientInternalH'.
brigAddClient ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  UserId ->
  ConnId ->
  NewClient ->
  Sem r (Either AuthenticationError Client)
brigAddClient uid connId client = do
  r <-
    brigRequest $
      method POST
        . header "Z-Connection" (toByteString' connId)
        . paths ["i", "clients", toByteString' uid]
        . contentJson
        . json client
        . expectStatus (flip elem [201, 403])
  if statusCode r == 201
    then Right <$> decodeBodyOrThrow "brig" r
    else pure (Left ReAuthFailed)

-- | Calls 'Brig.API.Internal.getMLSClients'.
getLocalMLSClients ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  Local UserId ->
  CipherSuiteTag ->
  Sem r (Set ClientInfo)
getLocalMLSClients lusr suite =
  brigRequest
    ( method GET
        . paths ["i", "mls", "clients", toByteString' (tUnqualified lusr)]
        . queryItem
          "ciphersuite"
          (toHeader (tagCipherSuite suite))
        . expect2xx
    )
    >>= decodeBodyOrThrow "brig"

-- | Calls 'Brig.API.Internal.getMLSClient'.
getLocalMLSClient ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  Local UserId ->
  ClientId ->
  CipherSuiteTag ->
  Sem r ClientInfo
getLocalMLSClient lusr cid suite =
  brigRequest
    ( method GET
        . paths
          ["i", "mls", "client", toByteString' (tUnqualified lusr), toByteString' cid]
        . queryItem
          "ciphersuite"
          (toHeader (tagCipherSuite suite))
        . expect2xx
    )
    >>= decodeBodyOrThrow "brig"

updateSearchIndex ::
  (Member Rpc r, Member (Input Endpoint) r) =>
  UserId ->
  Sem r ()
updateSearchIndex uid = do
  void . brigRequest $
    method POST
      . paths ["i", "index", "update", toByteString' uid]
      . expect2xx

-- | Calls 'Brig.API.Internal.getAccountsByInternalH'.
getAccountsBy ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  GetBy ->
  Sem r [User]
getAccountsBy localGetBy = do
  r <-
    brigRequest $
      method POST
        . path "/i/users/accounts-by"
        . json localGetBy
        . expect2xx
  decodeBodyOrThrow "brig" r

-- | Calls 'Brig.API.Internal.createGroupInternalH'.
createGroupInternal ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  ManagedBy ->
  TeamId ->
  Maybe UserId ->
  NewUserGroup ->
  Sem r (Either Wai.Error UserGroup)
createGroupInternal managedBy teamId creatorUserId newGroup = do
  let req =
        CreateGroupInternalRequest
          { managedBy,
            teamId,
            creatorUserId,
            newGroup
          }
  r <-
    brigRequest $
      method POST
        . path "/i/user-groups/full"
        . json req
  if statusCode r >= 200 && statusCode r < 300
    then Right <$> decodeBodyOrThrow @UserGroup "brig" r
    else Left <$> decodeBodyOrThrow @Wai.Error "brig" r

getGroupInternal ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  TeamId ->
  UserGroupId ->
  Bool ->
  Sem r (Maybe UserGroup)
getGroupInternal tid gid includeChannels = do
  r <-
    brigRequest $
      method GET
        . paths ["i", "user-groups", toByteString' tid, toByteString' gid, toByteString' includeChannels]
        . expect2xx
  decodeBodyOrThrow "brig" r

getGroupsInternal ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  TeamId ->
  Maybe Scim.Filter ->
  Sem r UserGroupPage
getGroupsInternal tid mbFilter = do
  maybeDisplayName :: Maybe Text <- case mbFilter of
    Just filter' -> case filter' of
      FilterAttrCompare (AttrPath _schema "displayName" Nothing) OpCo (ValString str) -> pure $ Just str
      _ -> throw $ ParseException "brig" $ "Unsupported SCIM filter: " <> show filter'
    Nothing -> pure Nothing
  r <-
    brigRequest $
      method GET
        . paths ["i", "user-groups", toByteString' tid]
        . maybe id (queryItem "nameContains" . Text.encodeUtf8) maybeDisplayName
        . expect2xx
  decodeBodyOrThrow "brig" r

updateGroup ::
  (Member Rpc r, Member (Input Endpoint) r) =>
  UpdateGroupInternalRequest ->
  Sem r ()
updateGroup reqBody =
  void $
    brigRequest $
      method PUT
        . paths ["i", "user-groups"]
        . json reqBody
        . expect2xx

deleteGroupInternal ::
  ( Member Rpc r,
    Member (Input Endpoint) r,
    Member (Error ParseException) r
  ) =>
  ManagedBy ->
  TeamId ->
  UserGroupId ->
  Sem r (Either DeleteGroupManagedError ())
deleteGroupInternal managedBy teamId groupId = do
  resp <-
    brigRequest $
      method DELETE
        . paths ["i", "user-groups", toByteString' teamId, toByteString' groupId, "managed", toByteString' managedBy]
  case (statusCode resp, errorLabel resp) of
    (status, _) | status >= 200 && status < 300 -> pure $ Right ()
    (403, Just "user-group-managed-by-mismatch") -> pure $ Left DeleteGroupManagedManagedByMismatch
    (status, label) ->
      throw $
        ParseException
          { _parseExceptionRemote = "brig",
            _parseExceptionMsg = "unexpected delete group managed response: " <> show status <> " / " <> show label
          }
  where
    errorLabel :: ResponseLBS -> Maybe LText
    errorLabel = fmap Wai.label . responseJsonMaybe
