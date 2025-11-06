module Wire.BrigAPIAccess.Rpc where

import Bilge
import Brig.Types.Intra (NewUserScimInvitation (..))
import Control.Monad.Catch (throwM)
import Data.Aeson
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Conversion
import Data.Code as Code
import Data.Handle (Handle (fromHandle))
import Data.HavePendingInvitations
import Data.Id
import Data.Misc
import Data.Qualified
import Data.Set qualified as Set
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as Lazy
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
import SAML2.WebSSO qualified as SAML
import System.Logger.Message qualified as Logger
import Util.Options
import Web.Cookie
import Web.HttpApiData
import Wire.API.Connection
import Wire.API.Error.Galley
import Wire.API.Locale
import Wire.API.MLS.CipherSuite
import Wire.API.Routes.Internal.Brig (CreateGroupFullRequest (..), GetBy)
import Wire.API.Routes.Internal.Brig.Connection
import Wire.API.Routes.Internal.Galley.TeamFeatureNoConfigMulti qualified as Multi
import Wire.API.Team.Export
import Wire.API.Team.Feature
import Wire.API.Team.LegalHold.Internal
import Wire.API.Team.Role
import Wire.API.Team.Size
import Wire.API.User (AccountStatus, AccountStatusResp (..), AccountStatusUpdate (..), EmailActivation (..), EmailAddress, EmailUpdate (..), HandleUpdate (..), LocaleUpdate (..), ManagedByUpdate (..), Name (..), NameUpdate (..), NewUserSpar (..), RichInfoUpdate (..), SelfProfile (..), UpdateConnectionsInternal, User, UserIds (..), UserSSOId (..), UserSet (..), VerificationAction, fromAccountStatusResp, userDeleted, userEmail, userId)
import Wire.API.User.Auth.LegalHold
import Wire.API.User.Auth.ReAuth
import Wire.API.User.Auth.Sso qualified as UserSso
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.User.Profile (ManagedBy)
import Wire.API.User.RichInfo
import Wire.API.UserGroup (NewUserGroup, UserGroup)
import Wire.BrigAPIAccess (BrigAPIAccess (..), OpaqueAuthToken (..))
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
      CreateGroupFull managedBy teamId creatorUserId newGroup ->
        createGroupFull managedBy teamId creatorUserId newGroup
      CreateSAML uref uid teamid name managedBy handle richInfo mLocale role ->
        createBrigUserSAML uref uid teamid name managedBy handle richInfo mLocale role
      CreateNoSAML extId email uid teamid name locale role ->
        createBrigUserNoSAML extId email uid teamid name locale role
      UpdateEmail uid email activation ->
        updateEmailAddress uid email activation
      GetAccount havePending uid ->
        getAccount havePending uid
      GetByHandle handle ->
        getByHandle handle
      GetByEmail email ->
        getByEmail email
      SetName uid name ->
        setName uid name
      SetHandle uid handle ->
        setHandle uid handle
      SetManagedBy uid managedBy ->
        setManagedBy uid managedBy
      SetSSOId uid ssoId ->
        setSSOId uid ssoId
      SetRichInfo uid richInfo ->
        setRichInfo uid richInfo
      SetLocale uid locale ->
        setLocale uid locale
      GetRichInfo uid ->
        getRichInfo uid
      CheckHandleAvailable handle ->
        checkHandleAvailable handle
      EnsureReAuthorised muid mpwd mcode maction ->
        ensureReAuthorised muid mpwd mcode maction
      SsoLogin uid ->
        ssoLogin uid
      GetStatus uid ->
        getAccountStatus uid
      GetStatusMaybe uid ->
        getAccountStatusMaybe uid
      SetStatus uid status ->
        setAccountStatus uid status
      GetDefaultUserLocale ->
        getDefaultUserLocale
      CheckAdminGetTeamId uid ->
        checkAdminGetTeamId uid

brigRequest :: (Member Rpc r, Member (Input Endpoint) r) => (Request -> Request) -> Sem r (Response (Maybe LByteString))
brigRequest req = do
  ep <- input
  rpcWithRetries "brig" ep req

decodeBodyOrThrow :: forall a r. (Typeable a, FromJSON a, Member (Error ParseException) r) => Text -> Response (Maybe LByteString) -> Sem r a
decodeBodyOrThrow ctx r = either (throw . ParseException ctx) pure (responseJsonEither r)

-- | Get statuses of all connections between two groups of users (the usual
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

-- | Calls 'Brig.API.Internal.createGroupFullInternalH'.
createGroupFull ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  ManagedBy ->
  TeamId ->
  Maybe UserId ->
  NewUserGroup ->
  Sem r UserGroup
createGroupFull managedBy teamId creatorUserId newGroup = do
  let req =
        CreateGroupFullRequest
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
        . expect2xx
  decodeBodyOrThrow "brig" r

-- | Helper function to convert response to SetCookie.
respToCookie :: (Member (Error ParseException) r) => ResponseLBS -> Sem r SetCookie
respToCookie resp = do
  unless (statusCode resp == 200) $ throw $ ParseException "brig" "Expected 200 status code"
  case getHeader "Set-Cookie" resp of
    Nothing -> throw $ ParseException "brig" "Could not retrieve cookie"
    Just cookieHeader -> pure $ parseSetCookie cookieHeader

createBrigUserSAML ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  SAML.UserRef ->
  UserId ->
  TeamId ->
  Name ->
  ManagedBy ->
  Maybe Handle ->
  Maybe RichInfo ->
  Maybe Locale ->
  Role ->
  Sem r UserId
createBrigUserSAML uref (Id buid) teamid name managedBy handle richInfo mLocale role = do
  let newUser =
        NewUserSpar
          { newUserSparUUID = buid,
            newUserSparDisplayName = name,
            newUserSparSSOId = UserSSOId uref,
            newUserSparTeamId = teamid,
            newUserSparManagedBy = managedBy,
            newUserSparHandle = handle,
            newUserSparRichInfo = richInfo,
            newUserSparLocale = mLocale,
            newUserSparRole = role
          }
  resp <-
    brigRequest $
      method POST
        . path "/i/users/spar"
        . json newUser
  if statusCode resp `elem` [200, 201]
    then userId . selfUser <$> decodeBodyOrThrow @SelfProfile "brig" resp
    else throw $ ParseException "brig" ("Failed to create SAML user: " ++ show (statusCode resp))

createBrigUserNoSAML ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  Text ->
  EmailAddress ->
  UserId ->
  TeamId ->
  Name ->
  Maybe Locale ->
  Role ->
  Sem r UserId
createBrigUserNoSAML extId email uid teamid uname locale role = do
  let newUser = NewUserScimInvitation teamid uid extId locale uname email role
  resp <-
    brigRequest $
      method POST
        . paths ["/i/teams", toByteString' teamid, "invitations"]
        . json newUser
  if statusCode resp `elem` [200, 201]
    then userId <$> decodeBodyOrThrow @User "brig" resp
    else throw $ ParseException "brig" ("Failed to create user from SCIM invitation: " ++ show (statusCode resp))

updateEmailAddress ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  UserId ->
  EmailAddress ->
  EmailActivation ->
  Sem r ()
updateEmailAddress buid email activation = do
  resp <-
    brigRequest $
      method PUT
        . path "/i/self/email"
        . header "Z-User" (toByteString' buid)
        . query
          [ ("activation", Just (toByteString' activation)),
            ("validate", Just (boolToBS validate)),
            ("activate", Just (boolToBS activate))
          ]
        . json (EmailUpdate email)
  case statusCode resp of
    204 -> pure ()
    202 -> pure ()
    _ -> throw $ ParseException "brig" ("Failed to update email: " ++ show (statusCode resp))
  where
    (validate, activate) = case activation of
      AutoActivate -> (False, True)
      SendActivationEmail -> (True, False)
    boolToBS :: Bool -> ByteString
    boolToBS True = "true"
    boolToBS False = "false"

getAccount ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  HavePendingInvitations ->
  UserId ->
  Sem r (Maybe User)
getAccount havePending buid = do
  resp <-
    brigRequest $
      method GET
        . paths ["/i/users"]
        . query
          [ ("ids", Just $ toByteString' buid),
            ( "includePendingInvitations",
              Just . toByteString' $
                case havePending of
                  WithPendingInvitations -> True
                  NoPendingInvitations -> False
            )
          ]
  case statusCode resp of
    200 ->
      decodeBodyOrThrow @[User] "brig" resp >>= \case
        [account] ->
          pure $
            if userDeleted account
              then Nothing
              else Just account
        _ -> pure Nothing
    404 -> pure Nothing
    _ -> throw $ ParseException "brig" ("Failed to get account: " ++ show (statusCode resp))

getByHandle ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  Handle ->
  Sem r (Maybe User)
getByHandle handle = do
  resp <-
    brigRequest $
      method GET
        . path "/i/users"
        . queryItem "handles" (toByteString' handle)
        . queryItem "includePendingInvitations" "true"
  case statusCode resp of
    200 -> listToMaybe <$> decodeBodyOrThrow @[User] "brig" resp
    404 -> pure Nothing
    _ -> throw $ ParseException "brig" ("Failed to get user by handle: " ++ show (statusCode resp))

getByEmail ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  EmailAddress ->
  Sem r (Maybe User)
getByEmail email = do
  resp <-
    brigRequest $
      method GET
        . path "/i/users"
        . queryItem "email" (toByteString' email)
        . queryItem "includePendingInvitations" "true"
  case statusCode resp of
    200 -> do
      macc <- listToMaybe <$> decodeBodyOrThrow @[User] "brig" resp
      case userEmail =<< macc of
        Just email' | email' == email -> pure macc
        _ -> pure Nothing
    404 -> pure Nothing
    _ -> throw $ ParseException "brig" ("Failed to get user by email: " ++ show (statusCode resp))

setName ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  UserId ->
  Name ->
  Sem r ()
setName buid (Name name) = do
  resp <-
    brigRequest $
      method PUT
        . paths ["/i/users", toByteString' buid, "name"]
        . json (NameUpdate name)
  let sCode = statusCode resp
  if sCode < 300
    then pure ()
    else throw $ ParseException "brig" ("Failed to set user name: " ++ show sCode)

setHandle ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  UserId ->
  Handle ->
  Sem r ()
setHandle buid handle = do
  resp <-
    brigRequest $
      method PUT
        . paths ["/i/users", toByteString' buid, "handle"]
        . json (HandleUpdate (fromHandle handle))
  case (statusCode resp, Wai.label <$> responseJsonMaybe @Wai.Error resp) of
    (200, Nothing) ->
      pure ()
    _ ->
      throw $ ParseException "brig" ("Failed to set user handle: " ++ show (statusCode resp))

setManagedBy ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  UserId ->
  ManagedBy ->
  Sem r ()
setManagedBy buid managedBy = do
  resp <-
    brigRequest $
      method PUT
        . paths ["/i/users", toByteString' buid, "managed-by"]
        . json (ManagedByUpdate managedBy)
  unless (statusCode resp == 200) $
    throw $
      ParseException "brig" ("Failed to set user managedBy: " ++ show (statusCode resp))

setSSOId ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  UserId ->
  UserSSOId ->
  Sem r ()
setSSOId buid ssoId = do
  resp <-
    brigRequest $
      method PUT
        . paths ["i", "users", toByteString' buid, "sso-id"]
        . json ssoId
  case statusCode resp of
    200 -> pure ()
    _ -> throw $ ParseException "brig" ("Failed to set user SSO ID: " ++ show (statusCode resp))

setRichInfo ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  UserId ->
  RichInfo ->
  Sem r ()
setRichInfo buid richInfo = do
  resp <-
    brigRequest $
      method PUT
        . paths ["i", "users", toByteString' buid, "rich-info"]
        . json (RichInfoUpdate $ unRichInfo richInfo)
  unless (statusCode resp == 200) $
    throw $
      ParseException "brig" ("Failed to set user rich info: " ++ show (statusCode resp))

setLocale ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  UserId ->
  Maybe Locale ->
  Sem r ()
setLocale buid = \case
  Just locale -> do
    resp <-
      brigRequest $
        method PUT
          . paths ["i", "users", toByteString' buid, "locale"]
          . json (LocaleUpdate locale)
    unless (statusCode resp == 200) $
      throw $
        ParseException "brig" ("Failed to set user locale: " ++ show (statusCode resp))
  Nothing -> do
    resp <-
      brigRequest $
        method DELETE
          . paths ["i", "users", toByteString' buid, "locale"]
    unless (statusCode resp == 200) $
      throw $
        ParseException "brig" ("Failed to delete user locale: " ++ show (statusCode resp))

getRichInfo ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  UserId ->
  Sem r RichInfo
getRichInfo buid = do
  resp <-
    brigRequest $
      method GET
        . paths ["/i/users", toByteString' buid, "rich-info"]
  case statusCode resp of
    200 -> decodeBodyOrThrow "brig" resp
    _ -> throw $ ParseException "brig" ("Failed to get user rich info: " ++ show (statusCode resp))

checkHandleAvailable ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  Handle ->
  Sem r Bool
checkHandleAvailable hnd = do
  resp <-
    brigRequest $
      method HEAD
        . paths ["/i/users/handles", toByteString' hnd]
  let sCode = statusCode resp
  if
    | sCode == 200 -> pure False
    | sCode == 404 -> pure True
    | otherwise -> throw $ ParseException "brig" ("Failed to check handle availability: " ++ show sCode)

ensureReAuthorised ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  Maybe UserId ->
  Maybe PlainTextPassword6 ->
  Maybe Code.Value ->
  Maybe VerificationAction ->
  Sem r ()
ensureReAuthorised Nothing _ _ _ = throw $ ParseException "brig" "Missing Z-User header"
ensureReAuthorised (Just uid) secret mbCode mbAction = do
  resp <-
    brigRequest $
      method GET
        . paths ["/i/users", toByteString' uid, "reauthenticate"]
        . json (ReAuthUser secret mbCode mbAction)
  case (statusCode resp, errorLabel resp) of
    (200, _) -> pure ()
    (403, Just "code-authentication-required") -> throw $ ParseException "brig" "Code authentication required"
    (403, Just "code-authentication-failed") -> throw $ ParseException "brig" "Code authentication failed"
    (403, _) -> throw $ ParseException "brig" "Re-authentication required"
    (_, _) -> throw $ ParseException "brig" ("Re-authentication failed: " ++ show (statusCode resp))
  where
    errorLabel :: ResponseLBS -> Maybe Lazy.Text
    errorLabel = fmap Wai.label . responseJsonMaybe

ssoLogin ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  UserId ->
  Sem r SetCookie
ssoLogin buid = do
  resp <-
    brigRequest $
      method POST
        . path "/i/sso-login"
        . json (UserSso.SsoLogin buid Nothing)
        . queryItem "persist" "true"
  if statusCode resp == 200
    then respToCookie resp
    else throw $ ParseException "brig" ("SSO login failed: " ++ show (statusCode resp))

getAccountStatus ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  UserId ->
  Sem r AccountStatus
getAccountStatus uid = do
  resp <- getStatusResp uid
  case statusCode resp of
    200 -> fromAccountStatusResp <$> decodeBodyOrThrow @AccountStatusResp "brig" resp
    _ -> throw $ ParseException "brig" ("Failed to get account status: " ++ show (statusCode resp))

getAccountStatusMaybe ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  UserId ->
  Sem r (Maybe AccountStatus)
getAccountStatusMaybe uid = do
  resp <- getStatusResp uid
  case statusCode resp of
    200 -> Just . fromAccountStatusResp <$> decodeBodyOrThrow @AccountStatusResp "brig" resp
    404 -> pure Nothing
    _ -> throw $ ParseException "brig" ("Failed to get account status: " ++ show (statusCode resp))

getStatusResp ::
  (Member Rpc r, Member (Input Endpoint) r) =>
  UserId ->
  Sem r ResponseLBS
getStatusResp uid = brigRequest $ method GET . paths ["/i/users", toByteString' uid, "status"]

setAccountStatus ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  UserId ->
  AccountStatus ->
  Sem r ()
setAccountStatus uid status = do
  resp <-
    brigRequest $
      method PUT
        . paths ["/i/users", toByteString' uid, "status"]
        . json (AccountStatusUpdate status)
  case statusCode resp of
    200 -> pure ()
    _ -> throw $ ParseException "brig" ("Failed to set account status: " ++ show (statusCode resp))

getDefaultUserLocale ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  Sem r Locale
getDefaultUserLocale = do
  resp <- brigRequest $ method GET . paths ["/i/users/locale"]
  case statusCode resp of
    200 -> luLocale <$> decodeBodyOrThrow @LocaleUpdate "brig" resp
    _ -> throw $ ParseException "brig" ("Failed to get default user locale: " ++ show (statusCode resp))

checkAdminGetTeamId ::
  (Member Rpc r, Member (Input Endpoint) r, Member (Error ParseException) r) =>
  UserId ->
  Sem r TeamId
checkAdminGetTeamId uid = do
  resp <- brigRequest $ method GET . paths ["/i/users", toByteString' uid, "check-admin-get-team-id"]
  case statusCode resp of
    200 -> decodeBodyOrThrow @TeamId "brig" resp
    _ -> throw $ ParseException "brig" ("Failed to check admin and get team ID: " ++ show (statusCode resp))
