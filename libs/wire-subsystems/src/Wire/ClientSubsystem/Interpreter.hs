module Wire.ClientSubsystem.Interpreter
  ( runClientSubsystem,
    ClientError (..),
    ClientDataError (..),
    ClientSubsystemConfig (..),
  )
where

import Control.Monad
import Data.Default
import Data.Domain
import Data.Id
import Data.Json.Util (ToJSONObject (..), toUTCTimeMillis)
import Data.Map qualified as Map
import Data.Qualified
import Data.Set qualified as Set
import Data.Time.Clock
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
import Servant.Client.Core (RunClient)
import System.Logger.Message
import Wire.API.Federation.API
import Wire.API.Federation.API.Brig as FederatedBrig
import Wire.API.Federation.Error
import Wire.API.Push.V2 qualified as V2
import Wire.API.User as User
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.UserEvent
import Wire.API.UserMap
import Wire.AuthenticationSubsystem (AuthenticationSubsystem)
import Wire.AuthenticationSubsystem qualified as Authentication
import Wire.AuthenticationSubsystem.Error
import Wire.ClientStore (ClientStore, DuplicateMLSPublicKey (..))
import Wire.ClientStore qualified as ClientStore
import Wire.ClientSubsystem (ClientSubsystem (..), ReAuthPolicy (..))
import Wire.ClientSubsystem.Error
import Wire.DeleteQueue (DeleteQueue)
import Wire.DeleteQueue qualified as DeleteQueue
import Wire.EmailSubsystem (EmailSubsystem)
import Wire.EmailSubsystem qualified as Email
import Wire.Events as Events
import Wire.FederationAPIAccess (FederationAPIAccess, runFederatedEither)
import Wire.GalleyAPIAccess as GalleyAPIAccess
import Wire.NotificationSubsystem
import Wire.Sem.Logger qualified as Log
import Wire.Sem.Now qualified as Now
import Wire.UserSubsystem (UserSubsystem)
import Wire.UserSubsystem qualified as User
import Wire.Util

data ClientSubsystemConfig = ClientSubsystemConfig
  { userMaxPermClients :: Int,
    consumableNotificationsEnabled :: Bool
  }

runClientSubsystem ::
  ( Member ClientStore r,
    Member (Input (Local ())) r,
    Member TinyLog r,
    Member (FederationAPIAccess m) r,
    RunClient (m 'Brig),
    FederationMonad m,
    Typeable m,
    Member (Error ClientError) r,
    Member Now.Now r,
    Member NotificationSubsystem r,
    Member GalleyAPIAccess r,
    Member Events r,
    Member EmailSubsystem r,
    Member DeleteQueue r,
    Member (Input ClientSubsystemConfig) r
  ) =>
  InterpreterFor AuthenticationSubsystem (UserSubsystem ': r) ->
  InterpreterFor UserSubsystem r ->
  InterpreterFor ClientSubsystem r
runClientSubsystem runAuth runUser =
  interpret $
    runUser . runAuth . \case
      InternalGetActivityTimestamps uid -> internalGetActivityTimestamps uid
      LookupLocalClient uid cid -> lookupLocalClient uid cid
      LookupLocalClients uid -> lookupLocalClients uid
      LookupLocalPublicClientsBulk uids -> lookupLocalPublicClientsBulk uids
      LookupPublicClient quid cid -> lookupPubClient quid cid
      LookupPublicClients quid -> lookupPubClients quid
      LookupPublicClientsBulk quids -> lookupPubClientsBulk quids
      AddClient luid conn new -> addClient def luid conn new
      AddClientWithPolicy policy luid conn new -> addClient policy luid conn new
      UpsertClient luid client new capabilities -> mapError ClientDataError $ upsertClient def luid client new capabilities
      OnClientEvent uid con event -> onClientEvent uid con event
      EnqueueClientDeletion uid con client -> execDelete uid con client

-- nb. We must ensure that the set of clients known to brig is always
-- a superset of the clients known to galley.
addClient ::
  ( Member UserSubsystem r,
    Member (Error ClientError) r,
    Member ClientStore r,
    Member Now.Now r,
    Member AuthenticationSubsystem r,
    Member NotificationSubsystem r,
    Member GalleyAPIAccess r,
    Member Events r,
    Member EmailSubsystem r,
    Member DeleteQueue r,
    Member (Input ClientSubsystemConfig) r
  ) =>
  ReAuthPolicy ->
  Local UserId ->
  Maybe ConnId ->
  NewClient ->
  Sem r Client
addClient policy luid@(tUnqualified -> uid) con new = do
  conf <- input
  usr <- User.getAccountNoFilter luid >>= maybe (throw (ClientUserNotFound uid)) pure
  verifyCode (newClientVerificationCode new)
  let mCapabilities :: Maybe ClientCapabilityList
      mCapabilities = updateLhDevice $ newClientCapabilities new
        where
          updateLhDevice :: Maybe ClientCapabilityList -> Maybe ClientCapabilityList
          updateLhDevice =
            if newClientType new == LegalHoldClientType
              then Just . ClientCapabilityList . maybe (Set.singleton implicitConsent) (Set.insert implicitConsent . fromClientCapabilityList)
              else id
          implicitConsent = ClientSupportsLegalholdImplicitConsent
  (client0, old, count) <- mapError ClientDataError (upsertClient policy luid clientId new mCapabilities)
  let client = client0 {clientMLSPublicKeys = newClientMLSPublicKeys new}
  when (conf.consumableNotificationsEnabled && supportsConsumableNotifications client) $
    setupConsumableNotifications uid client.clientId
  for_ old $ execDelete uid con
  GalleyAPIAccess.newClient uid client.clientId
  onClientEvent uid con (ClientAdded client)
  when (clientType client == LegalHoldClientType) $ Events.generateUserEvent uid con (UserLegalHoldEnabled uid)
  when (count > 1) $
    for_ (userEmail usr) $ \email ->
      Email.sendNewClientEmail email (userDisplayName usr) client (userLocale usr)
  pure client
  where
    clientId = clientIdFromPrekey (unpackLastPrekey $ newClientLastKey new)

    verifyCode mbCode =
      -- this only happens inside the login flow (in particular, when logging in from a new device)
      -- the code obtained for logging in is used a second time for adding the device
      Authentication.enforceVerificationCodeEither luid mbCode User.Login >>= \case
        Left VerificationCodeRequired -> throw ClientCodeAuthenticationRequired
        Left VerificationCodeNoPendingCode -> throw ClientCodeAuthenticationFailed
        Left VerificationCodeNoEmail -> throw ClientCodeAuthenticationFailed
        Right () -> pure ()

upsertClient ::
  forall r.
  ( Member AuthenticationSubsystem r,
    Member ClientStore r,
    Member Now.Now r,
    Member (Error ClientDataError) r,
    Member (Input ClientSubsystemConfig) r
  ) =>
  ReAuthPolicy ->
  Local UserId ->
  ClientId ->
  NewClient ->
  Maybe ClientCapabilityList ->
  Sem r (Client, [Client], Word)
upsertClient (ReAuthPolicy reAuthPolicy) u newId c caps = do
  conf <- input
  clients <- ClientStore.lookupClients (tUnqualified u)
  let typed = filter ((== newClientType c) . clientType) clients
      count = length typed
      upsert = any exists typed
  when (reAuthPolicy count upsert) do
    (Authentication.reauthenticateEither (tUnqualified u) (newClientPassword c))
      >>= either (throw . ClientReAuthError) pure
  let capacity = fmap (+ (-count)) (limit conf)
  unless (maybe True (> 0) capacity || upsert) $ throw TooManyClients
  new <- insert (tUnqualified u)
  let !total = fromIntegral (length clients + if upsert then 0 else 1)
      old = maybe (filter (not . exists) typed) (const []) (limit conf)
  pure (new, old, total)
  where
    limit :: ClientSubsystemConfig -> Maybe Int
    limit conf = case newClientType c of
      PermanentClientType -> Just conf.userMaxPermClients
      TemporaryClientType -> Nothing
      LegalHoldClientType -> Nothing

    exists :: Client -> Bool
    exists = (==) newId . (.clientId)

    insert uid = do
      now <- toUTCTimeMillis <$> Now.get
      let prekeys = unpackLastPrekey (newClientLastKey c) : newClientPrekeys c
      unless (all checkPrekeyBundle prekeys) $
        throw MalformedPrekeys
      mErr <- ClientStore.upsert uid newId now (c {newClientCapabilities = caps})
      case mErr of
        Just DuplicateMLSPublicKey -> throw MLSPublicKeyDuplicate
        Nothing ->
          pure $!
            Client
              { clientId = newId,
                clientType = newClientType c,
                clientTime = now,
                clientClass = newClientClass c,
                clientLabel = newClientLabel c,
                clientCookie = newClientCookie c,
                clientModel = newClientModel c,
                clientCapabilities = fromMaybe mempty caps,
                clientMLSPublicKeys = mempty,
                clientLastActive = Nothing
              }

internalGetActivityTimestamps :: (Member ClientStore r) => UserId -> Sem r [Maybe UTCTime]
internalGetActivityTimestamps = ClientStore.getActivityTimestamps

lookupLocalClient :: (Member ClientStore r) => UserId -> ClientId -> Sem r (Maybe Client)
lookupLocalClient uid = ClientStore.lookupClient uid

lookupLocalClients :: (Member ClientStore r) => UserId -> Sem r [Client]
lookupLocalClients = ClientStore.lookupClients

lookupPubClient ::
  ( Member ClientStore r,
    Member (Input (Local ())) r,
    Member TinyLog r,
    Member (FederationAPIAccess m) r,
    RunClient (m 'Brig),
    FederationMonad m,
    Typeable m
  ) =>
  Qualified UserId -> ClientId -> Sem r (Maybe PubClient)
lookupPubClient qid cid = do
  clients <- lookupPubClients qid
  pure $ find ((== cid) . pubClientId) clients

lookupPubClients ::
  ( Member ClientStore r,
    Member (Input (Local ())) r,
    Member TinyLog r,
    Member (FederationAPIAccess m) r,
    RunClient (m 'Brig),
    FederationMonad m,
    Typeable m
  ) =>
  Qualified UserId -> Sem r [PubClient]
lookupPubClients qid@(Qualified uid domain) = do
  getForUser <$> lookupPubClientsBulk [qid]
  where
    getForUser :: QualifiedUserMap (Set PubClient) -> [PubClient]
    getForUser qmap = fromMaybe [] $ do
      um <- userMap <$> Map.lookup domain (qualifiedUserMap qmap)
      Set.toList <$> Map.lookup uid um

lookupPubClientsBulk ::
  ( Member ClientStore r,
    Member (Input (Local ())) r,
    Member TinyLog r,
    Member (FederationAPIAccess m) r,
    RunClient (m 'Brig),
    FederationMonad m,
    Typeable m
  ) =>
  [Qualified UserId] -> Sem r (QualifiedUserMap (Set PubClient))
lookupPubClientsBulk qualifiedUids = do
  loc <- qualifyLocal ()
  let (localUsers, remoteUsers) = partitionQualified loc qualifiedUids
  remoteUserClientMap <- getRemoteClients $ indexQualified (fmap tUntagged remoteUsers)
  localUserClientMap <- Map.singleton (tDomain loc) <$> ClientStore.lookupPubClientsBulk localUsers
  pure $ QualifiedUserMap (Map.union localUserClientMap remoteUserClientMap)
  where
    getRemoteClients uids = do
      results <-
        traverse
          (\(d, ids) -> mapLeft (const d) . fmap (d,) <$> (getFederatedUserClients d (GetUserClients ids)))
          (Map.toList uids)
      forM_ (lefts results) $ \d ->
        Log.warn $
          field "remote_domain" (domainText d)
            ~~ msg (val "Failed to fetch clients for domain")
      pure $ Map.fromList (rights results)

lookupLocalPublicClientsBulk :: (Member ClientStore r) => [UserId] -> Sem r (UserMap (Set PubClient))
lookupLocalPublicClientsBulk = ClientStore.lookupPubClientsBulk

getFederatedUserClients ::
  ( Member TinyLog r,
    Member (FederationAPIAccess m) r,
    RunClient (m 'Brig),
    FederationMonad m,
    Typeable m
  ) =>
  Domain ->
  GetUserClients ->
  Sem r (Either FederationError (UserMap (Set PubClient)))
getFederatedUserClients domain guc = do
  Log.info $ msg @Text "Brig-federation: get users' clients from remote backend"
  runFederatedEither (toRemoteUnsafe domain ()) $ fedClient @'Brig @"get-user-clients" guc

onClientEvent ::
  (Member NotificationSubsystem r) =>
  -- | Originator of the event.
  UserId ->
  -- | Client connection ID.
  Maybe ConnId ->
  -- | The event.
  ClientEvent ->
  Sem r ()
onClientEvent orig conn e = do
  let event = ClientEvent e
  let rcpt = Recipient orig V2.RecipientClientsAll
  pushNotifications
    [ def
        { origin = Just orig,
          json = toJSONObject event,
          recipients = [rcpt],
          conn,
          apsData = toApsData event
        }
    ]

-- | Enqueue an orderly deletion of an existing client.
execDelete ::
  ( Member DeleteQueue r,
    Member AuthenticationSubsystem r,
    Member ClientStore r
  ) =>
  UserId ->
  Maybe ConnId ->
  Client ->
  Sem r ()
execDelete u con c = do
  for_ (clientCookie c) $ \l -> Authentication.revokeCookies u [] [l]
  DeleteQueue.enqueueClientDeletion c.clientId u con
  ClientStore.delete u c.clientId
