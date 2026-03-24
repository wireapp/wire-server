module Wire.ClientSubsystem.Interpreter (runClientSubsystem) where

import Control.Monad
import Data.Code as Code
import Data.Domain
import Data.Id
import Data.Json.Util (toUTCTimeMillis)
import Data.Map qualified as Map
import Data.Qualified
import Data.Set qualified as Set
import Data.Time.Clock
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
import Servant.Client.Core (RunClient)
import System.Logger.Message
import Wire.API.Federation.API
import Wire.API.Federation.API.Brig as FederatedBrig
import Wire.API.Federation.Error
import Wire.API.User as User
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.UserMap
import Wire.AuthenticationSubsystem (AuthenticationSubsystem)
import Wire.AuthenticationSubsystem qualified as Authentication
import Wire.AuthenticationSubsystem.Error
import Wire.ClientStore (ClientStore, DuplicateMLSPublicKey (..))
import Wire.ClientStore qualified as ClientStore
import Wire.ClientSubsystem (ClientSubsystem (..))
import Wire.Events as Events
import Wire.FederationAPIAccess (FederationAPIAccess, runFederatedEither)
import Wire.GalleyAPIAccess as GalleyAPIAccess
import Wire.NotificationSubsystem (NotificationSubsystem)
import Wire.Sem.Logger qualified as Log
import Wire.Sem.Now qualified as Now
import Wire.UserSubsystem qualified as User
import Wire.Util

data ClientSubsystemConfig = ClientSubsystemConfig
  { userMaxPermClients :: Int
  }

runClientSubsystem ::
  ( Member ClientStore r,
    Member (Input (Local ())) r,
    Member TinyLog r,
    Member (FederationAPIAccess m) r,
    RunClient (m 'Brig),
    FederationMonad m,
    Typeable m
  ) =>
  ClientSubsystemConfig ->
  InterpreterFor ClientSubsystem r
runClientSubsystem conf = interpret $ \case
  InternalGetActivityTimestamps uid -> internalGetActivityTimestamps uid
  LookupLocalClient uid cid -> lookupLocalClient uid cid
  LookupLocalClients uid -> lookupLocalClients uid
  LookupLocalPublicClientsBulk uids -> lookupLocalPublicClientsBulk uids
  LookupPublicClient quid cid -> lookupPubClient quid cid
  LookupPublicClients quid -> lookupPubClients quid
  LookupPublicClientsBulk quids -> lookupPubClientsBulk quids
  AddClient luid conn new -> addClient conf luid conn new

addClient ::
  -- ( Member GalleyAPIAccess r,
  --   Member NotificationSubsystem r,
  --   Member UserSubsystem r,
  --   Member DeleteQueue r,
  --   Member EmailSubsystem r,
  --   Member AuthenticationSubsystem r,
  --   Member VerificationCodeSubsystem r,
  --   Member Events r,
  --   Member ClientStore r
  -- ) =>
  ClientSubsystemConfig ->
  Local UserId ->
  Maybe ConnId ->
  NewClient ->
  Sem r Client
addClient = undefined -- addClientWithReAuthPolicy Data.reAuthForNewClients

-- nb. We must ensure that the set of clients known to brig is always
-- a superset of the clients known to galley.
addClientWithReAuthPolicy ::
  ClientSubsystemConfig ->
  ReAuthPolicy ->
  Local UserId ->
  Maybe ConnId ->
  NewClient ->
  Sem r Client
addClientWithReAuthPolicy conf policy luid@(tUnqualified -> u) con new = do
  usr <-
    (lift . liftSem $ User.getAccountNoFilter luid)
      >>= maybe (throwE (ClientUserNotFound u)) pure
  verifyCode (newClientVerificationCode new) luid
  let mCaps :: Maybe ClientCapabilityList
      mCaps = updlhdev $ newClientCapabilities new
        where
          updlhdev :: Maybe ClientCapabilityList -> Maybe ClientCapabilityList
          updlhdev =
            if newClientType new == LegalHoldClientType
              then Just . ClientCapabilityList . maybe (Set.singleton lhcaps) (Set.insert lhcaps . fromClientCapabilityList)
              else id
          lhcaps = ClientSupportsLegalholdImplicitConsent
  (clt0, old, count) <-
    (addClientWithReAuthPolicyX policy luid clientId' new conf.userMaxPermClients mCaps)
      !>> ClientDataError
  let clt = clt0 {clientMLSPublicKeys = newClientMLSPublicKeys new}
  consumableNotificationsEnabled <- asks (.settings.consumableNotifications)
  when (consumableNotificationsEnabled && supportsConsumableNotifications clt) $ lift $ liftSem $ do
    setupConsumableNotifications u clt.clientId
  for_ old $ execDelete u con
  GalleyAPIAccess.newClient u clt.clientId
  onClientEvent u con (ClientAdded clt)
  when (clientType clt == LegalHoldClientType) $ liftSem $ Events.generateUserEvent u con (UserLegalHoldEnabled u)
  when (count > 1) $
    for_ (userEmail usr) $
      \email ->
        liftSem $ sendNewClientEmail email (userDisplayName usr) clt (userLocale usr)
  pure clt
  where
    clientId' = clientIdFromPrekey (unpackLastPrekey $ newClientLastKey new)

    verifyCode ::
      Maybe Code.Value ->
      Local UserId ->
      Sem r ()
    verifyCode mbCode luid1 =
      -- this only happens inside the login flow (in particular, when logging in from a new device)
      -- the code obtained for logging in is used a second time for adding the device
      undefined "verifyCode" mbCode User.Login luid1 `catchE` \case
        VerificationCodeRequired -> undefined ClientCodeAuthenticationRequired
        VerificationCodeNoPendingCode -> undefined ClientCodeAuthenticationFailed
        VerificationCodeNoEmail -> undefined ClientCodeAuthenticationFailed

data ClientDataError
  = TooManyClients
  | ClientReAuthError !ReAuthError
  | ClientMissingAuth
  | MalformedPrekeys
  | MLSPublicKeyDuplicate
  | MLSNotEnabled
  | KeyPackageDecodingError
  | InvalidKeyPackageRef

-- | Re-authentication policy.
--
-- For a potential new client, a policy is a function that takes as arguments
-- the number of existing clients of the same type, and whether the client
-- already exists, and returns whether the user should be forced to
-- re-authenticate.
type ReAuthPolicy = Int -> Bool -> Bool

-- | Default re-authentication policy.
--
-- Re-authenticate if there is at least one other client.
reAuthForNewClients :: ReAuthPolicy
reAuthForNewClients count upsert = count > 0 && not upsert

addClientWithReAuthPolicy' ::
  forall r.
  ( Member AuthenticationSubsystem r,
    Member ClientStore r,
    Member Now.Now r
  ) =>
  ReAuthPolicy ->
  Local UserId ->
  ClientId ->
  NewClient ->
  Int ->
  Maybe ClientCapabilityList ->
  Sem r (Client, [Client], Word)
addClientWithReAuthPolicy' reAuthPolicy u newId c maxPermClients caps = do
  clients <- ClientStore.lookupClients (tUnqualified u)
  let typed = filter ((== newClientType c) . clientType) clients
  let count = length typed
  let upsert = any exists typed
  when (reAuthPolicy count upsert) do
    (Authentication.reauthenticateEither (tUnqualified u) (newClientPassword c))
      >>= either (undefined . ClientReAuthError) pure
  let capacity = fmap (+ (-count)) limit
  unless (maybe True (> 0) capacity || upsert) $
    undefined TooManyClients
  new <- insert (tUnqualified u)
  let !total = fromIntegral (length clients + if upsert then 0 else 1)
  let old = maybe (filter (not . exists) typed) (const []) limit
  pure (new, old, total)
  where
    limit :: Maybe Int
    limit = case newClientType c of
      PermanentClientType -> Just maxPermClients
      TemporaryClientType -> Nothing
      LegalHoldClientType -> Nothing

    exists :: Client -> Bool
    exists = (==) newId . (.clientId)

    insert uid = do
      now <- toUTCTimeMillis <$> Now.get
      let prekeys = unpackLastPrekey (newClientLastKey c) : newClientPrekeys c
      unless (all checkPrekeyBundle prekeys) $
        undefined MalformedPrekeys
      mErr <- ClientStore.upsert uid newId now (c {newClientCapabilities = caps})
      case mErr of
        Just DuplicateMLSPublicKey -> undefined MLSPublicKeyDuplicate
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
