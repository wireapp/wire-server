{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Wire.ClientSubsystem.Interpreter
  ( runClientSubsystem,
    ClientError (..),
    ClientDataError (..),
    ClientSubsystemConfig (..),
  )
where

import Control.Monad
import Data.ByteString.Conversion
import Data.Default
import Data.Domain
import Data.Id
import Data.Json.Util (ToJSONObject (..), toUTCTimeMillis)
import Data.Map qualified as Map
import Data.Misc
import Data.Qualified
import Data.Set ((\\))
import Data.Set qualified as Set
import Data.Time.Clock
import Imports hiding ((\\))
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
import Wire.API.Team.LegalHold
import Wire.API.Team.LegalHold.Internal
import Wire.API.User as User
import Wire.API.User.Client hiding (UpdateClient)
import Wire.API.User.Client qualified as Data
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
import Wire.FederationAPIAccess (FederationAPIAccess, runFederated, runFederatedEither)
import Wire.GalleyAPIAccess as GalleyAPIAccess
import Wire.NotificationSubsystem
import Wire.Sem.Logger qualified as Log
import Wire.Sem.Now qualified as Now
import Wire.UserSubsystem (UserSubsystem)
import Wire.UserSubsystem qualified as UserSubsystem
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
    Member (Input ClientSubsystemConfig) r,
    Member (Error FederationError) r
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
      RemoveClient uid conn cid mPwd -> rmClient uid conn cid mPwd
      RemoveLegalHoldClient uid -> removeLegalHoldClient uid
      PublishLegalHoldClientRequested uid req -> publishLegalHoldClientRequested uid req
      UpdateClient uid cid payload -> updateClient uid cid payload
      ClaimPrekey protectee uid domain cid -> claimPrekey protectee uid domain cid
      ClaimLocalPrekey protectee uid cid -> claimLocalPrekey protectee uid cid

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
  usr <- UserSubsystem.getAccountNoFilter luid >>= maybe (throw (ClientUserNotFound uid)) pure
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

-- nb. We must ensure that the set of clients known to brig is always
-- a superset of the clients known to galley.
rmClient ::
  ( Member AuthenticationSubsystem r,
    Member ClientStore r,
    Member (Error ClientError) r,
    Member DeleteQueue r
  ) =>
  UserId ->
  ConnId ->
  ClientId ->
  Maybe PlainTextPassword6 ->
  Sem r ()
rmClient u con clt pw =
  maybe (throw ClientNotFound) fn =<< ClientStore.lookupClient u clt
  where
    fn client = do
      case clientType client of
        -- Legal hold clients can't be removed
        LegalHoldClientType -> throw ClientLegalHoldCannotBeRemoved
        -- Temporary clients don't need to re-auth
        TemporaryClientType -> pure ()
        -- All other clients must authenticate
        _ ->
          (Authentication.reauthenticateEither u pw)
            >>= either (throw . ClientDataError . ClientReAuthError) (const $ pure ())
      execDelete u (Just con) client

removeLegalHoldClient ::
  ( Member Events r,
    Member ClientStore r,
    Member DeleteQueue r,
    Member AuthenticationSubsystem r
  ) =>
  UserId ->
  Sem r ()
removeLegalHoldClient uid = do
  clients <- ClientStore.lookupClients uid
  -- Should only be one; but just in case we'll treat it as a list
  let legalHoldClients = filter ((== LegalHoldClientType) . clientType) clients
  -- maybe log if this isn't the case
  forM_ legalHoldClients (execDelete uid Nothing)
  Events.generateUserEvent uid Nothing (UserLegalHoldDisabled uid)

publishLegalHoldClientRequested :: (Member Events r) => UserId -> LegalHoldClientRequest -> Sem r ()
publishLegalHoldClientRequested targetUser (LegalHoldClientRequest _requester lastPrekey') =
  Events.generateUserEvent targetUser Nothing lhClientEvent
  where
    clientId :: ClientId
    clientId = clientIdFromPrekey $ unpackLastPrekey lastPrekey'

    eventData :: LegalHoldClientRequestedData
    eventData = LegalHoldClientRequestedData targetUser lastPrekey' clientId

    lhClientEvent :: UserEvent
    lhClientEvent = LegalHoldClientRequested eventData

updateClient ::
  ( Member NotificationSubsystem r,
    Member ClientStore r,
    Member (Error ClientError) r,
    Member (Input ClientSubsystemConfig) r
  ) =>
  UserId ->
  ClientId ->
  Data.UpdateClient ->
  Sem r ()
updateClient uid cid req = do
  conf <- input
  client <- ClientStore.lookupClient uid cid >>= maybe (throw ClientNotFound) pure
  for_ req.updateClientLabel $ ClientStore.updateLabel uid cid . Just
  for_ req.updateClientCapabilities $ \caps -> do
    if client.clientCapabilities.fromClientCapabilityList `Set.isSubsetOf` caps.fromClientCapabilityList
      then do
        -- first set up the notification queues then save the data is more robust than the other way around
        let addedCapabilities = caps.fromClientCapabilityList \\ client.clientCapabilities.fromClientCapabilityList
        when (conf.consumableNotificationsEnabled && ClientSupportsConsumableNotifications `Set.member` addedCapabilities) $ do
          setupConsumableNotifications uid cid
        ClientStore.updateCapabilities uid cid . Just $ caps
      else throw ClientCapabilitiesCannotBeRemoved
  let lk = maybeToList (unpackLastPrekey <$> req.updateClientLastKey)
      prekeys = lk ++ req.updateClientPrekeys
  unless (all checkPrekeyBundle prekeys) $ throw (ClientDataError MalformedPrekeys)
  ClientStore.updatePrekeys uid cid prekeys
  mErr <- ClientStore.addMLSPublicKeys uid cid (Map.assocs req.updateClientMLSPublicKeys)
  case mErr of
    Just DuplicateMLSPublicKey -> throw (ClientDataError MLSPublicKeyDuplicate)
    Nothing -> pure ()

---------------------------------------------------------------------------------------
-- Prekeys

claimPrekey ::
  ( Member (Input (Local ())) r,
    Member TinyLog r,
    Member ClientStore r,
    Member DeleteQueue r,
    Member AuthenticationSubsystem r,
    Member GalleyAPIAccess r,
    Member (FederationAPIAccess m) r,
    RunClient (m 'Brig),
    FederationMonad m,
    Typeable m,
    Member (Error FederationError) r
  ) =>
  LegalholdProtectee ->
  UserId ->
  Domain ->
  ClientId ->
  Sem r (Maybe ClientPrekey)
claimPrekey protectee uid domain cid = do
  isDomainLocal <- isLocalDomain domain
  if isDomainLocal
    then claimLocalPrekey protectee uid cid
    else claimRemotePrekey (Qualified uid domain) cid

claimLocalPrekey ::
  ( Member ClientStore r,
    Member TinyLog r,
    Member DeleteQueue r,
    Member AuthenticationSubsystem r,
    Member GalleyAPIAccess r
  ) =>
  LegalholdProtectee ->
  UserId ->
  ClientId ->
  Sem r (Maybe ClientPrekey)
claimLocalPrekey protectee user client = do
  GalleyAPIAccess.guardLegalHold protectee (mkUserClients [(user, [client])])
  prekey <- ClientStore.claimPrekey user client
  when (isNothing prekey) (noPrekeys user client)
  pure prekey

claimRemotePrekey ::
  ( Member TinyLog r,
    Member (FederationAPIAccess m) r,
    RunClient (m 'Brig),
    FederationMonad m,
    Typeable m,
    Member (Error FederationError) r
  ) =>
  Qualified UserId ->
  ClientId ->
  Sem r (Maybe ClientPrekey)
claimRemotePrekey (Qualified user domain) client = do
  Log.info $ msg @Text "Brig-federation: claiming remote prekey"
  runFederated (toRemoteUnsafe domain ()) $ fedClient @'Brig @"claim-prekey" (user, client)

-- Utilities

-- | Defensive measure when no prekey is found for a
-- requested client: Ensure that the client does indeed
-- not exist, since there must be no client without prekeys,
-- thus repairing any inconsistencies related to distributed
-- (and possibly duplicated) client data.
noPrekeys ::
  ( Member ClientStore r,
    Member TinyLog r,
    Member DeleteQueue r,
    Member AuthenticationSubsystem r
  ) =>
  UserId ->
  ClientId ->
  Sem r ()
noPrekeys uid cid = do
  mclient <- ClientStore.lookupClient uid cid
  case mclient of
    Nothing -> do
      Log.warn $
        field "user" (toByteString uid)
          ~~ field "client" (toByteString cid)
          ~~ msg (val "No prekey found. Client is missing, so doing nothing.")
    Just client -> do
      Log.warn $
        field "user" (toByteString uid)
          ~~ field "client" (toByteString cid)
          ~~ msg (val "No prekey found. Deleting client.")
      execDelete uid Nothing client
