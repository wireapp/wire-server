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

-- TODO: Move to Brig.User.Client
module Brig.API.Client
  ( -- * Clients
    addClient,
    addClientWithReAuthPolicy,
    updateClient,
    rmClient,
    pubClient,
    legalHoldClientRequested,
    removeLegalHoldClient,
    lookupLocalClient,
    lookupLocalClients,
    lookupPubClient,
    lookupPubClients,
    lookupPubClientsBulk,
    lookupLocalPubClientsBulk,
    Data.lookupPrekeyIds,
    Data.lookupUsersClientIds,
    createAccessToken,

    -- * Prekeys
    claimLocalMultiPrekeyBundles,
    claimLocalPrekeyBundle,
    claimPrekey,
    claimLocalPrekey,
    claimPrekeyBundle,
    claimMultiPrekeyBundles,
    claimMultiPrekeyBundlesV3,
    Data.lookupClientIds,
  )
where

import Brig.API.Types
import Brig.API.Util
import Brig.App
import qualified Brig.Data.Client as Data
import Brig.Data.Nonce as Nonce
import qualified Brig.Data.User as Data
import Brig.Effects.GalleyProvider (GalleyProvider)
import qualified Brig.Effects.GalleyProvider as GalleyProvider
import Brig.Effects.JwtTools (JwtTools)
import qualified Brig.Effects.JwtTools as JwtTools
import Brig.Effects.PublicKeyBundle (PublicKeyBundle)
import qualified Brig.Effects.PublicKeyBundle as PublicKeyBundle
import Brig.Federation.Client (getUserClients)
import qualified Brig.Federation.Client as Federation
import Brig.IO.Intra (guardLegalhold)
import qualified Brig.IO.Intra as Intra
import qualified Brig.InternalEvent.Types as Internal
import qualified Brig.Options as Opt
import qualified Brig.Queue as Queue
import Brig.Types.Intra
import Brig.Types.Team.LegalHold (LegalHoldClientRequest (..))
import Brig.Types.User.Event
import qualified Brig.User.Auth as UserAuth
import qualified Brig.User.Auth.Cookie as Auth
import Brig.User.Email
import Cassandra (MonadClient)
import Control.Error
import Control.Lens (view)
import Data.ByteString.Conversion
import Data.Code as Code
import Data.Domain (Domain)
import Data.IP (IP)
import Data.Id (ClientId, ConnId, UserId)
import Data.List.Split (chunksOf)
import Data.Map.Strict (traverseWithKey)
import qualified Data.Map.Strict as Map
import Data.Misc (PlainTextPassword (..))
import Data.Qualified
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import Imports
import Network.HTTP.Types.Method (StdMethod)
import Network.Wai.Utilities
import Polysemy (Member)
import Servant (Link, ToHttpApiData (toUrlPiece))
import System.Logger.Class (field, msg, val, (~~))
import qualified System.Logger.Class as Log
import Wire.API.Federation.API.Brig (GetUserClients (GetUserClients))
import Wire.API.Federation.Error
import Wire.API.MLS.Credential (ClientIdentity (..))
import Wire.API.MLS.Epoch (addToEpoch)
import qualified Wire.API.Message as Message
import Wire.API.Team.LegalHold (LegalholdProtectee (..))
import Wire.API.User
import qualified Wire.API.User as Code
import Wire.API.User.Client
import Wire.API.User.Client.DPoPAccessToken
import Wire.API.User.Client.Prekey
import Wire.API.UserMap (QualifiedUserMap (QualifiedUserMap, qualifiedUserMap), UserMap (userMap))
import Wire.Sem.Concurrency
import Wire.Sem.FromUTC (FromUTC (fromUTCTime))
import Wire.Sem.Now as Now

lookupLocalClient :: UserId -> ClientId -> (AppT r) (Maybe Client)
lookupLocalClient uid = wrapClient . Data.lookupClient uid

lookupLocalClients :: UserId -> (AppT r) [Client]
lookupLocalClients = wrapClient . Data.lookupClients

lookupPubClient :: Qualified UserId -> ClientId -> ExceptT ClientError (AppT r) (Maybe PubClient)
lookupPubClient qid cid = do
  clients <- lookupPubClients qid
  pure $ find ((== cid) . pubClientId) clients

lookupPubClients :: Qualified UserId -> ExceptT ClientError (AppT r) [PubClient]
lookupPubClients qid@(Qualified uid domain) = do
  getForUser <$> lookupPubClientsBulk [qid]
  where
    getForUser :: QualifiedUserMap (Set PubClient) -> [PubClient]
    getForUser qmap = fromMaybe [] $ do
      um <- userMap <$> Map.lookup domain (qualifiedUserMap qmap)
      Set.toList <$> Map.lookup uid um

lookupPubClientsBulk :: [Qualified UserId] -> ExceptT ClientError (AppT r) (QualifiedUserMap (Set PubClient))
lookupPubClientsBulk qualifiedUids = do
  loc <- qualifyLocal ()
  let (localUsers, remoteUsers) = partitionQualified loc qualifiedUids
  remoteUserClientMap <-
    traverseWithKey
      (\domain' uids -> getUserClients domain' (GetUserClients uids))
      (indexQualified (fmap tUntagged remoteUsers))
      !>> ClientFederationError
  localUserClientMap <- Map.singleton (tDomain loc) <$> lookupLocalPubClientsBulk localUsers
  pure $ QualifiedUserMap (Map.union localUserClientMap remoteUserClientMap)

lookupLocalPubClientsBulk :: [UserId] -> ExceptT ClientError (AppT r) (UserMap (Set PubClient))
lookupLocalPubClientsBulk = lift . wrapClient . Data.lookupPubClientsBulk

addClient ::
  (Member GalleyProvider r) =>
  UserId ->
  Maybe ConnId ->
  Maybe IP ->
  NewClient ->
  ExceptT ClientError (AppT r) Client
addClient = addClientWithReAuthPolicy Data.reAuthForNewClients

-- nb. We must ensure that the set of clients known to brig is always
-- a superset of the clients known to galley.
addClientWithReAuthPolicy ::
  forall r.
  (Member GalleyProvider r) =>
  Data.ReAuthPolicy ->
  UserId ->
  Maybe ConnId ->
  Maybe IP ->
  NewClient ->
  ExceptT ClientError (AppT r) Client
addClientWithReAuthPolicy policy u con ip new = do
  acc <- lift (wrapClient $ Data.lookupAccount u) >>= maybe (throwE (ClientUserNotFound u)) pure
  verifyCode (newClientVerificationCode new) (userId . accountUser $ acc)
  loc <- maybe (pure Nothing) locationOf ip
  maxPermClients <- fromMaybe Opt.defUserMaxPermClients . Opt.setUserMaxPermClients <$> view settings
  let caps :: Maybe (Set ClientCapability)
      caps = updlhdev $ newClientCapabilities new
        where
          updlhdev =
            if newClientType new == LegalHoldClientType
              then Just . maybe (Set.singleton lhcaps) (Set.insert lhcaps)
              else id
          lhcaps = ClientSupportsLegalholdImplicitConsent
  (clt0, old, count) <-
    wrapClientE
      (Data.addClientWithReAuthPolicy policy u clientId' new maxPermClients loc caps)
      !>> ClientDataError
  let clt = clt0 {clientMLSPublicKeys = newClientMLSPublicKeys new}
  let usr = accountUser acc
  lift $ do
    for_ old $ execDelete u con
    liftSem $ GalleyProvider.newClient u (clientId clt)
    wrapHttp $ Intra.onClientEvent u con (ClientAdded u clt)
    when (clientType clt == LegalHoldClientType) $ wrapHttpClient $ Intra.onUserEvent u con (UserLegalHoldEnabled u)
    when (count > 1) $
      for_ (userEmail usr) $
        \email ->
          sendNewClientEmail (userDisplayName usr) email clt (userLocale usr)
  pure clt
  where
    clientId' = clientIdFromPrekey (unpackLastPrekey $ newClientLastKey new)

    verifyCode ::
      Maybe Code.Value ->
      UserId ->
      ExceptT ClientError (AppT r) ()
    verifyCode mbCode userId =
      -- this only happens inside the login flow (in particular, when logging in from a new device)
      -- the code obtained for logging in is used a second time for adding the device
      UserAuth.verifyCode mbCode Code.Login userId `catchE` \case
        VerificationCodeRequired -> throwE ClientCodeAuthenticationRequired
        VerificationCodeNoPendingCode -> throwE ClientCodeAuthenticationFailed
        VerificationCodeNoEmail -> throwE ClientCodeAuthenticationFailed

updateClient :: MonadClient m => UserId -> ClientId -> UpdateClient -> ExceptT ClientError m ()
updateClient u c r = do
  client <- lift (Data.lookupClient u c) >>= maybe (throwE ClientNotFound) pure
  for_ (updateClientLabel r) $ lift . Data.updateClientLabel u c . Just
  for_ (updateClientCapabilities r) $ \caps' -> do
    let ClientCapabilityList caps = clientCapabilities client
    if caps `Set.isSubsetOf` caps'
      then lift . Data.updateClientCapabilities u c . Just $ caps'
      else throwE ClientCapabilitiesCannotBeRemoved
  let lk = maybeToList (unpackLastPrekey <$> updateClientLastKey r)
  Data.updatePrekeys u c (lk ++ updateClientPrekeys r) !>> ClientDataError
  Data.addMLSPublicKeys u c (Map.assocs (updateClientMLSPublicKeys r)) !>> ClientDataError

-- nb. We must ensure that the set of clients known to brig is always
-- a superset of the clients known to galley.
rmClient :: UserId -> ConnId -> ClientId -> Maybe PlainTextPassword -> ExceptT ClientError (AppT r) ()
rmClient u con clt pw =
  maybe (throwE ClientNotFound) fn =<< lift (wrapClient $ Data.lookupClient u clt)
  where
    fn client = do
      case clientType client of
        -- Legal hold clients can't be removed
        LegalHoldClientType -> throwE ClientLegalHoldCannotBeRemoved
        -- Temporary clients don't need to re-auth
        TemporaryClientType -> pure ()
        -- All other clients must authenticate
        _ -> wrapClientE (Data.reauthenticate u pw) !>> ClientDataError . ClientReAuthError
      lift $ execDelete u (Just con) client

claimPrekey ::
  LegalholdProtectee ->
  UserId ->
  Domain ->
  ClientId ->
  ExceptT ClientError (AppT r) (Maybe ClientPrekey)
claimPrekey protectee u d c = do
  isLocalDomain <- (d ==) <$> viewFederationDomain
  if isLocalDomain
    then claimLocalPrekey protectee u c
    else wrapClientE $ claimRemotePrekey (Qualified u d) c

claimLocalPrekey ::
  LegalholdProtectee ->
  UserId ->
  ClientId ->
  ExceptT ClientError (AppT r) (Maybe ClientPrekey)
claimLocalPrekey protectee user client = do
  guardLegalhold protectee (mkUserClients [(user, [client])])
  lift $ do
    prekey <- wrapHttpClient $ Data.claimPrekey user client
    when (isNothing prekey) (noPrekeys user client)
    pure prekey

claimRemotePrekey ::
  ( MonadReader Env m,
    Log.MonadLogger m,
    MonadClient m
  ) =>
  Qualified UserId ->
  ClientId ->
  ExceptT ClientError m (Maybe ClientPrekey)
claimRemotePrekey quser client = fmapLT ClientFederationError $ Federation.claimPrekey quser client

claimPrekeyBundle :: LegalholdProtectee -> Domain -> UserId -> ExceptT ClientError (AppT r) PrekeyBundle
claimPrekeyBundle protectee domain uid = do
  isLocalDomain <- (domain ==) <$> viewFederationDomain
  if isLocalDomain
    then claimLocalPrekeyBundle protectee uid
    else claimRemotePrekeyBundle (Qualified uid domain)

claimLocalPrekeyBundle :: LegalholdProtectee -> UserId -> ExceptT ClientError (AppT r) PrekeyBundle
claimLocalPrekeyBundle protectee u = do
  clients <- map clientId <$> lift (wrapClient (Data.lookupClients u))
  guardLegalhold protectee (mkUserClients [(u, clients)])
  PrekeyBundle u . catMaybes <$> lift (mapM (wrapHttp . Data.claimPrekey u) clients)

claimRemotePrekeyBundle :: Qualified UserId -> ExceptT ClientError (AppT r) PrekeyBundle
claimRemotePrekeyBundle quser = do
  Federation.claimPrekeyBundle quser !>> ClientFederationError

claimMultiPrekeyBundles ::
  forall r.
  (Member (Concurrency 'Unsafe) r) =>
  LegalholdProtectee ->
  QualifiedUserClients ->
  ExceptT ClientError (AppT r) QualifiedUserClientPrekeyMap
claimMultiPrekeyBundles protectee quc = do
  loc <- qualifyLocal ()
  let (locals, remotes) =
        partitionQualifiedAndTag
          loc
          ( map
              (fmap UserClients . uncurry (flip Qualified))
              (Map.assocs (qualifiedUserClients quc))
          )
  localPrekeys <- traverse claimLocal locals
  remotePrekeys <-
    mapExceptT wrapHttpClient $
      traverseConcurrentlyWithErrors
        claimRemote
        remotes
        !>> ClientFederationError
  pure . qualifiedUserClientPrekeyMapFromList $ localPrekeys <> remotePrekeys
  where
    claimRemote ::
      ( Log.MonadLogger m,
        MonadIO m,
        MonadReader Env m
      ) =>
      Remote UserClients ->
      ExceptT FederationError m (Qualified UserClientPrekeyMap)
    claimRemote ruc =
      tUntagged . qualifyAs ruc
        <$> Federation.claimMultiPrekeyBundle (tDomain ruc) (tUnqualified ruc)

    claimLocal :: Local UserClients -> ExceptT ClientError (AppT r) (Qualified UserClientPrekeyMap)
    claimLocal luc =
      tUntagged . qualifyAs luc
        <$> claimLocalMultiPrekeyBundles protectee (tUnqualified luc)

-- Similar to claimMultiPrekeyBundles except for the following changes
-- 1) A new return type that contains both the client map and a list of
--    users that prekeys couldn't be fetched for.
-- 2) A semantic change on federation errors when gathering remote clients.
--    Remote federation errors at this step no-longer cause the entire call
--    to fail, allowing partial results to be returned.
claimMultiPrekeyBundlesV3 ::
  forall r.
  Member (Concurrency 'Unsafe) r =>
  LegalholdProtectee ->
  QualifiedUserClients ->
  ExceptT ClientError (AppT r) QualifiedUserClientPrekeyMapV4
claimMultiPrekeyBundlesV3 protectee quc = do
  loc <- qualifyLocal ()
  let (locals, remotes) =
        partitionQualifiedAndTag
          loc
          ( map
              (fmap UserClients . uncurry (flip Qualified))
              (Map.assocs (qualifiedUserClients quc))
          )
  localPrekeys <- traverse claimLocal locals
  remotePrekeys <- mapExceptT wrapHttpClient $ lift $ traverseConcurrently claimRemote remotes
  let prekeys =
        getQualifiedUserClientPrekeyMap $
          qualifiedUserClientPrekeyMapFromList $
            localPrekeys <> rights remotePrekeys
      failed = lefts remotePrekeys >>= toQualifiedUser . fst
  pure $
    QualifiedUserClientPrekeyMapV4 prekeys $
      if null failed
        then Nothing
        else pure failed
  where
    toQualifiedUser :: Remote UserClients -> [Qualified UserId]
    toQualifiedUser r = fmap (\u -> Qualified u $ tDomain r) . Map.keys . userClients . qUnqualified $ tUntagged r
    claimRemote :: Remote UserClients -> ExceptT FederationError HttpClientIO (Qualified UserClientPrekeyMap)
    claimRemote ruc =
      tUntagged . qualifyAs ruc
        <$> Federation.claimMultiPrekeyBundle (tDomain ruc) (tUnqualified ruc)

    claimLocal :: Local UserClients -> ExceptT ClientError (AppT r) (Qualified UserClientPrekeyMap)
    claimLocal luc =
      tUntagged . qualifyAs luc
        <$> claimLocalMultiPrekeyBundles protectee (tUnqualified luc)

claimLocalMultiPrekeyBundles ::
  forall r.
  Member (Concurrency 'Unsafe) r =>
  LegalholdProtectee ->
  UserClients ->
  ExceptT ClientError (AppT r) UserClientPrekeyMap
claimLocalMultiPrekeyBundles protectee userClients = do
  guardLegalhold protectee userClients
  lift
    . fmap mkUserClientPrekeyMap
    . foldMap (getChunk . Map.fromList)
    . chunksOf 16
    . Map.toList
    . Message.userClients
    $ userClients
  where
    getChunk :: Map UserId (Set ClientId) -> AppT r (Map UserId (Map ClientId (Maybe Prekey)))
    getChunk m = do
      e <- ask
      AppT $
        lift $
          Map.fromListWith (<>)
            <$> unsafePooledMapConcurrentlyN
              16
              (\(u, cids) -> (u,) <$> lowerAppT e (getUserKeys u cids))
              (Map.toList m)
    getUserKeys ::
      UserId ->
      Set ClientId ->
      (AppT r) (Map ClientId (Maybe Prekey))
    getUserKeys u =
      sequenceA . Map.fromSet (getClientKeys u)
    getClientKeys ::
      UserId ->
      ClientId ->
      (AppT r) (Maybe Prekey)
    getClientKeys u c = do
      key <- fmap prekeyData <$> wrapHttpClient (Data.claimPrekey u c)
      when (isNothing key) $ noPrekeys u c
      pure key

-- Utilities

-- | Enqueue an orderly deletion of an existing client.
execDelete :: UserId -> Maybe ConnId -> Client -> (AppT r) ()
execDelete u con c = do
  for_ (clientCookie c) $ \l -> wrapClient $ Auth.revokeCookies u [] [l]
  queue <- view internalEvents
  Queue.enqueue queue (Internal.DeleteClient (clientId c) u con)
  wrapClient $ Data.rmClient u (clientId c)

-- | Defensive measure when no prekey is found for a
-- requested client: Ensure that the client does indeed
-- not exist, since there must be no client without prekeys,
-- thus repairing any inconsistencies related to distributed
-- (and possibly duplicated) client data.
noPrekeys ::
  UserId ->
  ClientId ->
  (AppT r) ()
noPrekeys u c = do
  mclient <- wrapClient $ Data.lookupClient u c
  case mclient of
    Nothing -> do
      Log.warn $
        field "user" (toByteString u)
          ~~ field "client" (toByteString c)
          ~~ msg (val "No prekey found. Client is missing, so doing nothing.")
    Just client -> do
      Log.warn $
        field "user" (toByteString u)
          ~~ field "client" (toByteString c)
          ~~ msg (val "No prekey found. Deleting client.")
      execDelete u Nothing client

pubClient :: Client -> PubClient
pubClient c =
  PubClient
    { pubClientId = clientId c,
      pubClientClass = clientClass c
    }

legalHoldClientRequested :: UserId -> LegalHoldClientRequest -> (AppT r) ()
legalHoldClientRequested targetUser (LegalHoldClientRequest _requester lastPrekey') =
  wrapHttpClient $ Intra.onUserEvent targetUser Nothing lhClientEvent
  where
    clientId :: ClientId
    clientId = clientIdFromPrekey $ unpackLastPrekey lastPrekey'
    eventData :: LegalHoldClientRequestedData
    eventData = LegalHoldClientRequestedData targetUser lastPrekey' clientId
    lhClientEvent :: UserEvent
    lhClientEvent = LegalHoldClientRequested eventData

removeLegalHoldClient :: UserId -> (AppT r) ()
removeLegalHoldClient uid = do
  clients <- wrapClient $ Data.lookupClients uid
  -- Should only be one; but just in case we'll treat it as a list
  let legalHoldClients = filter ((== LegalHoldClientType) . clientType) clients
  -- maybe log if this isn't the case
  forM_ legalHoldClients (execDelete uid Nothing)
  wrapHttpClient $ Intra.onUserEvent uid Nothing (UserLegalHoldDisabled uid)

createAccessToken ::
  (Member JwtTools r, Member Now r, Member PublicKeyBundle r) =>
  UserId ->
  ClientId ->
  StdMethod ->
  Link ->
  Proof ->
  ExceptT CertEnrollmentError (AppT r) (DPoPAccessTokenResponse, CacheControl)
createAccessToken uid cid method link proof = do
  domain <- Opt.setFederationDomain <$> view settings
  nonce <- ExceptT $ note NonceNotFound <$> wrapClient (Nonce.lookupAndDeleteNonce uid (cs $ toByteString cid))
  httpsUrl <- do
    let urlBs = "https://" <> toByteString' domain <> "/" <> cs (toUrlPiece link)
    maybe (throwE MisconfiguredRequestUrl) pure $ fromByteString $ urlBs
  maxSkewSeconds <- Opt.setDpopMaxSkewSecs <$> view settings
  expiresIn <- Opt.setDpopTokenExpirationTimeSecs <$> view settings
  now <- fromUTCTime <$> lift (liftSem Now.get)
  let expiresAt = now & addToEpoch expiresIn
  pathToKeys <- ExceptT $ note KeyBundleError . Opt.setPublicKeyBundle <$> view settings
  pubKeyBundle <- ExceptT $ note KeyBundleError <$> liftSem (PublicKeyBundle.get pathToKeys)
  token <-
    ExceptT $
      liftSem $
        JwtTools.generateDPoPAccessToken
          proof
          (ClientIdentity domain uid cid)
          nonce
          httpsUrl
          method
          maxSkewSeconds
          expiresAt
          now
          pubKeyBundle
  pure $ (DPoPAccessTokenResponse token DPoP expiresIn, NoStore)
