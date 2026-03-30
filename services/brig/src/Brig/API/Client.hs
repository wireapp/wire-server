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
    updateClient,
    createAccessToken,

    -- * Prekeys
    claimLocalMultiPrekeyBundles,
    claimLocalPrekeyBundle,
    claimPrekey,
    claimLocalPrekey,
    claimPrekeyBundle,
    claimMultiPrekeyBundles,
    claimMultiPrekeyBundlesV3,
  )
where

import Brig.API.Handler (Handler)
import Brig.API.Types
import Brig.API.Util
import Brig.App
import Brig.Data.Nonce as Nonce
import Brig.Effects.JwtTools (JwtTools)
import Brig.Effects.JwtTools qualified as JwtTools
import Brig.Effects.PublicKeyBundle (PublicKeyBundle)
import Brig.Effects.PublicKeyBundle qualified as PublicKeyBundle
import Brig.Federation.Client qualified as Federation
import Brig.IO.Intra (guardLegalhold)
import Brig.Options qualified as Opt
import Cassandra (MonadClient)
import Control.Error
import Control.Monad.Trans.Except (except)
import Data.ByteString (toStrict)
import Data.ByteString.Conversion
import Data.Domain
import Data.HavePendingInvitations
import Data.Id (ClientId, UserId)
import Data.List.Split (chunksOf)
import Data.Map.Strict qualified as Map hiding ((\\))
import Data.Qualified
import Data.Set ((\\))
import Data.Set qualified as Set
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error
import Imports hiding ((\\))
import Network.HTTP.Types.Method (StdMethod)
import Network.Wai.Utilities
import Polysemy
import Servant (Link, ToHttpApiData (toUrlPiece))
import System.Logger.Class (field, msg, val, (~~))
import System.Logger.Class qualified as Log
import Wire.API.Federation.Error
import Wire.API.MLS.Credential (ClientIdentity (..))
import Wire.API.MLS.Epoch (addToEpoch)
import Wire.API.Message qualified as Message
import Wire.API.Routes.Internal.Brig
import Wire.API.Team.LegalHold (LegalholdProtectee (..))
import Wire.API.User
import Wire.API.User.Client
import Wire.API.User.Client.DPoPAccessToken
import Wire.API.User.Client.Prekey
import Wire.ClientStore (ClientStore, DuplicateMLSPublicKey (..))
import Wire.ClientStore qualified as ClientStore
import Wire.ClientSubsystem
import Wire.ClientSubsystem.Error
import Wire.NotificationSubsystem
import Wire.Sem.Concurrency
import Wire.Sem.FromUTC (FromUTC (fromUTCTime))
import Wire.Sem.Now as Now
import Wire.UserSubsystem (UserSubsystem)
import Wire.UserSubsystem qualified as User

updateClient ::
  (Member NotificationSubsystem r, Member ClientStore r) =>
  UserId ->
  ClientId ->
  UpdateClient ->
  (Handler r) ()
updateClient uid cid req = do
  client <- (lift (liftSem (ClientStore.lookupClient uid cid)) >>= maybe (throwE ClientNotFound) pure) !>> clientErrorToHttpError
  consumableNotificationsEnabled <- asks (.settings.consumableNotifications)
  lift . liftSem $ for_ req.updateClientLabel $ ClientStore.updateLabel uid cid . Just
  for_ req.updateClientCapabilities $ \caps -> do
    if client.clientCapabilities.fromClientCapabilityList `Set.isSubsetOf` caps.fromClientCapabilityList
      then do
        -- first set up the notification queues then save the data is more robust than the other way around
        let addedCapabilities = caps.fromClientCapabilityList \\ client.clientCapabilities.fromClientCapabilityList
        when (consumableNotificationsEnabled && ClientSupportsConsumableNotifications `Set.member` addedCapabilities) $ lift $ liftSem $ do
          setupConsumableNotifications uid cid
        lift . liftSem . ClientStore.updateCapabilities uid cid . Just $ caps
      else throwE $ clientErrorToHttpError ClientCapabilitiesCannotBeRemoved
  let lk = maybeToList (unpackLastPrekey <$> req.updateClientLastKey)
      prekeys = lk ++ req.updateClientPrekeys
  ( do
      unless (all checkPrekeyBundle prekeys) $
        throwE MalformedPrekeys
      lift . liftSem $ ClientStore.updatePrekeys uid cid prekeys
      mErr <- lift . liftSem $ ClientStore.addMLSPublicKeys uid cid (Map.assocs req.updateClientMLSPublicKeys)
      case mErr of
        Just DuplicateMLSPublicKey -> throwE MLSPublicKeyDuplicate
        Nothing -> pure ()
    )
    !>> ClientDataError
    !>> clientErrorToHttpError

claimPrekey ::
  ( Member ClientSubsystem r,
    Member ClientStore r
  ) =>
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
  ( Member ClientSubsystem r,
    Member ClientStore r
  ) =>
  LegalholdProtectee ->
  UserId ->
  ClientId ->
  ExceptT ClientError (AppT r) (Maybe ClientPrekey)
claimLocalPrekey protectee user client = do
  guardLegalhold protectee (mkUserClients [(user, [client])])
  lift $ do
    prekey <- liftSem $ ClientStore.claimPrekey user client
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

claimPrekeyBundle :: (Member ClientStore r) => LegalholdProtectee -> Domain -> UserId -> ExceptT ClientError (AppT r) PrekeyBundle
claimPrekeyBundle protectee domain uid = do
  isLocalDomain <- (domain ==) <$> viewFederationDomain
  if isLocalDomain
    then claimLocalPrekeyBundle protectee uid
    else claimRemotePrekeyBundle (Qualified uid domain)

claimLocalPrekeyBundle :: (Member ClientStore r) => LegalholdProtectee -> UserId -> ExceptT ClientError (AppT r) PrekeyBundle
claimLocalPrekeyBundle protectee u = do
  clients <- map (.clientId) <$> lift (liftSem (ClientStore.lookupClients u))
  guardLegalhold protectee (mkUserClients [(u, clients)])
  PrekeyBundle u . catMaybes <$> lift (mapM (liftSem . ClientStore.claimPrekey u) clients)

claimRemotePrekeyBundle :: Qualified UserId -> ExceptT ClientError (AppT r) PrekeyBundle
claimRemotePrekeyBundle quser = do
  Federation.claimPrekeyBundle quser !>> ClientFederationError

claimMultiPrekeyBundlesInternal ::
  forall r.
  ( Member (Concurrency 'Unsafe) r,
    Member ClientSubsystem r,
    Member ClientStore r
  ) =>
  LegalholdProtectee ->
  QualifiedUserClients ->
  ExceptT
    ClientError
    (AppT r)
    ([Qualified UserClientPrekeyMap], [Remote UserClients])
claimMultiPrekeyBundlesInternal protectee quc = do
  loc <- qualifyLocal ()
  let (locals, remotes) =
        partitionQualifiedAndTag
          loc
          ( map
              (fmap UserClients . uncurry (flip Qualified))
              (Map.assocs (qualifiedUserClients quc))
          )
  localPrekeys <- traverse claimLocal locals
  pure (localPrekeys, remotes)
  where
    claimLocal ::
      Local UserClients ->
      ExceptT ClientError (AppT r) (Qualified UserClientPrekeyMap)
    claimLocal luc =
      tUntagged . qualifyAs luc
        <$> claimLocalMultiPrekeyBundles protectee (tUnqualified luc)

claimMultiPrekeyBundlesV3 ::
  ( Member (Concurrency 'Unsafe) r,
    Member ClientSubsystem r,
    Member ClientStore r
  ) =>
  LegalholdProtectee ->
  QualifiedUserClients ->
  ExceptT ClientError (AppT r) QualifiedUserClientPrekeyMap
claimMultiPrekeyBundlesV3 protectee quc = do
  (localPrekeys, remotes) <- claimMultiPrekeyBundlesInternal protectee quc
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

-- Similar to claimMultiPrekeyBundles except for the following changes
-- 1) A new return type that contains both the client map and a list of
--    users that prekeys couldn't be fetched for.
-- 2) A semantic change on federation errors when gathering remote clients.
--    Remote federation errors at this step no-longer cause the entire call
--    to fail, allowing partial results to be returned.
claimMultiPrekeyBundles ::
  forall r.
  ( Member (Concurrency 'Unsafe) r,
    Member ClientSubsystem r,
    Member ClientStore r
  ) =>
  LegalholdProtectee ->
  QualifiedUserClients ->
  ExceptT ClientError (AppT r) QualifiedUserClientPrekeyMapV4
claimMultiPrekeyBundles protectee quc = do
  (localPrekeys, remotes) <- claimMultiPrekeyBundlesInternal protectee quc
  remotePrekeys <- mapExceptT wrapHttpClient $ lift $ traverseConcurrentlySem claimRemote remotes
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

claimLocalMultiPrekeyBundles ::
  forall r.
  ( Member (Concurrency 'Unsafe) r,
    Member ClientSubsystem r,
    Member ClientStore r
  ) =>
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
    getChunk :: Map UserId (Set ClientId) -> AppT r (Map UserId (Map ClientId (Maybe UncheckedPrekeyBundle)))
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
      (AppT r) (Map ClientId (Maybe UncheckedPrekeyBundle))
    getUserKeys u =
      sequenceA . Map.fromSet (getClientKeys u)
    getClientKeys ::
      UserId ->
      ClientId ->
      (AppT r) (Maybe UncheckedPrekeyBundle)
    getClientKeys u c = do
      key <- fmap prekeyData <$> liftSem (ClientStore.claimPrekey u c)
      when (isNothing key) $ noPrekeys u c
      pure key

-- Utilities

-- | Defensive measure when no prekey is found for a
-- requested client: Ensure that the client does indeed
-- not exist, since there must be no client without prekeys,
-- thus repairing any inconsistencies related to distributed
-- (and possibly duplicated) client data.
noPrekeys ::
  ( Member ClientSubsystem r,
    Member ClientStore r
  ) =>
  UserId ->
  ClientId ->
  (AppT r) ()
noPrekeys u c = do
  mclient <- liftSem $ ClientStore.lookupClient u c
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
      liftSem $ enqueueClientDeletion u Nothing client

createAccessToken ::
  (Member JwtTools r, Member Now r, Member PublicKeyBundle r, Member UserSubsystem r) =>
  Local UserId ->
  ClientId ->
  StdMethod ->
  Link ->
  Proof ->
  ExceptT CertEnrollmentError (AppT r) (DPoPAccessTokenResponse, CacheControl)
createAccessToken luid cid method link proof = do
  let domain = tDomain luid
  let uid = tUnqualified luid
  (tid, handle, displayName) <- do
    mUser <-
      fmap listToMaybe
        . lift
        . liftSem
        . User.getAccountsBy
        . qualifyAs luid
        $ getByNoFilters {getByUserId = [tUnqualified luid], includePendingInvitations = NoPendingInvitations}
    except $
      (,,)
        <$> note NotATeamUser (userTeam =<< mUser)
        <*> note MissingHandle (userHandle =<< mUser)
        <*> note MissingName (userDisplayName <$> mUser)
  nonce <-
    ExceptT $
      note NonceNotFound
        <$> wrapClient
          ( Nonce.lookupAndDeleteNonce
              uid
              (T.decodeUtf8With lenientDecode . toStrict $ toByteString cid)
          )
  httpsUrl <-
    except $
      note MisconfiguredRequestUrl $
        fromByteString $
          "https://" <> toByteString' domain <> "/" <> T.encodeUtf8 (toUrlPiece link)
  maxSkewSeconds <- Opt.setDpopMaxSkewSecs <$> asks (.settings)
  expiresIn <- Opt.dpopTokenExpirationTimeSecs <$> asks (.settings)
  now <- fromUTCTime <$> lift (liftSem Now.get)
  let expiresAt = now & addToEpoch expiresIn
  pubKeyBundle <- do
    pathToKeys <- ExceptT (note KeyBundleError <$> asks (.settings.publicKeyBundle))
    ExceptT $ note KeyBundleError <$> liftSem (PublicKeyBundle.get pathToKeys)
  token <-
    ExceptT $
      liftSem $
        JwtTools.generateDPoPAccessToken
          proof
          (ClientIdentity domain uid cid)
          handle
          displayName
          tid
          nonce
          httpsUrl
          method
          maxSkewSeconds
          expiresAt
          pubKeyBundle
  pure $ (DPoPAccessTokenResponse token DPoP expiresIn, NoStore)
