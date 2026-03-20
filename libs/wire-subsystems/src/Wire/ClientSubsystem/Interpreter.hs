module Wire.ClientSubsystem.Interpreter (runClientSubsystem) where

import Control.Monad
import Data.Domain
import Data.Id
import Data.Map as Map
import Data.Qualified
import Data.Set as Set
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
import Wire.API.User.Client
import Wire.API.UserMap
import Wire.ClientStore (ClientStore)
import Wire.ClientStore qualified as ClientStore
import Wire.ClientSubsystem (ClientSubsystem (..))
import Wire.FederationAPIAccess (FederationAPIAccess, runFederatedEither)
import Wire.Sem.Logger qualified as Log
import Wire.Util

runClientSubsystem ::
  ( Member ClientStore r,
    Member (Input (Local ())) r,
    Member TinyLog r,
    Member (FederationAPIAccess m) r,
    RunClient (m 'Brig),
    FederationMonad m,
    Typeable m
  ) =>
  InterpreterFor ClientSubsystem r
runClientSubsystem = interpret $ \case
  InternalGetActivityTimestamps uid -> internalGetActivityTimestamps uid
  LookupLocalClient uid cid -> lookupLocalClient uid cid
  LookupLocalClients uid -> lookupLocalClients uid
  LookupLocalPublicClientsBulk uids -> lookupLocalPublicClientsBulk uids
  LookupPublicClient quid cid -> lookupPubClient quid cid
  LookupPublicClients quid -> lookupPubClients quid
  LookupPublicClientsBulk quids -> lookupPubClientsBulk quids

--
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
