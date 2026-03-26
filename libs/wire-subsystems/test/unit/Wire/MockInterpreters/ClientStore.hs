module Wire.MockInterpreters.ClientStore where

import Data.ByteString.Lazy qualified as LBS
import Data.Id
import Data.Map qualified as Map
import Data.Set qualified as Set
import Imports
import Polysemy
import Polysemy.State
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.UserMap
import Wire.ClientStore

data StoredClient = StoredClient
  { client :: Client,
    prekeys :: [UncheckedPrekeyBundle]
  }

newtype ClientStoreState = ClientStoreState
  { byUser :: Map UserId (Map ClientId StoredClient)
  }

emptyClientStoreState :: ClientStoreState
emptyClientStoreState = ClientStoreState mempty

runInMemoryClientStoreInterpreter :: InterpreterFor ClientStore r
runInMemoryClientStoreInterpreter =
  evalState emptyClientStoreState . reinterpret handleClientStore

runInMemoryClientStoreInterpreterWithState :: (Member (State ClientStoreState) r) => InterpreterFor ClientStore r
runInMemoryClientStoreInterpreterWithState = interpret handleClientStore

handleClientStore :: (Member (State ClientStoreState) r) => ClientStore m a -> Sem r a
handleClientStore = \case
  Upsert uid cid now new -> do
    let client =
          Client
            { clientId = cid,
              clientType = new.newClientType,
              clientTime = now,
              clientClass = new.newClientClass,
              clientLabel = new.newClientLabel,
              clientCookie = new.newClientCookie,
              clientModel = new.newClientModel,
              clientCapabilities = fromMaybe mempty new.newClientCapabilities,
              clientMLSPublicKeys = new.newClientMLSPublicKeys,
              clientLastActive = Nothing
            }
        stored =
          StoredClient
            { client,
              prekeys = unpackLastPrekey new.newClientLastKey : new.newClientPrekeys
            }
    modify \st -> st {byUser = Map.alter (Just . Map.insert cid stored . fromMaybe mempty) uid st.byUser}
    pure Nothing
  Delete uid cid ->
    modify \st -> st {byUser = Map.update (\clients -> Just $ Map.delete cid clients) uid st.byUser}
  UpdateLabel uid cid label ->
    modifyStoredClient uid cid \stored ->
      stored {client = stored.client {clientLabel = label}}
  UpdateCapabilities uid cid capabilities ->
    modifyStoredClient uid cid \stored -> stored {client = stored.client {clientCapabilities = fromMaybe mempty capabilities}}
  UpdateLastActive uid cid lastActive ->
    modifyStoredClient uid cid \stored -> stored {client = stored.client {clientLastActive = Just lastActive}}
  LookupClient uid cid ->
    gets $ fmap (.client) . Map.lookup cid . Map.findWithDefault mempty uid . (.byUser)
  LookupClients uid ->
    gets $ map (.client) . Map.elems . Map.findWithDefault mempty uid . (.byUser)
  LookupClientIds uid ->
    gets $ Map.keys . Map.findWithDefault mempty uid . (.byUser)
  LookupClientIdsBulk uids ->
    gets $ \st ->
      mkUserClients
        [ (uid, Map.keys $ Map.findWithDefault mempty uid st.byUser)
        | uid <- uids
        ]
  LookupClientsBulk uids ->
    gets $ \st ->
      UserMap $
        Map.fromList
          [ (uid, Set.fromList $ map (.client) $ Map.elems $ Map.findWithDefault mempty uid st.byUser)
          | uid <- uids
          ]
  LookupPubClientsBulk uids ->
    gets $ \st ->
      UserMap $
        Map.fromList
          [ (uid, Set.fromList $ toPubClient . (.client) <$> Map.elems (Map.findWithDefault mempty uid st.byUser))
          | uid <- uids
          ]
  LookupPrekeyIds uid cid ->
    gets $ maybe [] (map (.prekeyId) . (.prekeys)) . Map.lookup cid . Map.findWithDefault mempty uid . (.byUser)
  GetActivityTimestamps uid ->
    gets $ map (.client.clientLastActive) . Map.elems . Map.findWithDefault mempty uid . (.byUser)
  UpdatePrekeys uid cid prekeys ->
    modifyStoredClient uid cid \stored -> stored {prekeys}
  ClaimPrekey uid cid -> do
    mStored <- gets $ Map.lookup cid . Map.findWithDefault mempty uid . (.byUser)
    case mStored of
      Nothing -> pure Nothing
      Just stored -> case stored.prekeys of
        [] -> pure Nothing
        prekey : rest -> do
          modifyStoredClient uid cid \stored' -> stored' {prekeys = rest}
          pure (Just (ClientPrekey cid prekey))
  AddMLSPublicKeys uid cid keys -> do
    modifyStoredClient uid cid \stored ->
      let merged = Map.union stored.client.clientMLSPublicKeys (Map.fromList keys)
       in stored {client = stored.client {clientMLSPublicKeys = merged}}
    pure Nothing
  LookupMLSPublicKey uid cid scheme ->
    gets $
      fmap (LBS.fromStrict)
        . ( ( Map.lookup scheme
                . (.client.clientMLSPublicKeys)
            )
              <=< (Map.lookup cid . Map.findWithDefault mempty uid . (.byUser))
          )

toPubClient :: Client -> PubClient
toPubClient client =
  PubClient
    { pubClientId = client.clientId,
      pubClientClass = client.clientClass
    }

modifyStoredClient ::
  (Member (State ClientStoreState) r) =>
  UserId ->
  ClientId ->
  (StoredClient -> StoredClient) ->
  Sem r ()
modifyStoredClient uid cid update =
  modify \st -> st {byUser = Map.adjust (Map.adjust update cid) uid st.byUser}
