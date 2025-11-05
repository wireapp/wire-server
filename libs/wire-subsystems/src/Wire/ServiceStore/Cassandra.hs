module Wire.ServiceStore.Cassandra where

import Cassandra
import Cassandra qualified as C
import Control.Lens
import Data.Id
import Data.Misc
import Imports
import Polysemy
import Polysemy.TinyLog
import Wire.API.Bot.Service qualified as Bot
import Wire.API.Provider
import Wire.API.Provider.Service hiding (DeleteService)
import Wire.ServiceStore (ServiceStore (..))
import Wire.Util

interpretServiceStoreToCassandra ::
  ( Member (Embed IO) r,
    Member TinyLog r
  ) =>
  ClientState ->
  Sem (ServiceStore ': r) a ->
  Sem r a
interpretServiceStoreToCassandra cassClient = interpret $ \case
  CreateService s -> do
    logEffect "ServiceStore.CreateService"
    embedClient cassClient $ insertService s
  GetService sr -> do
    logEffect "ServiceStore.GetService"
    embedClient cassClient $ lookupService sr
  DeleteService sr -> do
    logEffect "ServiceStore.DeleteService"
    embedClient cassClient $ deleteService sr

insertService :: (MonadClient m) => Bot.Service -> m ()
insertService s = do
  let sid = s ^. Bot.serviceRef . serviceRefId
  let pid = s ^. Bot.serviceRef . serviceRefProvider
  let tok = s ^. Bot.serviceToken
  let url = s ^. Bot.serviceUrl
  let fps = Set (s ^. Bot.serviceFingerprints)
  let ena = s ^. Bot.serviceEnabled
  retry x5 $ write insertSrv (params LocalQuorum (pid, sid, url, tok, fps, ena))

lookupService :: (MonadClient m) => ServiceRef -> m (Maybe Bot.Service)
lookupService s =
  fmap toService
    <$> retry x1 (query1 selectSrv (params LocalQuorum (s ^. serviceRefProvider, s ^. serviceRefId)))
  where
    toService (url, tok, Set fps, ena) =
      Bot.newService s url tok fps & set Bot.serviceEnabled ena

deleteService :: (MonadClient m) => ServiceRef -> m ()
deleteService s = retry x5 (write rmSrv (params LocalQuorum (s ^. serviceRefProvider, s ^. serviceRefId)))

rmSrv :: PrepQuery W (ProviderId, ServiceId) ()
rmSrv = "delete from service where provider = ? AND id = ?"

insertSrv :: PrepQuery W (ProviderId, ServiceId, HttpsUrl, ServiceToken, C.Set (Fingerprint Rsa), Bool) ()
insertSrv = "insert into service (provider, id, base_url, auth_token, fingerprints, enabled) values (?, ?, ?, ?, ?, ?)"

selectSrv :: PrepQuery R (ProviderId, ServiceId) (HttpsUrl, ServiceToken, C.Set (Fingerprint Rsa), Bool)
selectSrv = "select base_url, auth_token, fingerprints, enabled from service where provider = ? AND id = ?"
