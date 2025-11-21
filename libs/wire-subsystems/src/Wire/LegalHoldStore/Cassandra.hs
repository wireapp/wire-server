module Wire.LegalHoldStore.Cassandra (interpretLegalHoldStoreToCassandra, validateServiceKey) where

import Cassandra
import Control.Exception (catch)
import Data.ByteString.Conversion.To
import Data.ByteString.Lazy.Char8 qualified as LC8
import Data.Id
import Data.LegalHold
import Data.Misc
import Galley.Types.Teams (FeatureDefaults (..))
import Imports
import OpenSSL.EVP.Digest qualified as SSL
import OpenSSL.EVP.PKey qualified as SSL
import OpenSSL.PEM qualified as SSL
import OpenSSL.RSA qualified as SSL
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import Ssl.Util qualified as SSL
import Wire.API.Provider.Service
import Wire.API.Team.Feature (LegalholdConfig)
import Wire.API.Team.LegalHold.Internal
import Wire.API.User.Client.Prekey
import Wire.LegalHoldStore (LegalHoldStore (..))
import Wire.LegalHoldStore.Cassandra.Queries qualified as Q
import Wire.LegalHoldStore.Env (LegalHoldEnv (..))
import Wire.TeamStore.Cassandra.Queries qualified as QTS
import Wire.Util (embedClientInput, logEffect)

interpretLegalHoldStoreToCassandra ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
    Member (Input LegalHoldEnv) r,
    Member TinyLog r
  ) =>
  FeatureDefaults LegalholdConfig ->
  Sem (LegalHoldStore ': r) a ->
  Sem r a
interpretLegalHoldStoreToCassandra lh = interpret $ \case
  CreateSettings s -> do
    logEffect "LegalHoldStore.CreateSettings"
    embedClientInput $ createSettings s
  GetSettings tid -> do
    logEffect "LegalHoldStore.GetSettings"
    embedClientInput $ getSettings tid
  RemoveSettings tid -> do
    logEffect "LegalHoldStore.RemoveSettings"
    embedClientInput $ removeSettings tid
  InsertPendingPrekeys uid pkeys -> do
    logEffect "LegalHoldStore.InsertPendingPrekeys"
    embedClientInput $ insertPendingPrekeys uid pkeys
  SelectPendingPrekeys uid -> do
    logEffect "LegalHoldStore.SelectPendingPrekeys"
    embedClientInput $ selectPendingPrekeys uid
  DropPendingPrekeys uid -> do
    logEffect "LegalHoldStore.DropPendingPrekeys"
    embedClientInput $ dropPendingPrekeys uid
  SetUserLegalHoldStatus tid uid st -> do
    logEffect "LegalHoldStore.SetUserLegalHoldStatus"
    embedClientInput $ setUserLegalHoldStatus tid uid st
  SetTeamLegalholdWhitelisted tid -> do
    logEffect "LegalHoldStore.SetTeamLegalholdWhitelisted"
    embedClientInput $ setTeamLegalholdWhitelisted tid
  UnsetTeamLegalholdWhitelisted tid -> do
    logEffect "LegalHoldStore.UnsetTeamLegalholdWhitelisted"
    embedClientInput $ unsetTeamLegalholdWhitelisted tid
  IsTeamLegalholdWhitelisted tid -> do
    logEffect "LegalHoldStore.IsTeamLegalholdWhitelisted"
    embedClientInput $ isTeamLegalholdWhitelisted lh tid
  MakeVerifiedRequestFreshManager fpr url r -> do
    logEffect "LegalHoldStore.MakeVerifiedRequestFreshManager"
    env <- input
    embed @IO $ makeVerifiedRequestFreshManager env fpr url r
  MakeVerifiedRequest fpr url r -> do
    logEffect "LegalHoldStore.MakeVerifiedRequest"
    env <- input
    embed @IO $ makeVerifiedRequest env fpr url r
  ValidateServiceKey sk -> do
    logEffect "LegalHoldStore.ValidateServiceKey"
    embed @IO $ validateServiceKey sk

createSettings :: (MonadClient m) => LegalHoldService -> m ()
createSettings (LegalHoldService tid url fpr tok key) =
  retry x1 $ write Q.insertLegalHoldSettings (params LocalQuorum (url, fpr, tok, key, tid))

getSettings :: (MonadClient m) => TeamId -> m (Maybe LegalHoldService)
getSettings tid = fmap toLegalHoldService <$> retry x1 (query1 Q.selectLegalHoldSettings (params LocalQuorum (Identity tid)))
  where
    toLegalHoldService (httpsUrl, fingerprint, tok, key) = LegalHoldService tid httpsUrl fingerprint tok key

removeSettings :: (MonadClient m) => TeamId -> m ()
removeSettings tid = retry x5 (write Q.removeLegalHoldSettings (params LocalQuorum (Identity tid)))

insertPendingPrekeys :: (MonadClient m) => UserId -> [Prekey] -> m ()
insertPendingPrekeys uid keys = retry x5 . batch $ do
  forM_ keys $ \(Prekey keyId key) -> addPrepQuery Q.insertPendingPrekeys (uid, keyId, key)

selectPendingPrekeys :: (MonadClient m) => UserId -> m (Maybe ([Prekey], LastPrekey))
selectPendingPrekeys uid = pickLastKey . fmap fromTuple <$> retry x1 (query Q.selectPendingPrekeys (params LocalQuorum (Identity uid)))
  where
    fromTuple (keyId, key) = Prekey keyId key
    pickLastKey allPrekeys = case unsnoc allPrekeys of
      Nothing -> Nothing
      Just (keys, lst) -> pure (keys, lastPrekey . prekeyKey $ lst)

dropPendingPrekeys :: (MonadClient m) => UserId -> m ()
dropPendingPrekeys uid = retry x5 (write Q.dropPendingPrekeys (params LocalQuorum (Identity uid)))

setUserLegalHoldStatus :: (MonadClient m) => TeamId -> UserId -> UserLegalHoldStatus -> m ()
setUserLegalHoldStatus tid uid status = retry x5 (write Q.updateUserLegalHoldStatus (params LocalQuorum (status, tid, uid)))

setTeamLegalholdWhitelisted :: (MonadClient m) => TeamId -> m ()
setTeamLegalholdWhitelisted tid = retry x5 (write Q.insertLegalHoldWhitelistedTeam (params LocalQuorum (Identity tid)))

unsetTeamLegalholdWhitelisted :: (MonadClient m) => TeamId -> m ()
unsetTeamLegalholdWhitelisted tid = retry x5 (write Q.removeLegalHoldWhitelistedTeam (params LocalQuorum (Identity tid)))

isTeamLegalholdWhitelisted :: FeatureDefaults LegalholdConfig -> TeamId -> Client Bool
isTeamLegalholdWhitelisted FeatureLegalHoldDisabledPermanently _ = pure False
isTeamLegalholdWhitelisted FeatureLegalHoldDisabledByDefault _ = pure False
isTeamLegalholdWhitelisted FeatureLegalHoldWhitelistTeamsAndImplicitConsent tid =
  isJust <$> (runIdentity <$$> retry x5 (query1 QTS.selectLegalHoldWhitelistedTeam (params LocalQuorum (Identity tid))))

validateServiceKey :: (MonadIO m) => ServiceKeyPEM -> m (Maybe (ServiceKey, Fingerprint Rsa))
validateServiceKey pem =
  liftIO $
    readPublicKey >>= \pk ->
      case SSL.toPublicKey =<< pk of
        Nothing -> pure Nothing
        Just pk' -> do
          Just sha <- SSL.getDigestByName "SHA256"
          let size = SSL.rsaSize (pk' :: SSL.RSAPubKey)
          if size < minRsaKeySize
            then pure Nothing
            else do
              fpr <- Fingerprint <$> SSL.rsaFingerprint sha pk'
              let bits = fromIntegral size * 8
              let key = ServiceKey RsaServiceKey bits pem
              pure (Just (key, fpr))
  where
    readPublicKey =
      ( do
          pk <- SSL.readPublicKey (LC8.unpack (toByteString pem))
          pure (Just pk)
      )
        `catch` (\(_ :: SomeException) -> pure Nothing)
    minRsaKeySize :: Int
    minRsaKeySize = 256
