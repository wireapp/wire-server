{-# LANGUAGE RecordWildCards #-}

module Work where

import Brig.Data.Activation (mkActivationKey)
import Brig.Data.UserKey (UserKey, keyText, userEmailKey)
import Brig.Types.Activation (ActivationKey)
import Cassandra
import Data.Id
import Imports
import qualified Spar.Data as SparData
import Wire.API.User

data Env = Env
  { brigClient :: ClientState,
    sparClient :: ClientState
  }

changeExtId :: Env -> TeamId -> Email -> ExternalId -> IO ()
changeExtId env@Env {..} tid email extid = do
  mbUByExt <- lookupExternalId env tid (fromEmail email)
  mbUByEmail <- lookupByEmail env email
  case listToMaybe (catMaybes [mbUByExt, mbUByEmail]) of
    Nothing -> do
      pure ()
    Just uid -> do
      managedBy <- fromMaybe ManagedByWire <$> getUserManagedBy env uid
      case managedBy of
        ManagedByWire -> do
          pure ()
        ManagedByScim -> do
          runClient sparClient $
            SparData.insertScimExternalId extid uid
          let authId = AuthSCIM (ScimDetails extid (EmailWithSource email EmailFromEmailField))
          updateAuthId env uid (Just authId)

lookupExternalId :: Env -> TeamId -> Text -> IO (Maybe UserId)
lookupExternalId Env {..} tid extid =
  runClient sparClient $
    SparData.lookupScimExternalId (ExternalId tid extid)

-- Should behave similar to Brig.API.User.lookupAccountsByIdentity
lookupByEmail :: Env -> Email -> IO (Maybe UserId)
lookupByEmail env email = do
  let uk = userEmailKey email
  activeUid <- lookupKey env uk
  uidFromKey <- lookupActivationCode env uk
  pure (listToMaybe $ catMaybes [activeUid, uidFromKey])

-- -- Should behave similar to Brig.Data.UserKey.lookupKey
lookupKey :: Env -> UserKey -> IO (Maybe UserId)
lookupKey Env {..} k =
  runClient brigClient $
    fmap runIdentity
      <$> retry x1 (query1 keySelect (params Quorum (Identity $ keyText k)))
  where
    keySelect :: PrepQuery R (Identity Text) (Identity UserId)
    keySelect = "SELECT user FROM user_keys WHERE key = ?"

lookupActivationCode :: Env -> UserKey -> IO (Maybe UserId)
lookupActivationCode Env {..} k =
  fmap join $
    fmap runIdentity
      <$> runClient
        brigClient
        ( liftIO (mkActivationKey k)
            >>= retry x1 . query1 codeSelect . params Quorum . Identity
        )
  where
    codeSelect :: PrepQuery R (Identity ActivationKey) (Identity (Maybe UserId))
    codeSelect = "SELECT user FROM activation_keys WHERE key = ?"

updateAuthId :: Env -> UserId -> Maybe AuthId -> IO ()
updateAuthId Env {..} u authid =
  runClient brigClient $
    retry x5 $ write userAuthIdUpdate (params Quorum (authid, u))
  where
    userAuthIdUpdate :: PrepQuery W (Maybe AuthId, UserId) ()
    userAuthIdUpdate = "UPDATE user SET sso_id = ? WHERE id = ?"

getUserManagedBy :: Env -> UserId -> IO (Maybe ManagedBy)
getUserManagedBy Env {..} u = do
  fmap runIdentity
    <$> runClient
      brigClient
      ( retry x5 $ query1 selectManagedBy (params Quorum (Identity u))
      )
  where
    selectManagedBy :: PrepQuery R (Identity UserId) (Identity ManagedBy)
    selectManagedBy = "SELECT managed_by WHERE id = ?"
