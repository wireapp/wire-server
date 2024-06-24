module Wire.MockInterpreters.UserStore where

import Data.Handle
import Data.Id
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.State
import Wire.API.User hiding (DeleteUser)
import Wire.API.User qualified as User
import Wire.StoredUser
import Wire.UserStore

staticUserStoreInterpreter ::
  forall r.
  (Member (State [StoredUser]) r) =>
  InterpreterFor UserStore r
staticUserStoreInterpreter = interpret $ \case
  GetUser uid -> gets $ find (\user -> user.id == uid)
  UpdateUser uid update -> modify (map doUpdate)
    where
      doUpdate :: StoredUser -> StoredUser
      doUpdate u =
        if u.id == uid
          then
            maybe Imports.id setStoredUserAccentId update.accentId
              . maybe Imports.id setStoredUserAssets update.assets
              . maybe Imports.id setStoredUserPict update.pict
              . maybe Imports.id setStoredUserName update.name
              . maybe Imports.id setStoredUserLocale update.locale
              . maybe Imports.id setStoredUserSupportedProtocols update.supportedProtocols
              $ u
          else u
  UpdateUserHandleEither uid hUpdate -> runError $ modifyLocalUsers (traverse doUpdate)
    where
      doUpdate :: StoredUser -> Sem (Error StoredUserUpdateError : r) StoredUser
      doUpdate u
        | u.id == uid = do
            handles <- gets $ mapMaybe (.handle)
            when
              ( hUpdate.old
                  /= Just hUpdate.new
                  && elem hUpdate.new handles
              )
              $ throw StoredUserUpdateHandleExists
            pure $ setStoredUserHandle hUpdate.new u
      doUpdate u = pure u

      modifyLocalUsers :: forall r1. (Member (State [StoredUser]) r1) => ([StoredUser] -> Sem r1 [StoredUser]) -> Sem r1 ()
      modifyLocalUsers f = do
        us <- get
        us' <- f us
        put us'
  DeleteUser user -> modify $ filter (\u -> u.id /= User.userId user)
  LookupHandle h -> miniBackendLookupHandle h
  GlimpseHandle h -> miniBackendLookupHandle h
  LookupStatus uid -> miniBackendLookupStatus uid
  IsActivated uid -> miniBackendIsActivated uid

miniBackendIsActivated :: (Member (State [StoredUser]) r) => UserId -> Sem r Bool
miniBackendIsActivated uid = do
  gets $
    maybe False (.activated)
      . find ((== uid) . (.id))

miniBackendLookupStatus :: (Member (State [StoredUser]) r) => UserId -> Sem r (Maybe AccountStatus)
miniBackendLookupStatus uid = do
  users <- get
  pure $ (.status) =<< (find ((== uid) . (.id)) users)

miniBackendLookupHandle ::
  (Member (State [StoredUser]) r) =>
  Handle ->
  Sem r (Maybe UserId)
miniBackendLookupHandle h = do
  gets $
    fmap (.id)
      . find ((== Just h) . (.handle))
