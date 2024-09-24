{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Wire.MockInterpreters.UserStore where

import Cassandra.Util
import Data.Handle
import Data.Id
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.State
import Wire.API.User hiding (DeleteUser)
import Wire.API.User qualified as User
import Wire.StoredUser
import Wire.UserStore
import Wire.UserStore.IndexUser

inMemoryUserStoreInterpreter ::
  forall r.
  (Member (State [StoredUser]) r) =>
  InterpreterFor UserStore r
inMemoryUserStoreInterpreter = interpret $ \case
  GetUsers uids -> gets $ filter (\user -> user.id `elem` uids)
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
  GetIndexUser uid ->
    gets $ fmap storedUserToIndexUser . find (\user -> user.id == uid)
  GetIndexUsersPaginated _pageSize _pagingState ->
    error "GetIndexUsersPaginated not implemented in inMemoryUserStoreInterpreter"
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
  LookupHandle h -> lookupHandleImpl h
  GlimpseHandle h -> lookupHandleImpl h
  LookupStatus uid -> lookupStatusImpl uid
  IsActivated uid -> isActivatedImpl uid
  LookupLocale uid -> lookupLocaleImpl uid
  UpdateUserTeam uid tid ->
    modify $
      map
        (\u -> if u.id == uid then u {teamId = Just tid} :: StoredUser else u)

storedUserToIndexUser :: StoredUser -> IndexUser
storedUserToIndexUser storedUser =
  -- If we really care about this, we could start storing the writetimes, but we
  -- don't need it right now
  let withDefaultTime x = WithWriteTime x $ Writetime $ UTCTime (YearDay 0 1) 0
   in IndexUser
        { userId = storedUser.id,
          teamId = withDefaultTime <$> storedUser.teamId,
          name = withDefaultTime storedUser.name,
          accountStatus = withDefaultTime <$> storedUser.status,
          handle = withDefaultTime <$> storedUser.handle,
          email = withDefaultTime <$> storedUser.email,
          colourId = withDefaultTime storedUser.accentId,
          activated = withDefaultTime storedUser.activated,
          serviceId = withDefaultTime <$> storedUser.serviceId,
          managedBy = withDefaultTime <$> storedUser.managedBy,
          ssoId = withDefaultTime <$> storedUser.ssoId,
          unverifiedEmail = Nothing,
          writeTimeBumper = Nothing
        }

lookupLocaleImpl :: (Member (State [StoredUser]) r) => UserId -> Sem r (Maybe ((Maybe Language, Maybe Country)))
lookupLocaleImpl uid = do
  users <- get
  let mUser = find ((== uid) . (.id)) users
  pure $ (\u -> (u.language, u.country)) <$> mUser

isActivatedImpl :: (Member (State [StoredUser]) r) => UserId -> Sem r Bool
isActivatedImpl uid = do
  gets $
    maybe False (.activated)
      . find ((== uid) . (.id))

lookupStatusImpl :: (Member (State [StoredUser]) r) => UserId -> Sem r (Maybe AccountStatus)
lookupStatusImpl uid = do
  users <- get
  pure $ (.status) =<< (find ((== uid) . (.id)) users)

lookupHandleImpl ::
  (Member (State [StoredUser]) r) =>
  Handle ->
  Sem r (Maybe UserId)
lookupHandleImpl h = do
  gets $
    fmap (.id)
      . find ((== Just h) . (.handle))
