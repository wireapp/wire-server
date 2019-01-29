{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}  -- for Show UserRowInsert

-- TODO: Move to Brig.User.Account.DB
module Brig.Data.User
    ( AuthError (..)
    , ReAuthError (..)
    , newAccount
    , authenticate
    , reauthenticate
    , filterActive
    , isActivated
    , lookupAccount
    , lookupAccounts
    , lookupUser
    , lookupUsers
    , lookupName
    , lookupLocale
    , lookupPassword
    , lookupStatus
    , lookupUserTeam
    , insertAccount
    , updateUser
    , updateEmail
    , updatePhone
    , updateSSOId
    , activateUser
    , deactivateUser
    , updateLocale
    , updatePassword
    , updateStatus
    , updateSearchableStatus
    , deleteEmail
    , deletePhone
    , deleteServiceUser
    , lookupServiceUsers
    , lookupServiceUsersForTeam
    , updateHandle
    ) where

import Imports
import Brig.App (AppIO, settings, zauthEnv, currentTime)
import Brig.Data.Instances ()
import Brig.Options
import Brig.Password
import Brig.Types
import Brig.Types.Intra
import Brig.Types.User (newUserExpiresIn)
import Cassandra
import Control.Error
import Control.Lens hiding (from)
import Data.Id
import Data.Json.Util (UTCTimeMillis, toUTCTimeMillis)
import Data.Misc (PlainTextPassword (..))
import Data.Range (fromRange)
import Data.Time (addUTCTime)
import Data.Conduit (ConduitM)
import Data.UUID.V4
import Galley.Types.Bot

import qualified Brig.ZAuth as ZAuth

-- | Authentication errors.
data AuthError
    = AuthInvalidUser
    | AuthInvalidCredentials
    | AuthSuspended
    | AuthEphemeral

-- | Re-authentication errors.
data ReAuthError
    = ReAuthError !AuthError
    | ReAuthMissingPassword

newAccount :: NewUser -> Maybe InvitationId -> Maybe TeamId -> AppIO (UserAccount, Maybe Password)
newAccount u inv tid = do
    defLoc  <- setDefaultLocale <$> view settings
    uid     <- Id <$> do
        case (inv, newUserUUID u) of
            (Just (toUUID -> uuid), _) -> pure uuid
            (_, Just uuid)             -> pure uuid
            (Nothing, Nothing)         -> liftIO nextRandom
    passwd  <- maybe (return Nothing) (fmap Just . liftIO . mkSafePassword) pass
    expiry  <- case status of
                   Ephemeral -> do
                       -- Ephemeral users' expiry time is in expires_in (default sessionTokenTimeout) seconds
                       e <- view zauthEnv
                       let ZAuth.SessionTokenTimeout defTTL = e^.ZAuth.settings.ZAuth.sessionTokenTimeout
                           ttl = fromMaybe defTTL (fromRange <$> newUserExpiresIn u)
                       now <- liftIO =<< view currentTime
                       return . Just . toUTCTimeMillis $ addUTCTime (fromIntegral ttl) now
                   _ -> return Nothing
    return (UserAccount (user uid (locale defLoc) expiry) status, passwd)
  where
    ident         = newUserIdentity u
    pass          = newUserPassword u
    name          = newUserName u
    pict          = fromMaybe noPict (newUserPict u)
    assets        = newUserAssets u
    status        = case ident of
                        Nothing -> -- any user registering without either an email or a phone is Ephemeral,
                                   -- i.e. can be deleted after expires_in or sessionTokenTimeout
                                   Ephemeral
                        Just _  -> Active
    colour        = fromMaybe defaultAccentId (newUserAccentId u)
    locale defLoc = fromMaybe defLoc (newUserLocale u)
    user  uid l e = User uid ident name pict assets colour False l Nothing Nothing e tid ManagedByWire

-- | Mandatory password authentication.
authenticate :: UserId -> PlainTextPassword -> ExceptT AuthError AppIO ()
authenticate u pw = lift (lookupAuth u) >>= \case
    Nothing                   -> throwE AuthInvalidUser
    Just (_,         Deleted) -> throwE AuthInvalidUser
    Just (_,       Suspended) -> throwE AuthSuspended
    Just (_,       Ephemeral) -> throwE AuthEphemeral
    Just (Nothing,         _) -> throwE AuthInvalidCredentials
    Just (Just pw',   Active) ->
        unless (verifyPassword pw pw') $
            throwE AuthInvalidCredentials

-- | Password reauthentication. If the account has a password, reauthentication
-- is mandatory. If the account has no password and no password is given,
-- reauthentication is a no-op.
reauthenticate :: (MonadClient m) => UserId -> Maybe PlainTextPassword -> ExceptT ReAuthError m ()
reauthenticate u pw = lift (lookupAuth u) >>= \case
    Nothing                    -> throwE (ReAuthError AuthInvalidUser)
    Just (_,          Deleted) -> throwE (ReAuthError AuthInvalidUser)
    Just (_,        Suspended) -> throwE (ReAuthError AuthSuspended)
    Just (Nothing,          _) -> for_ pw $ const (throwE $ ReAuthError AuthInvalidCredentials)
    Just (Just pw',    Active) -> maybeReAuth pw'
    Just (Just pw', Ephemeral) -> maybeReAuth pw'
  where
    maybeReAuth pw' = case pw of
        Nothing -> throwE ReAuthMissingPassword
        Just  p ->
            unless (verifyPassword p pw') $
                throwE (ReAuthError AuthInvalidCredentials)

insertAccount
    :: UserAccount
    -> Maybe (ConvId, Maybe TeamId) -- ^ If a bot: conversation and team
                                    --   (if a team conversation)
    -> Maybe Password
    -> Bool                         -- ^ Whether the user is activated
    -> SearchableStatus
    -> AppIO ()
insertAccount (UserAccount u status) mbConv password activated searchable = retry x5 $ batch $ do
    setType BatchLogged
    setConsistency Quorum
    let Locale l c = userLocale u
    addPrepQuery userInsert
        ( userId u, userName u, userPict u, userAssets u, userEmail u
        , userPhone u, userSSOId u, userAccentId u, password, activated
        , status, userExpire u, l, c
        , view serviceRefProvider <$> userService u
        , view serviceRefId <$> userService u
        , userHandle u
        , searchable
        , userTeam u
        , userManagedBy u
        )
    for_ ((,) <$> userService u <*> mbConv) $ \(sref, (cid, mbTid)) -> do
        let pid = sref ^. serviceRefProvider
            sid = sref ^. serviceRefId
        addPrepQuery cqlServiceUser (pid, sid, BotId (userId u), cid, mbTid)
        for_ mbTid $ \tid ->
            addPrepQuery cqlServiceTeam (pid, sid, BotId (userId u), cid, tid)
  where
    cqlServiceUser :: PrepQuery W (ProviderId, ServiceId, BotId, ConvId, Maybe TeamId) ()
    cqlServiceUser = "INSERT INTO service_user (provider, service, user, conv, team) \
                     \VALUES (?, ?, ?, ?, ?)"
    cqlServiceTeam :: PrepQuery W (ProviderId, ServiceId, BotId, ConvId, TeamId) ()
    cqlServiceTeam = "INSERT INTO service_team (provider, service, user, conv, team) \
                     \VALUES (?, ?, ?, ?, ?)"

updateLocale :: UserId -> Locale -> AppIO ()
updateLocale u (Locale l c) = write userLocaleUpdate (params Quorum (l, c, u))

updateUser :: UserId -> UserUpdate -> AppIO ()
updateUser u UserUpdate{..} = retry x5 $ batch $ do
    setType BatchLogged
    setConsistency Quorum
    for_ uupName     $ \n -> addPrepQuery userNameUpdate     (n, u)
    for_ uupPict     $ \p -> addPrepQuery userPictUpdate     (p, u)
    for_ uupAssets   $ \a -> addPrepQuery userAssetsUpdate   (a, u)
    for_ uupAccentId $ \c -> addPrepQuery userAccentIdUpdate (c, u)

updateEmail :: UserId -> Email -> AppIO ()
updateEmail u e = retry x5 $ write userEmailUpdate (params Quorum (e, u))

updatePhone :: UserId -> Phone -> AppIO ()
updatePhone u p = retry x5 $ write userPhoneUpdate (params Quorum (p, u))

updateSSOId :: UserId -> UserSSOId -> AppIO Bool
updateSSOId u ssoid = do
  mteamid <- lookupUserTeam u
  case mteamid of
    Just _ -> do
      retry x5 $ write userSSOIdUpdate (params Quorum (ssoid, u))
      pure True
    Nothing -> pure False

updateHandle :: UserId -> Handle -> AppIO ()
updateHandle u h = retry x5 $ write userHandleUpdate (params Quorum (h, u))

updatePassword :: UserId -> PlainTextPassword -> AppIO ()
updatePassword u t = do
    p <- liftIO $ mkSafePassword t
    retry x5 $ write userPasswordUpdate (params Quorum (p, u))

deleteEmail :: UserId -> AppIO ()
deleteEmail u = retry x5 $ write userEmailDelete (params Quorum (Identity u))

deletePhone :: UserId -> AppIO ()
deletePhone u = retry x5 $ write userPhoneDelete (params Quorum (Identity u))

deleteServiceUser :: ProviderId -> ServiceId -> BotId -> AppIO ()
deleteServiceUser pid sid bid = do
    lookupServiceUser pid sid bid >>= \case
        Nothing -> pure ()
        Just (_, mbTid) -> retry x5 $ batch $ do
            setType BatchLogged
            setConsistency Quorum
            addPrepQuery cql (pid, sid, bid)
            for_ mbTid $ \tid ->
                addPrepQuery cqlTeam (pid, sid, tid, bid)
  where
    cql :: PrepQuery W (ProviderId, ServiceId, BotId) ()
    cql = "DELETE FROM service_user \
          \WHERE provider = ? AND service = ? AND user = ?"
    cqlTeam :: PrepQuery W (ProviderId, ServiceId, TeamId, BotId) ()
    cqlTeam = "DELETE FROM service_team \
              \WHERE provider = ? AND service = ? AND team = ? AND user = ?"

updateStatus :: UserId -> AccountStatus -> AppIO ()
updateStatus u s = retry x5 $ write userStatusUpdate (params Quorum (s, u))

updateSearchableStatus :: UserId -> SearchableStatus -> AppIO ()
updateSearchableStatus u s =
    retry x5 $ write userSearchableStatusUpdate (params Quorum (s, u))

-- | Whether the account has been activated by verifying
-- an email address or phone number.
isActivated :: UserId -> AppIO Bool
isActivated u = (== Just (Identity True)) <$>
    retry x1 (query1 activatedSelect (params Quorum (Identity u)))

filterActive :: [UserId] -> AppIO [UserId]
filterActive us = map (view _1) . filter isActiveUser <$>
    retry x1 (query accountStateSelectAll (params Quorum (Identity us)))
  where
    isActiveUser :: (UserId, Bool, Maybe AccountStatus) -> Bool
    isActiveUser (_, True, Just Active) = True
    isActiveUser  _                     = False

lookupUser :: UserId -> AppIO (Maybe User)
lookupUser u = listToMaybe <$> lookupUsers [u]

activateUser :: UserId -> UserIdentity -> AppIO ()
activateUser u ident = do
    let email = emailIdentity ident
    let phone = phoneIdentity ident
    retry x5 $ write userActivatedUpdate (params Quorum (email, phone, u))

deactivateUser :: UserId -> AppIO ()
deactivateUser u =
    retry x5 $ write userDeactivatedUpdate (params Quorum (Identity u))

lookupLocale :: UserId -> AppIO (Maybe Locale)
lookupLocale u = do
    defLoc <- setDefaultLocale <$> view settings
    fmap (toLocale defLoc) <$> retry x1 (query1 localeSelect (params Quorum (Identity u)))

lookupName :: UserId -> AppIO (Maybe Name)
lookupName u = fmap runIdentity <$>
    retry x1 (query1 nameSelect (params Quorum (Identity u)))

lookupPassword :: UserId -> AppIO (Maybe Password)
lookupPassword u = join . fmap runIdentity <$>
    retry x1 (query1 passwordSelect (params Quorum (Identity u)))

lookupStatus :: UserId -> AppIO (Maybe AccountStatus)
lookupStatus u = join . fmap runIdentity <$>
    retry x1 (query1 statusSelect (params Quorum (Identity u)))

lookupUserTeam :: UserId -> AppIO (Maybe TeamId)
lookupUserTeam u = join . fmap runIdentity <$>
    retry x1 (query1 teamSelect (params Quorum (Identity u)))

lookupAuth :: (MonadClient m) => UserId -> m (Maybe (Maybe Password, AccountStatus))
lookupAuth u = fmap f <$> retry x1 (query1 authSelect (params Quorum (Identity u)))
  where
    f (pw, st) = (pw, fromMaybe Active st)

lookupUsers :: [UserId] -> AppIO [User]
lookupUsers usrs = do
    loc <- setDefaultLocale  <$> view settings
    toUsers loc <$> retry x1 (query usersSelect (params Quorum (Identity usrs)))

lookupAccount :: UserId -> AppIO (Maybe UserAccount)
lookupAccount u = listToMaybe <$> lookupAccounts [u]

lookupAccounts :: [UserId] -> AppIO [UserAccount]
lookupAccounts usrs = do
    loc <- setDefaultLocale  <$> view settings
    fmap (toUserAccount loc) <$> retry x1 (query accountsSelect (params Quorum (Identity usrs)))

lookupServiceUser :: ProviderId -> ServiceId -> BotId -> AppIO (Maybe (ConvId, Maybe TeamId))
lookupServiceUser pid sid bid = retry x1 (query1 cql (params Quorum (pid, sid, bid)))
  where
    cql :: PrepQuery R (ProviderId, ServiceId, BotId) (ConvId, Maybe TeamId)
    cql = "SELECT conv, team FROM service_user \
          \WHERE provider = ? AND service = ? AND user = ?"

-- | NB: might return a lot of users, and therefore we do streaming here (page-by-page).
lookupServiceUsers
    :: ProviderId
    -> ServiceId
    -> ConduitM () [(BotId, ConvId, Maybe TeamId)] AppIO ()
lookupServiceUsers pid sid =
    paginateC cql (paramsP Quorum (pid, sid) 100) x1
  where
    cql :: PrepQuery R (ProviderId, ServiceId) (BotId, ConvId, Maybe TeamId)
    cql = "SELECT user, conv, team FROM service_user \
          \WHERE provider = ? AND service = ?"

lookupServiceUsersForTeam
    :: ProviderId
    -> ServiceId
    -> TeamId
    -> ConduitM () [(BotId, ConvId)] AppIO ()
lookupServiceUsersForTeam pid sid tid =
    paginateC cql (paramsP Quorum (pid, sid, tid) 100) x1
  where
    cql :: PrepQuery R (ProviderId, ServiceId, TeamId) (BotId, ConvId)
    cql = "SELECT user, conv FROM service_team \
          \WHERE provider = ? AND service = ? AND team = ?"

-------------------------------------------------------------------------------
-- Queries

type Activated = Bool

type UserRow = (UserId, Name, Maybe Pict, Maybe Email, Maybe Phone, Maybe UserSSOId, ColourId,
                Maybe [Asset], Activated, Maybe AccountStatus, Maybe UTCTimeMillis, Maybe Language,
                Maybe Country, Maybe ProviderId, Maybe ServiceId, Maybe Handle, Maybe TeamId, Maybe ManagedBy)

type UserRowInsert = (UserId, Name, Pict, [Asset], Maybe Email, Maybe Phone, Maybe UserSSOId, ColourId,
                      Maybe Password, Activated, AccountStatus, Maybe UTCTimeMillis, Language, Maybe Country,
                      Maybe ProviderId, Maybe ServiceId, Maybe Handle, SearchableStatus, Maybe TeamId, ManagedBy)

deriving instance Show UserRowInsert

-- Represents a 'UserAccount'
type AccountRow = (UserId, Name, Maybe Pict, Maybe Email, Maybe Phone, Maybe UserSSOId,
                   ColourId, Maybe [Asset], Activated, Maybe AccountStatus,
                   Maybe UTCTimeMillis, Maybe Language, Maybe Country,
                   Maybe ProviderId, Maybe ServiceId, Maybe Handle, Maybe TeamId,
                   Maybe ManagedBy)


usersSelect :: PrepQuery R (Identity [UserId]) UserRow
usersSelect = "SELECT id, name, picture, email, phone, sso_id, accent_id, assets, \
              \activated, status, expires, language, country, provider, service, \
              \handle, team, managed_by \
              \FROM user where id IN ?"

nameSelect :: PrepQuery R (Identity UserId) (Identity Name)
nameSelect = "SELECT name FROM user WHERE id = ?"

localeSelect :: PrepQuery R (Identity UserId) (Maybe Language, Maybe Country)
localeSelect = "SELECT language, country FROM user WHERE id = ?"

authSelect :: PrepQuery R (Identity UserId) (Maybe Password, Maybe AccountStatus)
authSelect = "SELECT password, status FROM user WHERE id = ?"

passwordSelect :: PrepQuery R (Identity UserId) (Identity (Maybe Password))
passwordSelect = "SELECT password FROM user WHERE id = ?"

activatedSelect :: PrepQuery R (Identity UserId) (Identity Bool)
activatedSelect = "SELECT activated FROM user WHERE id = ?"

accountStateSelectAll :: PrepQuery R (Identity [UserId]) (UserId, Bool, Maybe AccountStatus)
accountStateSelectAll = "SELECT id, activated, status FROM user WHERE id IN ?"

statusSelect :: PrepQuery R (Identity UserId) (Identity (Maybe AccountStatus))
statusSelect = "SELECT status FROM user WHERE id = ?"

teamSelect :: PrepQuery R (Identity UserId) (Identity (Maybe TeamId))
teamSelect = "SELECT team FROM user WHERE id = ?"

accountsSelect :: PrepQuery R (Identity [UserId]) AccountRow
accountsSelect = "SELECT id, name, picture, email, phone, sso_id, accent_id, assets, \
                 \activated, status, expires, language, country, provider, \
                 \service, handle, team, managed_by \
                 \FROM user WHERE id IN ?"

userInsert :: PrepQuery W UserRowInsert ()
userInsert = "INSERT INTO user (id, name, picture, assets, email, phone, sso_id, \
                               \accent_id, password, activated, status, expires, language, \
                               \country, provider, service, handle, searchable, team, managed_by) \
                               \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

userNameUpdate :: PrepQuery W (Name, UserId) ()
userNameUpdate = "UPDATE user SET name = ? WHERE id = ?"

userPictUpdate :: PrepQuery W (Pict, UserId) ()
userPictUpdate = "UPDATE user SET picture = ? WHERE id = ?"

userAssetsUpdate :: PrepQuery W ([Asset], UserId) ()
userAssetsUpdate = "UPDATE user SET assets = ? WHERE id = ?"

userAccentIdUpdate :: PrepQuery W (ColourId, UserId) ()
userAccentIdUpdate = "UPDATE user SET accent_id = ? WHERE id = ?"

userEmailUpdate :: PrepQuery W (Email, UserId) ()
userEmailUpdate = "UPDATE user SET email = ? WHERE id = ?"

userPhoneUpdate :: PrepQuery W (Phone, UserId) ()
userPhoneUpdate = "UPDATE user SET phone = ? WHERE id = ?"

userSSOIdUpdate :: PrepQuery W (UserSSOId, UserId) ()
userSSOIdUpdate = "UPDATE user SET sso_id = ? WHERE id = ?"

userHandleUpdate :: PrepQuery W (Handle, UserId) ()
userHandleUpdate = "UPDATE user SET handle = ? WHERE id = ?"

userPasswordUpdate :: PrepQuery W (Password, UserId) ()
userPasswordUpdate = "UPDATE user SET password = ? WHERE id = ?"

userStatusUpdate :: PrepQuery W (AccountStatus, UserId) ()
userStatusUpdate = "UPDATE user SET status = ? WHERE id = ?"

userSearchableStatusUpdate :: PrepQuery W (SearchableStatus, UserId) ()
userSearchableStatusUpdate = "UPDATE user SET searchable = ? WHERE id = ?"

userDeactivatedUpdate :: PrepQuery W (Identity UserId) ()
userDeactivatedUpdate = "UPDATE user SET activated = false WHERE id = ?"

userActivatedUpdate :: PrepQuery W (Maybe Email, Maybe Phone, UserId) ()
userActivatedUpdate = "UPDATE user SET activated = true, email = ?, phone = ? WHERE id = ?"

userLocaleUpdate :: PrepQuery W (Language, Maybe Country, UserId) ()
userLocaleUpdate = "UPDATE user SET language = ?, country = ? WHERE id = ?"

userEmailDelete :: PrepQuery W (Identity UserId) ()
userEmailDelete = "UPDATE user SET email = null WHERE id = ?"

userPhoneDelete :: PrepQuery W (Identity UserId) ()
userPhoneDelete = "UPDATE user SET phone = null WHERE id = ?"

-------------------------------------------------------------------------------
-- Conversions

toUserAccount :: Locale -> AccountRow -> UserAccount
toUserAccount defaultLocale (uid, name, pict, email, phone, ssoid, accent, assets,
                             activated, status, expires, lan, con, pid, sid,
                             handle, tid, managed_by) =
    let ident = toIdentity activated email phone ssoid
        deleted = maybe False (== Deleted) status
        expiration = if status == Just Ephemeral then expires else Nothing
        loc = toLocale defaultLocale (lan, con)
        svc = newServiceRef <$> sid <*> pid
    in UserAccount (User uid ident name (fromMaybe noPict pict)
                         (fromMaybe [] assets) accent deleted loc svc handle expiration tid
                         (fromMaybe ManagedByWire managed_by))
                   (fromMaybe Active status)

toUsers :: Locale -> [UserRow] -> [User]
toUsers defaultLocale = fmap mk
  where
    mk (uid, name, pict, email, phone, ssoid, accent, assets, activated, status,
        expires, lan, con, pid, sid, handle, tid, managed_by) =
        let ident = toIdentity activated email phone ssoid
            deleted = maybe False (== Deleted) status
            expiration = if status == Just Ephemeral then expires else Nothing
            loc = toLocale defaultLocale (lan, con)
            svc = newServiceRef <$> sid <*> pid
        in User uid ident name (fromMaybe noPict pict) (fromMaybe [] assets)
                accent deleted loc svc handle expiration tid
                (fromMaybe ManagedByWire managed_by)

toLocale :: Locale -> (Maybe Language, Maybe Country) -> Locale
toLocale _ (Just l, c) = Locale l c
toLocale l _           = l

toIdentity :: Bool -> Maybe Email -> Maybe Phone -> Maybe UserSSOId -> Maybe UserIdentity
toIdentity True  (Just e) (Just p) Nothing      = Just $! FullIdentity e p
toIdentity True  (Just e) Nothing  Nothing      = Just $! EmailIdentity e
toIdentity True  Nothing  (Just p) Nothing      = Just $! PhoneIdentity p
toIdentity True  email    phone    (Just ssoid) = Just $! SSOIdentity ssoid email phone
toIdentity True  Nothing  Nothing  Nothing      = Nothing
toIdentity False _        _        _            = Nothing
