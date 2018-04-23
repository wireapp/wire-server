{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
    , insertAccount
    , updateUser
    , updateEmail
    , updatePhone
    , activateUser
    , deactivateUser
    , updateLocale
    , updatePassword
    , updateStatus
    , updateSearchableStatus
    , deleteEmail
    , deletePhone
    , updateHandle

    -- * Newtype wrappers
    , Activated(..)
    , Expires(..)
    ) where

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
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Foldable (for_)
import Data.Id
import Data.Misc (PlainTextPassword (..))
import Data.Range (fromRange)
import Data.Time (UTCTime, addUTCTime)
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
    defaultLocale <- setDefaultLocale <$> view settings
    uid     <- Id <$> maybe (liftIO nextRandom) (return . toUUID) inv
    passwd  <- maybe (return Nothing) (fmap Just . mkSafePassword) (newUserPassword u)
    expiry  <- case status of
                   Ephemeral -> do
                       -- Ephemeral users' expiry time is in expires_in (default sessionTokenTimeout) seconds
                       e <- view zauthEnv
                       let ZAuth.SessionTokenTimeout defTTL = e^.ZAuth.settings.ZAuth.sessionTokenTimeout
                           ttl = fromMaybe defTTL (fromRange <$> newUserExpiresIn u)
                       now <- liftIO =<< view currentTime
                       return $ Just (addUTCTime (fromIntegral ttl) now)
                   _ -> return Nothing
    return (UserAccount (mkUser uid defaultLocale expiry) status, passwd)
  where
    status = case newUserIdentity u of
        -- any user registering without either an email or a phone is Ephemeral,
        -- i.e. can be deleted after expires_in or sessionTokenTimeout
        Nothing -> Ephemeral
        Just _  -> Active
    mkUser uid defaultLocale expiry =
        User {
            -- fields passed to 'newAccount'
              userTeam     = tid
            -- fields taken from 'NewUser'
            , userIdentity = newUserIdentity u
            , userName     = newUserName     u
            , userPict     = newUserPict     u & fromMaybe noPict
            , userAssets   = newUserAssets   u
            , userAccentId = newUserAccentId u & fromMaybe defaultAccentId
            , userLocale   = newUserLocale   u & fromMaybe defaultLocale
            -- fields that are generated during user creation
            , userId       = uid
            , userExpire   = expiry
            -- constant fields
            , userDeleted  = False
            , userService  = Nothing
            , userHandle   = Nothing
            , userJournal  = Nothing
            }

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
    :: UserAccount -> Maybe Password -> Activated -> SearchableStatus -> AppIO ()
insertAccount (UserAccount u status) password activated searchable = do
    let Locale l c = userLocale u
    retry x5 $ write userInsert $ params Quorum
        ( userId u, userName u, userPict u, userAssets u, userEmail u
        , userPhone u, userAccentId u, password, activated
        , status, Expires <$> userExpire u, l, c
        , view serviceRefProvider <$> userService u
        , view serviceRefId <$> userService u
        , userHandle u
        , searchable
        , userTeam u
        , userJournal u
        )

updateLocale :: UserId -> Locale -> AppIO ()
updateLocale u (Locale l c) = write userLocaleUpdate (params Quorum (l, c, u))

updateUser :: UserId -> UserUpdate -> AppIO ()
updateUser u UserUpdate{..} = do
    for_ uupName     $ \n -> write userNameUpdate     (params Quorum (n, u))
    for_ uupPict     $ \p -> write userPictUpdate     (params Quorum (p, u))
    for_ uupAssets   $ \a -> write userAssetsUpdate   (params Quorum (a, u))
    for_ uupAccentId $ \c -> write userAccentIdUpdate (params Quorum (c, u))

updateEmail :: UserId -> Email -> AppIO ()
updateEmail u e = retry x5 $ write userEmailUpdate (params Quorum (e, u))

updatePhone :: UserId -> Phone -> AppIO ()
updatePhone u p = retry x5 $ write userPhoneUpdate (params Quorum (p, u))

updateHandle :: UserId -> Handle -> AppIO ()
updateHandle u h = retry x5 $ write userHandleUpdate (params Quorum (h, u))

updatePassword :: UserId -> PlainTextPassword -> AppIO ()
updatePassword u t = do
    p <- mkSafePassword t
    retry x5 $ write userPasswordUpdate (params Quorum (p, u))

deleteEmail :: UserId -> AppIO ()
deleteEmail u = retry x5 $ write userEmailDelete (params Quorum (Identity u))

deletePhone :: UserId -> AppIO ()
deletePhone u = retry x5 $ write userPhoneDelete (params Quorum (Identity u))

updateStatus :: UserId -> AccountStatus -> AppIO ()
updateStatus u s = retry x5 $ write userStatusUpdate (params Quorum (s, u))

updateSearchableStatus :: UserId -> SearchableStatus -> AppIO ()
updateSearchableStatus u s =
    retry x5 $ write userSearchableStatusUpdate (params Quorum (s, u))

-- | Whether the account has been activated by verifying
-- an email address or phone number.
isActivated :: UserId -> AppIO Bool
isActivated u = (== Just (Identity (Activated True))) <$>
    retry x1 (query1 activatedSelect (params Quorum (Identity u)))

filterActive :: [UserId] -> AppIO [UserId]
filterActive us = map (view _1) . filter isActiveUser <$>
    retry x1 (query accountStateSelectAll (params Quorum (Identity us)))
  where
    isActiveUser :: (UserId, Activated, Maybe AccountStatus) -> Bool
    isActiveUser (_, Activated True, Just Active) = True
    isActiveUser  _                               = False

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
    fmap (toUserAccount loc) <$> retry x1 (query usersSelect (params Quorum (Identity usrs)))

-------------------------------------------------------------------------------
-- Queries

newtype Activated = Activated { getActivated :: Bool } deriving (Eq, Show, Cql)
newtype Expires = Expires { getExpiry :: UTCTime } deriving (Eq, Show, Cql)

-- | Things that we store in the @user@ table. Corresponds to 'UserAccount'.
type UserRow = (UserId, Name, Maybe Pict, Maybe Email, Maybe Phone, ColourId,
                Maybe [Asset], Activated, Maybe AccountStatus, Maybe Expires,
                Maybe Language, Maybe Country, Maybe ProviderId, Maybe ServiceId,
                Maybe Handle, Maybe TeamId, Maybe JournalId)

usersSelect :: PrepQuery R (Identity [UserId]) UserRow
usersSelect = "SELECT id, name, picture, email, phone, accent_id, assets, \
              \activated, status, expires, language, country, provider, \
              \service, handle, team, journal \
              \FROM user where id IN ?"

nameSelect :: PrepQuery R (Identity UserId) (Identity Name)
nameSelect = "SELECT name FROM user WHERE id = ?"

localeSelect :: PrepQuery R (Identity UserId) (Maybe Language, Maybe Country)
localeSelect = "SELECT language, country FROM user WHERE id = ?"

authSelect :: PrepQuery R (Identity UserId) (Maybe Password, Maybe AccountStatus)
authSelect = "SELECT password, status FROM user WHERE id = ?"

passwordSelect :: PrepQuery R (Identity UserId) (Identity (Maybe Password))
passwordSelect = "SELECT password FROM user WHERE id = ?"

activatedSelect :: PrepQuery R (Identity UserId) (Identity Activated)
activatedSelect = "SELECT activated FROM user WHERE id = ?"

accountStateSelectAll :: PrepQuery R (Identity [UserId]) (UserId, Activated, Maybe AccountStatus)
accountStateSelectAll = "SELECT id, activated, status FROM user WHERE id IN ?"

statusSelect :: PrepQuery R (Identity UserId) (Identity (Maybe AccountStatus))
statusSelect = "SELECT status FROM user WHERE id = ?"

userInsert :: PrepQuery W (UserId, Name, Pict, [Asset], Maybe Email, Maybe Phone,
                           ColourId, Maybe Password, Activated, AccountStatus,
                           Maybe Expires, Language, Maybe Country, Maybe ProviderId,
                           Maybe ServiceId, Maybe Handle, SearchableStatus, Maybe TeamId,
                           Maybe JournalId) ()
userInsert = "INSERT INTO user (id, name, picture, assets, email, phone, \
                               \accent_id, password, activated, status, expires, language, \
                               \country, provider, service, handle, searchable, team, journal) \
                               \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

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

toUserAccount :: Locale -> UserRow -> UserAccount
toUserAccount defaultLocale (uid, name, pict, email, phone, accent, assets,
                             activated, status, expires, lan, con, pid, sid,
                             handle, tid, journal) =
    UserAccount
        { accountStatus = fromMaybe Active status
        , accountUser   = User
            { userId       = uid
            , userIdentity = toIdentity activated email phone
            , userName     = name
            , userPict     = fromMaybe noPict pict
            , userAssets   = fromMaybe [] assets
            , userAccentId = accent
            , userDeleted  = maybe False (== Deleted) status
            , userLocale   = toLocale defaultLocale (lan, con)
            , userService  = newServiceRef <$> sid <*> pid
            , userHandle   = handle
            , userExpire   = if status == Just Ephemeral
                                 then getExpiry <$> expires
                                 else Nothing
            , userTeam     = tid
            , userJournal  = journal
            }
        }

toUsers :: Locale -> [UserRow] -> [User]
toUsers defaultLocale = fmap (accountUser . toUserAccount defaultLocale)

toLocale :: Locale -> (Maybe Language, Maybe Country) -> Locale
toLocale _ (Just l, c) = Locale l c
toLocale l _           = l

toIdentity :: Activated -> Maybe Email -> Maybe Phone -> Maybe UserIdentity
toIdentity (Activated True) (Just e) (Just p) = Just $! FullIdentity e p
toIdentity (Activated True) (Just e) Nothing  = Just $! EmailIdentity e
toIdentity (Activated True) Nothing  (Just p) = Just $! PhoneIdentity p
toIdentity _    _        _                    = Nothing
