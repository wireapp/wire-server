module Wire.UserStore.Cassandra (interpretUserStoreCassandra) where

import Cassandra
import Data.Handle
import Data.Id
import Database.CQL.Protocol
import Imports
import Polysemy
import Polysemy.Embed
import Polysemy.Error
import Wire.API.User hiding (DeleteUser)
import Wire.StoredUser
import Wire.UserStore
import Wire.UserStore.Unique

interpretUserStoreCassandra :: (Member (Embed IO) r) => ClientState -> InterpreterFor UserStore r
interpretUserStoreCassandra casClient =
  interpret $
    runEmbedded (runClient casClient) . \case
      GetUser uid -> getUserImpl uid
      UpdateUser uid update -> embed $ updateUserImpl uid update
      UpdateUserHandleEither uid update -> embed $ updateUserHandleEitherImpl uid update
      DeleteUser user -> embed $ deleteUserImpl user
      LookupHandle hdl -> embed $ lookupHandleImpl LocalQuorum hdl
      GlimpseHandle hdl -> embed $ lookupHandleImpl One hdl
      LookupStatus uid -> embed $ lookupStatusImpl uid
      IsActivated uid -> embed $ isActivatedImpl uid
      LookupLocale uid -> embed $ lookupLocaleImpl uid
      LookupAccounts uids -> embed $ lookupAccountsImpl uids

lookupAccountsImpl :: [UserId] -> Client [StoredUser]
lookupAccountsImpl usrs =
  map asRecord
    <$> retry x1 (query accountsSelect (params LocalQuorum (Identity usrs)))

getUserImpl :: (Member (Embed Client) r) => UserId -> Sem r (Maybe StoredUser)
getUserImpl uid = embed $ do
  mUserTuple <- retry x1 $ query1 selectUser (params LocalQuorum (Identity uid))
  pure $ asRecord <$> mUserTuple

updateUserImpl :: UserId -> StoredUserUpdate -> Client ()
updateUserImpl uid update =
  retry x5 $ batch do
    -- PERFORMANCE(fisx): if a user changes 4 attributes with one request, the database will
    -- be hit with one request for each attribute.  this is probably fine, since this
    -- operation is not heavily used.  (also, the four operations are batched, which may or
    -- may not help.)
    setType BatchLogged
    setConsistency LocalQuorum
    for_ update.name \n -> addPrepQuery userDisplayNameUpdate (n, uid)
    for_ update.textStatus \s -> addPrepQuery userTextStatusUpdate (s, uid)
    for_ update.pict \p -> addPrepQuery userPictUpdate (p, uid)
    for_ update.assets \a -> addPrepQuery userAssetsUpdate (a, uid)
    for_ update.locale \a -> addPrepQuery userLocaleUpdate (a.lLanguage, a.lCountry, uid)
    for_ update.accentId \c -> addPrepQuery userAccentIdUpdate (c, uid)
    for_ update.supportedProtocols \a -> addPrepQuery userSupportedProtocolsUpdate (a, uid)

updateUserHandleEitherImpl :: UserId -> StoredUserHandleUpdate -> Client (Either StoredUserUpdateError ())
updateUserHandleEitherImpl uid update =
  runM $ runError do
    claimed <- embed $ claimHandleImpl uid update.old update.new
    unless claimed $ throw StoredUserUpdateHandleExists

-- | Claim a new handle for an existing 'User': validate it, and in case of success, assign it
-- to user and mark it as taken.
claimHandleImpl :: UserId -> Maybe Handle -> Handle -> Client Bool
claimHandleImpl uid oldHandle newHandle =
  isJust <$> do
    owner <- lookupHandleImpl LocalQuorum newHandle
    case owner of
      Just uid' | uid /= uid' -> pure Nothing
      _ -> do
        let key = "@" <> fromHandle newHandle
        withClaim uid key (30 # Minute) $
          do
            -- Record ownership
            retry x5 $ write handleInsert (params LocalQuorum (newHandle, uid))
            -- Update profile
            result <- updateHandle uid newHandle
            -- Free old handle (if it changed)
            for_ (mfilter (/= newHandle) oldHandle) $
              freeHandleImpl uid
            pure result
  where
    updateHandle :: UserId -> Handle -> Client ()
    updateHandle u h = retry x5 $ write userHandleUpdate (params LocalQuorum (h, u))

-- | Free a 'Handle', making it available to be claimed again.
freeHandleImpl :: UserId -> Handle -> Client ()
freeHandleImpl uid h = do
  mbHandleUid <- lookupHandleImpl LocalQuorum h
  case mbHandleUid of
    Just handleUid | handleUid == uid -> do
      retry x5 $ write handleDelete (params LocalQuorum (Identity h))
      let key = "@" <> fromHandle h
      deleteClaim uid key (30 # Minute)
    _ -> pure () -- this shouldn't happen, the call side should always check that `h` and `uid` belong to the same account.

-- | Sending an empty 'Handle' here causes C* to throw "Key may not be empty"
-- error.
lookupHandleImpl :: Consistency -> Handle -> Client (Maybe UserId)
lookupHandleImpl consistencyLevel h = do
  (runIdentity =<<)
    <$> retry x1 (query1 handleSelect (params consistencyLevel (Identity h)))

deleteUserImpl :: User -> Client ()
deleteUserImpl user = do
  for_ (userHandle user) \h ->
    freeHandleImpl (userId user) h
  retry x5 $
    write
      updateUserToTombstone
      ( params
          LocalQuorum
          (Deleted, Name "default", defaultAccentId, noPict, [], userId user)
      )

lookupStatusImpl :: UserId -> Client (Maybe AccountStatus)
lookupStatusImpl u =
  (runIdentity =<<)
    <$> retry x1 (query1 statusSelect (params LocalQuorum (Identity u)))

isActivatedImpl :: UserId -> Client Bool
isActivatedImpl uid =
  (== Just (Identity True))
    <$> retry x1 (query1 activatedSelect (params LocalQuorum (Identity uid)))

lookupLocaleImpl :: UserId -> Client (Maybe (Maybe Language, Maybe Country))
lookupLocaleImpl u = do
  retry x1 (query1 localeSelect (params LocalQuorum (Identity u)))

--------------------------------------------------------------------------------
-- Queries

selectUser :: PrepQuery R (Identity UserId) (TupleType StoredUser)
selectUser =
  "SELECT id, name, text_status, picture, email, sso_id, accent_id, assets, \
  \activated, status, expires, language, country, provider, service, \
  \handle, team, managed_by, supported_protocols \
  \FROM user where id = ?"

userDisplayNameUpdate :: PrepQuery W (Name, UserId) ()
userDisplayNameUpdate = "UPDATE user SET name = ? WHERE id = ?"

userTextStatusUpdate :: PrepQuery W (TextStatus, UserId) ()
userTextStatusUpdate = "UPDATE user SET text_status = ? WHERE id = ?"

userPictUpdate :: PrepQuery W (Pict, UserId) ()
userPictUpdate = "UPDATE user SET picture = ? WHERE id = ?"

userAssetsUpdate :: PrepQuery W ([Asset], UserId) ()
userAssetsUpdate = "UPDATE user SET assets = ? WHERE id = ?"

userAccentIdUpdate :: PrepQuery W (ColourId, UserId) ()
userAccentIdUpdate = "UPDATE user SET accent_id = ? WHERE id = ?"

userLocaleUpdate :: PrepQuery W (Language, Maybe Country, UserId) ()
userLocaleUpdate = "UPDATE user SET language = ?, country = ? WHERE id = ?"

userSupportedProtocolsUpdate :: PrepQuery W (Imports.Set BaseProtocolTag, UserId) ()
userSupportedProtocolsUpdate = "UPDATE user SET supported_protocols = ? WHERE id = ?"

handleInsert :: PrepQuery W (Handle, UserId) ()
handleInsert = "INSERT INTO user_handle (handle, user) VALUES (?, ?)"

handleSelect :: PrepQuery R (Identity Handle) (Identity (Maybe UserId))
handleSelect = "SELECT user FROM user_handle WHERE handle = ?"

handleDelete :: PrepQuery W (Identity Handle) ()
handleDelete = "DELETE FROM user_handle WHERE handle = ?"

userHandleUpdate :: PrepQuery W (Handle, UserId) ()
userHandleUpdate = "UPDATE user SET handle = ? WHERE id = ?"

updateUserToTombstone :: PrepQuery W (AccountStatus, Name, ColourId, Pict, [Asset], UserId) ()
updateUserToTombstone =
  "UPDATE user SET status = ?, name = ?,\
  \ accent_id = ?, picture = ?, assets = ?, handle = null, country = null,\
  \ language = null, email = null, sso_id = null WHERE id = ?"

statusSelect :: PrepQuery R (Identity UserId) (Identity (Maybe AccountStatus))
statusSelect = "SELECT status FROM user WHERE id = ?"

activatedSelect :: PrepQuery R (Identity UserId) (Identity Bool)
activatedSelect = "SELECT activated FROM user WHERE id = ?"

localeSelect :: PrepQuery R (Identity UserId) (Maybe Language, Maybe Country)
localeSelect = "SELECT language, country FROM user WHERE id = ?"

accountsSelect :: PrepQuery R (Identity [UserId]) (TupleType StoredUser)
accountsSelect =
  [sql|
  SELECT id, name, text_status, picture, email, sso_id, accent_id, assets,
  activated, status, expires, language, country, provider,
  service, handle, team, managed_by, supported_protocols
  FROM user WHERE id IN ?
  |]
