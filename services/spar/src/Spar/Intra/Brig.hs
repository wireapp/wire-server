{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

-- | Client functions for interacting with the Brig API.
module Spar.Intra.Brig
  ( toUserSSOId,
    fromUserSSOId,
    urefToExternalId,
    mkUserName,
    UserOrInvitation (JustUser, JustInvitation),
    userOrInvitationTeam,
    userOrInvitationId,
    userOrInvitationHandle,
    userOrInvitationManagedBy,
    userOrInvitationScimExternalId,
    getBrigUser,
    getBrigActualUser,
    getBrigInvitation,
    getBrigActualUserTeam,
    getBrigUsers,
    getBrigUserByHandle,
    getBrigUserByEmail,
    getBrigUserRichInfo,
    setBrigUserName,
    setBrigUserHandle,
    setBrigUserManagedBy,
    setBrigUserUserRef,
    setBrigUserRichInfo,
    checkHandleAvailable,
    deleteBrigUser,
    createBrigUserSaml,
    createBrigUserInvitation,
    updateEmail,
    getZUsrOwnedTeam,
    ensureReAuthorised,
    ssoLogin,
    parseResponse,
    MonadSparToBrig (..),
    getStatus,
    getStatusMaybe,
    setStatus,
    giveDefaultHandle,
  )
where

-- TODO: when creating user, we need to be able to provide more
-- master data (first name, last name, ...)

import Bilge
import Brig.Types.Intra
import Brig.Types.User
import Brig.Types.User.Auth (SsoLogin (..))
import Control.Lens
import Control.Monad.Except
import Data.Aeson (FromJSON, eitherDecode')
import Data.ByteString.Conversion
import Data.Coerce (coerce)
import Data.Handle (Handle (Handle, fromHandle))
import Data.Id (Id (Id), InvitationId, TeamId, UserId)
import Data.Ix
import Data.Misc (PlainTextPassword)
import Data.String.Conversions
import Imports
import Network.HTTP.Types.Method
import qualified Network.Wai.Utilities.Error as Wai
import qualified SAML2.WebSSO as SAML
import Spar.Error
import Spar.Intra.Galley as Galley (MonadSparToGalley, assertIsTeamOwner)
import Web.Cookie
import qualified Wire.API.Team.Invitation as Inv
import Wire.API.User
import Wire.API.User.RichInfo as RichInfo

----------------------------------------------------------------------

data UserOrInvitation
  = JustUser User
  | JustInvitation Inv.Invitation
  deriving (Eq, Show, Generic)

userOrInvitationTeam :: UserOrInvitation -> Maybe TeamId
userOrInvitationTeam = \case
  JustUser usr -> userTeam usr
  JustInvitation inv -> Just $ Inv.inTeam inv

userOrInvitationId :: UserOrInvitation -> UserId
userOrInvitationId = \case
  JustUser usr -> userId usr
  JustInvitation inv -> coerce $ Inv.inInvitation inv

userOrInvitationHandle :: UserOrInvitation -> Maybe Handle
userOrInvitationHandle = \case
  JustUser usr -> userHandle usr
  JustInvitation inv -> Inv.inInviteeHandle inv

userOrInvitationManagedBy :: UserOrInvitation -> ManagedBy
userOrInvitationManagedBy = \case
  JustUser usr -> userManagedBy usr
  JustInvitation inv -> Inv.inManagedBy inv

userOrInvitationScimExternalId :: UserOrInvitation -> Either String Text
userOrInvitationScimExternalId = \case
  JustUser usr -> case (userIdentity >=> ssoIdentity $ usr, userEmail usr) of
    (Just ssoid, _) -> case fromUserSSOId ssoid of
      Right (SAML.UserRef _ subj) -> maybe (Left "bad uref from brig") Right $ SAML.shortShowNameID subj
      Left err -> Left err
    (Nothing, Just email) -> Right $ fromEmail email
    (Nothing, Nothing) -> Left "brig user without external id"
  JustInvitation inv -> Right . fromEmail . Inv.inIdentity $ inv

toUserSSOId :: SAML.UserRef -> UserSSOId
toUserSSOId (SAML.UserRef tenant subject) =
  UserSSOId (cs $ SAML.encodeElem tenant) (cs $ SAML.encodeElem subject)

fromUserSSOId :: MonadError String m => UserSSOId -> m SAML.UserRef
fromUserSSOId (UserSSOId (cs -> tenant) (cs -> subject)) =
  case (SAML.decodeElem tenant, SAML.decodeElem subject) of
    (Right t, Right s) -> pure $ SAML.UserRef t s
    (Left msg, _) -> throwError msg
    (_, Left msg) -> throwError msg

urefToExternalId :: SAML.UserRef -> Maybe Text
urefToExternalId = SAML.shortShowNameID . view SAML.uidSubject

-- | Take a maybe text, construct a 'Name' from what we have in a scim user.  If the text
-- isn't present, use the saml subject (usually an email address).  If both are 'Nothing',
-- fail.
mkUserName :: Maybe Text -> Either Email SAML.UserRef -> Either String Name
mkUserName (Just n) _ = mkName n
mkUserName Nothing (Right uref) = mkName (SAML.unsafeShowNameID $ uref ^. SAML.uidSubject)
mkUserName Nothing (Left email) = mkName (fromEmail email)

parseResponse :: (FromJSON a, MonadError SparError m) => Response (Maybe LBS) -> m a
parseResponse resp = do
  bdy <- maybe (throwSpar SparNoBodyInBrigResponse) pure $ responseBody resp
  either (throwSpar . SparCouldNotParseBrigResponse . cs) pure $ eitherDecode' bdy

-- | Similar to 'Network.Wire.Client.API.Auth.tokenResponse', but easier: we just need to set the
-- cookie in the response, and the redirect will make the client negotiate a fresh auth token.
-- (This is the easiest way, since the login-request that we are in the middle of responding to here
-- is not from the wire client, but from a browser that is still processing a redirect from the
-- IdP.)
respToCookie :: (HasCallStack, MonadError SparError m) => Response (Maybe LBS) -> m SetCookie
respToCookie resp = do
  let crash = throwSpar SparCouldNotRetrieveCookie
  unless (statusCode resp == 200) crash
  maybe crash (pure . parseSetCookie) $ getHeader "Set-Cookie" resp

----------------------------------------------------------------------

class MonadError SparError m => MonadSparToBrig m where
  call :: (Request -> Request) -> m (Response (Maybe LBS))

instance MonadSparToBrig m => MonadSparToBrig (ReaderT r m) where
  call = lift . call

-- | Create a user on brig with saml credentials.
createBrigUserSaml ::
  (HasCallStack, MonadSparToBrig m) =>
  SAML.UserRef ->
  UserId ->
  TeamId ->
  Name ->
  ManagedBy ->
  m UserId
createBrigUserSaml uref (Id buid) teamid uname managedBy = do
  let newUser :: NewUser
      newUser =
        NewUser
          { newUserDisplayName = uname,
            newUserUUID = Just buid,
            newUserIdentity = Just $ SSOIdentity (toUserSSOId uref) Nothing Nothing,
            newUserPict = Nothing,
            newUserAssets = [],
            newUserAccentId = Nothing,
            newUserEmailCode = Nothing,
            newUserPhoneCode = Nothing,
            newUserOrigin = Just . NewUserOriginTeamUser . NewTeamMemberSSO $ teamid,
            newUserLabel = Nothing,
            newUserLocale = Nothing,
            newUserPassword = Nothing,
            newUserExpiresIn = Nothing,
            newUserManagedBy = Just managedBy
          }
  resp :: Response (Maybe LBS) <-
    call $
      method POST
        . path "/i/users"
        . json newUser
  if statusCode resp `elem` [200, 201]
    then userId . selfUser <$> parseResponse @SelfProfile resp
    else rethrow resp

-- | Create a user on brig via the traditional team invite procedure.
createBrigUserInvitation ::
  (HasCallStack, MonadSparToBrig m) =>
  Email ->
  TeamId ->
  Name ->
  Handle ->
  ManagedBy ->
  m UserId
createBrigUserInvitation email teamid uname uhandle managedBy = do
  let invreq = Inv.InvitationRequest Nothing Nothing (Just uname) (Just uhandle) email Nothing managedBy
  invresp <- call $ method POST . paths ["/i/teams", toByteString' teamid, "invitations"] . json invreq
  if statusCode invresp `notElem` [200, 201]
    then rethrow invresp
    else
      responseJsonMaybe invresp
        & maybe
          (throwSpar (SparBrigError "Could not parse invitation in response body"))
          (pure . coerce @InvitationId @UserId . Inv.inInvitation)

updateEmail :: (HasCallStack, MonadSparToBrig m) => UserId -> Email -> m ()
updateEmail buid email = do
  resp <-
    call $
      method PUT
        . path "/i/self/email"
        . header "Z-User" (toByteString' buid)
        . query [("validate", Just "true")]
        . json (EmailUpdate email)
  case statusCode resp of
    204 -> pure ()
    202 -> pure ()
    -- everything else is an error; if the response body still cannot be parsed as a
    -- Wai.Error, it's ok to crash with a 500 here, so we use the unsafe parser.
    _ -> throwError . SAML.CustomServant . waiToServant . responseJsonUnsafe $ resp

-- | Get a user; returns 'Nothing' if neither user nor invitation is found or if user and/or
-- invitation have been deleted.
getBrigUser :: (HasCallStack, MonadSparToBrig m) => TeamId -> UserId -> m (Maybe UserOrInvitation)
getBrigUser tid uid = do
  getBrigActualUser uid >>= \case
    Just u -> pure . Just . JustUser $ u
    Nothing ->
      getBrigInvitation tid (coerce uid) >>= \case
        Just i -> pure . Just . JustInvitation $ i
        Nothing -> pure Nothing

getBrigActualUser :: (HasCallStack, MonadSparToBrig m) => UserId -> m (Maybe User)
getBrigActualUser buid = do
  resp :: Response (Maybe LBS) <-
    call $
      method GET
        . path "/self"
        . header "Z-User" (toByteString' buid)
  case statusCode resp of
    200 -> do
      user <- selfUser <$> parseResponse @SelfProfile resp
      pure $
        if (userDeleted user)
          then Nothing
          else Just user
    404 -> pure Nothing
    _ -> throwSpar (SparBrigError "Could not retrieve user")

getBrigInvitation :: (HasCallStack, MonadSparToBrig m) => TeamId -> InvitationId -> m (Maybe Inv.Invitation)
getBrigInvitation tid = getBrigInvitationBy "by-id" (Just tid) . toByteString'

getBrigInvitationBy :: (HasCallStack, MonadSparToBrig m) => ByteString -> Maybe TeamId -> ByteString -> m (Maybe Inv.Invitation)
getBrigInvitationBy keyType mtid keyValue = do
  resp :: ResponseLBS <-
    call $
      method GET
        . paths
          ( ["/i/teams/"]
              <> (maybe [] ((: []) . toByteString') mtid)
              <> ["/invitations/", keyType, keyValue]
          )
  case (statusCode resp, responseJsonMaybe resp) of
    (200, Just inv) -> pure inv
    (404, _) -> pure Nothing
    _ -> rethrow resp

-- | Get a list of users; returns a shorter list if some 'UserId's come up empty (no errors).
--
-- TODO: implement an internal end-point on brig that makes this possible with one request.
-- TODO(arianvp): This endpoint exists!
-- TODO(fisx): not any more with the invitations also qualifying as scim users!
getBrigUsers :: (HasCallStack, MonadSparToBrig m) => TeamId -> [UserId] -> m [UserOrInvitation]
getBrigUsers tid = fmap catMaybes . mapM (getBrigUser tid)

-- | Get a user; returns 'Nothing' if the user was not found.
--
-- TODO: currently this is not used, but it might be useful later when/if
-- @hscim@ stops doing checks during user creation.
getBrigUserByHandle :: (HasCallStack, MonadSparToBrig m) => Handle -> m (Maybe UserOrInvitation)
getBrigUserByHandle handle = do
  getBrigActualUserByHandle handle >>= \case
    Just u -> pure . Just . JustUser $ u
    Nothing ->
      getBrigInvitationByHandle handle >>= \case
        Just i -> pure . Just . JustInvitation $ i
        Nothing -> pure Nothing

getBrigActualUserByHandle :: (HasCallStack, MonadSparToBrig m) => Handle -> m (Maybe User)
getBrigActualUserByHandle handle = do
  resp :: Response (Maybe LBS) <-
    call $
      method GET
        . path "/i/users"
        . queryItem "handles" (toByteString' handle)
  -- This returns [UserAccount]
  case statusCode resp of
    200 -> parse <$> parseResponse @[UserAccount] resp
    404 -> pure Nothing
    _ -> throwSpar (SparBrigError "Could not retrieve user")
  where
    parse :: [UserAccount] -> Maybe User
    parse (x : []) = Just $ accountUser x
    parse _ = Nothing

getBrigInvitationByHandle :: (HasCallStack, MonadSparToBrig m) => Handle -> m (Maybe Inv.Invitation)
getBrigInvitationByHandle = getBrigInvitationBy "handle" Nothing . toByteString'

getBrigUserByEmail :: (HasCallStack, MonadSparToBrig m) => Email -> m (Maybe UserOrInvitation)
getBrigUserByEmail email =
  getBrigActualUserByEmail email >>= \case
    Just u -> pure . Just . JustUser $ u
    Nothing ->
      getBrigInvitationByEmail email >>= \case
        Just i -> pure . Just . JustInvitation $ i
        Nothing -> pure Nothing

getBrigActualUserByEmail :: (HasCallStack, MonadSparToBrig m) => Email -> m (Maybe User)
getBrigActualUserByEmail email = do
  resp :: ResponseLBS <-
    call $
      method GET
        . path "/i/users"
        . queryItem "email" (toByteString' email)
  case statusCode resp of
    200 -> parse <$> parseResponse @[UserAccount] resp
    404 -> pure Nothing
    _ -> throwSpar (SparBrigError "Could not retrieve user")
  where
    parse :: [UserAccount] -> Maybe User
    parse (x : []) = Just $ accountUser x
    parse _ = Nothing

getBrigInvitationByEmail :: (HasCallStack, MonadSparToBrig m) => Email -> m (Maybe Inv.Invitation)
getBrigInvitationByEmail = getBrigInvitationBy "email" Nothing . toByteString'

-- | Set user' name.  Fails with status <500 if brig fails with <500, and with 500 if brig
-- fails with >= 500.
setBrigUserName :: (HasCallStack, MonadSparToBrig m) => TeamId -> UserId -> Name -> m ()
setBrigUserName tid buid (Name name) = do
  resp <-
    call $
      method PUT
        . paths ["/i/teams/", toByteString' tid, "/user/", toByteString' buid]
        . json (NameUpdate name)
  let sCode = statusCode resp
  if
      | sCode < 300 ->
        pure ()
      | inRange (400, 499) sCode ->
        throwSpar . SparBrigErrorWith (responseStatus resp) $ "set name failed"
      | otherwise ->
        throwSpar . SparBrigError . cs $ "set name failed with status " <> show sCode

-- | Set user's handle.  Fails with status <500 if brig fails with <500, and with 500 if brig fails
-- with >= 500.
--
-- This calls an internal end-point that also changes the value on invitations.  Invitations
-- are only hit in the corner case of a scim update between an invitation being created and
-- accepted.
--
-- NB: that this doesn't take a 'HandleUpdate', since we already construct a valid handle in
-- 'validateScimUser' to increase the odds that user creation doesn't fail half-way through
-- the many database write operations.
setBrigUserHandle :: (HasCallStack, MonadSparToBrig m) => TeamId -> UserId -> Handle {- not 'HandleUpdate'! -} -> m ()
setBrigUserHandle tid buid handle = do
  resp <-
    call $
      method PUT
        . paths ["/i/teams/", toByteString' tid, "/handle/", toByteString' buid]
        . json (HandleUpdate (fromHandle handle))
  case (statusCode resp, Wai.label <$> responseJsonMaybe @Wai.Error resp) of
    (200, Nothing) -> do
      pure ()
    _ -> do
      rethrow resp

-- | Set user's managedBy. Fails with status <500 if brig fails with <500, and with 500 if
-- brig fails with >= 500.
setBrigUserManagedBy :: (HasCallStack, MonadSparToBrig m) => TeamId -> UserId -> ManagedBy -> m ()
setBrigUserManagedBy tid buid managedBy = do
  resp <-
    call $
      method PUT
        . paths ["/i/teams/", toByteString' tid, "/managed-by/", toByteString' buid]
        . json (ManagedByUpdate managedBy)
  let sCode = statusCode resp
  if
      | sCode < 300 ->
        pure ()
      | inRange (400, 499) sCode ->
        throwSpar . SparBrigErrorWith (responseStatus resp) $ "set managedBy failed"
      | otherwise ->
        throwSpar . SparBrigError . cs $ "set managedBy failed with status " <> show sCode

-- | Take an existing brig user and assign it saml credentials.  If brig user does not exist,
-- try to find an invitation.  If the invitation exists, delete it, and create a new saml user
-- like when auto-provisioning.
setBrigUserUserRef :: (HasCallStack, MonadSparToBrig m) => UserId -> SAML.UserRef -> m ()
setBrigUserUserRef buid uref = do
  resp <-
    call $
      method PUT
        . paths ["i", "users", toByteString' buid, "sso-id"]
        . json (toUserSSOId uref)
  () <- error "62450eda-db39-11ea-a590-3b8694239e39 - implement what the haddocks promise.  or perhaps the logic should go elsewhere, outside of this module?"
  let sCode = statusCode resp
  if
      | sCode < 300 ->
        pure ()
      | inRange (400, 499) sCode ->
        throwSpar . SparBrigErrorWith (responseStatus resp) $ "set UserSSOId failed"
      | otherwise ->
        throwSpar . SparBrigError . cs $ "set UserSSOId failed with status " <> show sCode

-- | Set user's richInfo. Fails with status <500 if brig fails with <500, and with 500 if
-- brig fails with >= 500.
setBrigUserRichInfo :: (HasCallStack, MonadSparToBrig m) => UserId -> RichInfo -> m ()
setBrigUserRichInfo buid richInfo = do
  resp <-
    call $
      method PUT
        . paths ["i", "users", toByteString' buid, "rich-info"]
        . json (RichInfoUpdate $ unRichInfo richInfo)
  let sCode = statusCode resp
  if
      | sCode < 300 ->
        pure ()
      | inRange (400, 499) sCode ->
        throwSpar . SparBrigErrorWith (responseStatus resp) $ "set richInfo failed"
      | otherwise ->
        throwSpar . SparBrigError . cs $ "set richInfo failed with status " <> show sCode

getBrigUserRichInfo :: (HasCallStack, MonadSparToBrig m) => UserId -> m RichInfo
getBrigUserRichInfo buid = do
  RichInfo.RichInfo <$> do
    resp <-
      call $
        method GET
          . paths ["i", "users", toByteString' buid, "rich-info"]
    case statusCode resp of
      200 -> parseResponse resp
      _ -> throwSpar (SparBrigErrorWith (responseStatus resp) "Could not retrieve rich info")

checkHandleAvailable :: (HasCallStack, MonadSparToBrig m) => Handle -> m Bool
checkHandleAvailable hnd = do
  resp <-
    call $
      method HEAD
        . paths ["/i/users/handles", toByteString' hnd]
  let sCode = statusCode resp
  if
      | sCode == 200 -> -- handle exists
        pure False
      | sCode == 404 -> -- handle not found
        pure True
      | sCode < 500 ->
        throwSpar . SparBrigErrorWith (responseStatus resp) $ "check handle failed"
      | otherwise ->
        throwSpar . SparBrigError . cs $ "check handle failed with status " <> show sCode

-- | Call brig to delete a user or an invitation
deleteBrigUser :: (HasCallStack, MonadSparToBrig m, MonadIO m) => TeamId -> UserId -> m ()
deleteBrigUser tid buid = do
  deleteBrigActualUser buid
  deleteBrigInvitation tid (coerce @UserId @InvitationId buid)

deleteBrigActualUser :: (HasCallStack, MonadSparToBrig m, MonadIO m) => UserId -> m ()
deleteBrigActualUser buid = do
  resp :: Response (Maybe LBS) <-
    call $
      method DELETE
        . paths ["/i/users", toByteString' buid]
  let sCode = statusCode resp
  if
      | sCode < 300 -> pure ()
      | inRange (400, 499) sCode ->
        throwSpar $ SparBrigErrorWith (responseStatus resp) "failed to delete user"
      | otherwise ->
        throwSpar $ SparBrigError ("delete user failed with status " <> cs (show sCode))

deleteBrigInvitation :: (HasCallStack, MonadSparToBrig m, MonadIO m) => TeamId -> InvitationId -> m ()
deleteBrigInvitation tid invid = do
  resp <- call $ method DELETE . paths ["/i/teams", toByteString' tid, "invitations", toByteString' invid]
  unless (statusCode resp == 200) $ do
    rethrow resp

-- | Check that a user id exists on brig and has a team id.
getBrigActualUserTeam :: (HasCallStack, MonadSparToBrig m) => UserId -> m (Maybe TeamId)
getBrigActualUserTeam buid = do
  usr <- getBrigActualUser buid
  pure $ userTeam =<< usr

-- | Get the team that the user is an owner of.
--
-- Called by post handler, and by 'authorizeIdP'.
getZUsrOwnedTeam ::
  (HasCallStack, SAML.SP m, MonadSparToBrig m, MonadSparToGalley m) =>
  Maybe UserId ->
  m TeamId
getZUsrOwnedTeam Nothing = throwSpar SparMissingZUsr
getZUsrOwnedTeam (Just uid) = do
  usr <- getBrigActualUser uid
  case userTeam =<< usr of
    Nothing -> throwSpar SparNotInTeam
    Just teamid -> teamid <$ Galley.assertIsTeamOwner teamid uid

-- | Verify user's password (needed for certain powerful operations).
ensureReAuthorised ::
  (HasCallStack, MonadSparToBrig m) =>
  Maybe UserId ->
  Maybe PlainTextPassword ->
  m ()
ensureReAuthorised Nothing _ = throwSpar SparMissingZUsr
ensureReAuthorised (Just uid) secret = do
  resp <-
    call $
      method GET
        . paths ["/i/users", toByteString' uid, "reauthenticate"]
        . json (ReAuthUser secret)
  let sCode = statusCode resp
  if
      | sCode == 200 ->
        pure ()
      | sCode == 403 ->
        throwSpar SparReAuthRequired
      | inRange (400, 499) sCode ->
        throwSpar . SparBrigErrorWith (responseStatus resp) $ "reauthentication failed"
      | otherwise ->
        throwSpar . SparBrigError . cs $ "reauthentication failed with status " <> show sCode

-- | Get persistent cookie from brig and redirect user past login process.
--
-- If brig responds with status >=400;<500, return Nothing.  Otherwise, crash (500).
ssoLogin ::
  (HasCallStack, SAML.HasConfig m, MonadSparToBrig m) =>
  UserId ->
  m (Maybe SetCookie)
ssoLogin buid = do
  resp :: Response (Maybe LBS) <-
    call $
      method POST
        . path "/i/sso-login"
        . json (SsoLogin buid Nothing)
        . queryItem "persist" "true"
  let sCode = statusCode resp
  if
      | sCode < 300 ->
        Just <$> respToCookie resp
      | inRange (400, 499) sCode ->
        pure Nothing
      | otherwise ->
        throwSpar . SparBrigError . cs $ "sso-login failed with status " <> show sCode

getStatus' :: (HasCallStack, MonadSparToBrig m) => UserId -> m ResponseLBS
getStatus' uid = call $ method GET . paths ["/i/users", toByteString' uid, "status"]

getStatus :: (HasCallStack, MonadSparToBrig m) => UserId -> m AccountStatus
getStatus uid = do
  resp <- getStatus' uid
  case statusCode resp of
    200 -> fromAccountStatusResp <$> parseResponse @AccountStatusResp resp
    _ -> rethrow resp

getStatusMaybe :: (HasCallStack, MonadSparToBrig m) => UserId -> m (Maybe AccountStatus)
getStatusMaybe uid = do
  resp <- getStatus' uid
  case statusCode resp of
    200 -> Just . fromAccountStatusResp <$> parseResponse @AccountStatusResp resp
    404 -> pure Nothing
    _ -> rethrow resp

setStatus :: (HasCallStack, MonadSparToBrig m) => UserId -> AccountStatus -> m ()
setStatus uid status = do
  resp <-
    call $
      method PUT
        . paths ["/i/users", toByteString' uid, "status"]
        . json (AccountStatusUpdate status)
  case statusCode resp of
    200 -> pure ()
    _ -> throwSpar (SparBrigErrorWith (responseStatus resp) "Could not set status")

-- | If the user has no 'Handle', set it to its 'UserId' and update the user in brig.
-- Return the handle the user now has (the old one if it existed, the newly created one
-- otherwise).
--
-- RATIONALE: Finding the handle can fail for users that have been created without scim, and
-- have stopped the onboarding process at the point where they are asked by the client to
-- enter a handle.
--
-- We make up a handle in this case, and the scim peer can find the user, see that the handle
-- is not the one it expects, and update it.
--
-- We cannot simply respond with 404 in this case, because the user exists.  404 would suggest
-- do the scim peer that it should post the user to create it, but that would create a new
-- user instead of finding the old that should be put under scim control.
giveDefaultHandle :: (HasCallStack, MonadSparToBrig m) => TeamId -> UserOrInvitation -> m Handle
giveDefaultHandle tid usr = case userOrInvitationHandle usr of
  Just handle -> pure handle
  Nothing -> do
    let handle = Handle . cs . toByteString' $ uid
        uid = userOrInvitationId usr
    setBrigUserHandle tid uid handle
    pure handle

-- | If a call to brig fails, we often just want to respond with whatever brig said.
--
-- TODO: https://github.com/zinfra/backend-issues/issues/1613 (also, consistently check error
-- labels everywhere like in 'setBrigActualUserHandle')
--
-- FUTUREWORK: with servant, there will be a way for the type checker to confirm that we
-- handle all exceptions that brig can legally throw!
rethrow :: ResponseLBS -> (HasCallStack, MonadSparToBrig m) => m a
rethrow resp = throwError err
  where
    err :: SparError
    err =
      responseJsonMaybe resp
        & maybe
          (SAML.CustomError . SparBrigError . cs . fromMaybe "" . responseBody $ resp)
          (SAML.CustomServant . waiToServant)
