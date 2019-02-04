{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

-- | Spar talking to Brig.
module Spar.Intra.Brig where

-- TODO: when creating user, we need to be able to provide more
-- master data (first name, last name, ...)

import Imports
import Bilge
import Brig.Types.Intra
import Brig.Types.User
import Brig.Types.User.Auth (SsoLogin(..))
import Control.Lens
import Control.Monad.Except
import Data.Aeson (FromJSON, eitherDecode')
import Data.ByteString.Conversion
import Data.Id (Id(Id), UserId, TeamId)
import Data.Ix
import Data.Range
import Data.String.Conversions
import Network.HTTP.Types.Method
import Spar.Error
import Web.Cookie

import qualified Data.Text as Text
import qualified SAML2.WebSSO as SAML


----------------------------------------------------------------------

toUserSSOId :: SAML.UserRef -> UserSSOId
toUserSSOId (SAML.UserRef tenant subject) =
  UserSSOId (cs $ SAML.encodeElem tenant) (cs $ SAML.encodeElem subject)

fromUserSSOId :: MonadError String m => UserSSOId -> m SAML.UserRef
fromUserSSOId (UserSSOId (cs -> tenant) (cs -> subject)) =
  case (SAML.decodeElem tenant, SAML.decodeElem subject) of
    (Right t, Right s) -> pure $ SAML.UserRef t s
    (Left msg, _)      -> throwError msg
    (_, Left msg)      -> throwError msg


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


-- | Create a user on brig.
createUser
  :: (HasCallStack, MonadSparToBrig m)
  => SAML.UserRef    -- ^ SSO identity
  -> UserId
  -> TeamId
  -> Maybe Name      -- ^ User name (if 'Nothing', the subject ID will be used)
  -> ManagedBy       -- ^ Who should have control over the user
  -> m UserId
createUser suid (Id buid) teamid mbName managedBy = do
  uname :: Name <- case mbName of
    Just n -> pure n
    Nothing -> do
      let subject = suid ^. SAML.uidSubject
          badName = throwSpar . SparBadUserName $ SAML.encodeElem subject
          mkName  = Name . fromRange <$>
                    (fmap (Text.take 128) . SAML.shortShowNameID >=> checked @ST @1 @128) subject
      maybe badName pure mkName

  let newUser :: NewUser
      newUser = NewUser
        { newUserName           = uname
        , newUserUUID           = Just buid
        , newUserIdentity       = Just $ SSOIdentity (toUserSSOId suid) Nothing Nothing
        , newUserPict           = Nothing
        , newUserAssets         = []
        , newUserAccentId       = Nothing
        , newUserEmailCode      = Nothing
        , newUserPhoneCode      = Nothing
        , newUserOrigin         = Just . NewUserOriginTeamUser . NewTeamMemberSSO $ teamid
        , newUserLabel          = Nothing
        , newUserLocale         = Nothing
        , newUserPassword       = Nothing
        , newUserExpiresIn      = Nothing
        , newUserManagedBy      = Just managedBy
        }

  resp :: Response (Maybe LBS) <- call
    $ method POST
    . path "/i/users"
    . json newUser
  if | statusCode resp < 300
       -> userId . selfUser <$> parseResponse @SelfProfile resp
     | inRange (400, 499) (statusCode resp)
       -> throwSpar . SparBrigErrorWith (responseStatus resp) $ "create user failed"
     | otherwise
       -> throwSpar . SparBrigError . cs $ "create user failed with status " <> show (statusCode resp)


-- | Get a user; returns 'Nothing' if the user was not found or has been deleted.
getUser :: (HasCallStack, MonadSparToBrig m) => UserId -> m (Maybe User)
getUser buid = do
  resp :: Response (Maybe LBS) <- call
    $ method GET
    . path "/self"
    . header "Z-User" (toByteString' buid)
  case statusCode resp of
    200 -> do
      user <- selfUser <$> parseResponse @SelfProfile resp
      pure $ if (userDeleted user)
        then Nothing
        else Just user
    404 -> pure Nothing
    _   -> throwSpar (SparBrigError "Could not retrieve user")

-- | Get a list of users; returns a shorter list if some 'UserId's come up empty (no errors).
--
-- TODO: implement an internal end-point on brig that makes this possible with one request.
getUsers :: (HasCallStack, MonadSparToBrig m) => [UserId] -> m [User]
getUsers = fmap catMaybes . mapM getUser

-- | Get a user; returns 'Nothing' if the user was not found.
--
-- TODO: currently this is not used, but it might be useful later when/if
-- @hscim@ stops doing checks during user creation.
getUserByHandle :: (HasCallStack, MonadSparToBrig m) => Handle -> m (Maybe User)
getUserByHandle handle = do
  resp :: Response (Maybe LBS) <- call
    $ method GET
    . path "/i/users"
    . queryItem "handles" (toByteString' handle)
  -- This returns [UserAccount]
  case statusCode resp of
    200 -> parse <$> parseResponse @[UserAccount] resp
    404 -> pure Nothing
    _   -> throwSpar (SparBrigError "Could not retrieve user")
 where
  parse :: [UserAccount] -> Maybe User
  parse (x:[]) = Just $ accountUser x
  parse _      = Nothing -- TODO: What if more accounts get returned?

-- | Set user' name.  Fails with status <500 if brig fails with <500, and with 500 if brig
-- fails with >= 500.
setName :: (HasCallStack, MonadSparToBrig m) => UserId -> Name -> m ()
setName buid name = do
  resp <- call
    $ method PUT
    . path "/self"
    . header "Z-User" (toByteString' buid)
    . header "Z-Connection" ""
    . json UserUpdate
               { uupName = Just name
               , uupPict = Nothing
               , uupAssets = Nothing
               , uupAccentId = Nothing
               }
  if | statusCode resp < 300
       -> pure ()
     | inRange (400, 499) (statusCode resp)
       -> throwSpar . SparBrigErrorWith (responseStatus resp) $ "set name failed"
     | otherwise
       -> throwSpar . SparBrigError . cs $ "set name failed with status " <> show (statusCode resp)

-- | Set user's handle.  Fails with status <500 if brig fails with <500, and with 500 if brig fails
-- with >= 500.
setHandle :: (HasCallStack, MonadSparToBrig m) => UserId -> Handle -> m ()
setHandle buid (Handle handle) = do
  resp <- call
    $ method PUT
    . path "/self/handle"
    . header "Z-User" (toByteString' buid)
    . header "Z-Connection" ""
    . json (HandleUpdate handle)
  if | statusCode resp < 300
       -> pure ()
     | inRange (400, 499) (statusCode resp)
       -> throwSpar . SparBrigErrorWith (responseStatus resp) $ "set handle failed"
     | otherwise
       -> throwSpar . SparBrigError . cs $ "set handle failed with status " <> show (statusCode resp)

-- | Set user's managedBy. Fails with status <500 if brig fails with <500, and with 500 if
-- brig fails with >= 500.
setManagedBy :: (HasCallStack, MonadSparToBrig m) => UserId -> ManagedBy -> m ()
setManagedBy buid managedBy = do
  resp <- call
    $ method PUT
    . paths ["i", "users", toByteString' buid, "managed-by"]
    . json (ManagedByUpdate managedBy)
  if | statusCode resp < 300
       -> pure ()
     | inRange (400, 499) (statusCode resp)
       -> throwSpar . SparBrigErrorWith (responseStatus resp) $ "set managedBy failed"
     | otherwise
       -> throwSpar . SparBrigError . cs $ "set managedBy failed with status " <> show (statusCode resp)

-- | This works under the assumption that the user must exist on brig.  If it does not, brig
-- responds with 404 and this function returns 'False'.
bindUser :: (HasCallStack, MonadSparToBrig m) => UserId -> SAML.UserRef -> m Bool
bindUser uid (toUserSSOId -> ussoid) = do
  resp <- call $ method PUT
    . paths ["/i/users", toByteString' uid, "sso-id"]
    . json ussoid
  pure $ Bilge.statusCode resp < 300

-- | Check that a user id exists on brig and has a team id.
isTeamUser :: (HasCallStack, MonadSparToBrig m) => UserId -> m Bool
isTeamUser buid = isJust <$> getUserTeam buid

-- | Check that a user id exists on brig and has a team id.
getUserTeam :: (HasCallStack, MonadSparToBrig m) => UserId -> m (Maybe TeamId)
getUserTeam buid = do
  usr <- getUser buid
  pure $ userTeam =<< usr


-- | If user is not in team, throw 'SparNotInTeam'; if user is in team but not owner, throw
-- 'SparNotTeamOwner'; otherwise, return.
assertIsTeamOwner :: (HasCallStack, MonadSparToBrig m) => UserId -> TeamId -> m ()
assertIsTeamOwner buid tid = do
  self <- maybe (throwSpar SparNotInTeam) pure =<< getUser buid
  when (userTeam self /= Just tid) $ (throwSpar SparNotInTeam)
  resp :: Response (Maybe LBS) <- call
    $ method GET
    . paths ["i", "users", toByteString' buid, "is-team-owner", toByteString' tid]
  when (statusCode resp >= 400) $ throwSpar SparNotTeamOwner

-- | Get the team that the user is an owner of.
--
-- Called by post handler, and by 'authorizeIdP'.
getZUsrOwnedTeam :: (HasCallStack, SAML.SP m, MonadSparToBrig m)
            => Maybe UserId -> m TeamId
getZUsrOwnedTeam Nothing = throwSpar SparMissingZUsr
getZUsrOwnedTeam (Just uid) = do
  usr <- getUser uid
  case userTeam =<< usr of
    Nothing -> throwSpar SparNotInTeam
    Just teamid -> teamid <$ assertIsTeamOwner uid teamid


-- | Get persistent cookie from brig and redirect user past login process.
--
-- If brig responds with status >=400;<500, return Nothing.  Otherwise, crash (500).
ssoLogin :: (HasCallStack, SAML.HasConfig m, MonadSparToBrig m)
         => UserId -> m (Maybe SetCookie)
ssoLogin buid = do
  resp :: Response (Maybe LBS) <- call
    $ method POST
    . path "/i/sso-login"
    . json (SsoLogin buid Nothing)
    . queryItem "persist" "true"
  if | statusCode resp < 300
       -> Just <$> respToCookie resp
     | inRange (400, 499) (statusCode resp)
       -> pure Nothing
     | otherwise
       -> throwSpar . SparBrigError . cs $ "sso-login failed with status " <> show (statusCode resp)
