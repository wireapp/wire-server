{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

-- | Spar talking to Brig.
module Spar.Intra.Brig where

-- TODO: when creating user, we need to be able to provide more
-- master data (first name, last name, ...)

import Bilge
import Control.Monad.Except
import Data.Aeson (eitherDecode')
import Data.Aeson (FromJSON)
import Data.ByteString.Conversion
import Data.Id (Id(Id), UserId, TeamId)
import Data.String.Conversions
import GHC.Stack
import Lens.Micro
import Network.HTTP.Types.Method
import Servant hiding (URI)
import URI.ByteString
import Web.Cookie

import qualified Brig.Types.User as Brig
import qualified Brig.Types.User.Auth as Brig
import qualified SAML2.WebSSO as SAML


----------------------------------------------------------------------

toUserSSOId :: SAML.UserRef -> Brig.UserSSOId
toUserSSOId (SAML.UserRef tenant subject) =
  Brig.UserSSOId (cs $ SAML.encodeElem tenant) (cs $ SAML.encodeElem subject)

fromUserSSOId :: MonadError String m => Brig.UserSSOId -> m SAML.UserRef
fromUserSSOId (Brig.UserSSOId (cs -> tenant) (cs -> subject)) =
  case (SAML.decodeElem tenant, SAML.decodeElem subject) of
    (Right t, Right s) -> pure $ SAML.UserRef t s
    (Left msg, _)      -> throwError msg
    (_, Left msg)      -> throwError msg


parseResponse :: (FromJSON a, MonadError ServantErr m) => Response (Maybe LBS) -> m a
parseResponse resp = do
    bdy <- maybe (throwError err500) pure $ responseBody resp
    either (const $ throwError err500) pure $ eitherDecode' bdy

-- | Similar to 'Network.Wire.Client.API.Auth.tokenResponse', but easier: we just need to set the
-- cookie in the response, and the redirect will make the client negotiate a fresh auth token.
-- (This is the easiest way, since the login-request that we are in the middle of responding to here
-- is not from the wire client, but from a browser that is still processing a redirect from the
-- IdP.)
respToCookie :: (HasCallStack, MonadError ServantErr m) => Response (Maybe LBS) -> m SetCookie
respToCookie resp = do
  let crash msg = throwError err500 { errBody = msg }
  unless (statusCode resp == 200) $ crash "bad status code"
  maybe (crash "no cookie") (pure . parseSetCookie) $ getHeader "Set-Cookie" resp


----------------------------------------------------------------------

class Monad m => MonadSparToBrig m where
  call :: (Request -> Request) -> m (Response (Maybe LBS))


-- | Create a user on brig.
createUser :: (HasCallStack, MonadError ServantErr m, MonadSparToBrig m) => SAML.UserRef -> UserId -> TeamId -> m UserId
createUser suid (Id buid) teamid = do
  let newUser :: Brig.NewUser
      newUser = Brig.NewUser
        { Brig.newUserName           = Brig.Name . cs . SAML.encodeElem $ suid ^. SAML.uidSubject
        , Brig.newUserUUID           = Just buid
        , Brig.newUserIdentity       = Just $ Brig.SSOIdentity (toUserSSOId suid) Nothing Nothing
        , Brig.newUserPict           = Nothing
        , Brig.newUserAssets         = []
        , Brig.newUserAccentId       = Nothing
        , Brig.newUserEmailCode      = Nothing
        , Brig.newUserPhoneCode      = Nothing
        , Brig.newUserOrigin         = Just . Brig.NewUserOriginTeamUser . Brig.NewTeamMemberSSO $ teamid
        , Brig.newUserLabel          = Nothing
        , Brig.newUserLocale         = Nothing
        , Brig.newUserPassword       = Nothing
        , Brig.newUserExpiresIn      = Nothing
        }

  resp :: Response (Maybe LBS) <- call
    $ method POST
    . path "/i/users"
    . json newUser
    . expect2xx
  parseResponse resp


getUser :: (HasCallStack, MonadError ServantErr m, MonadSparToBrig m) => UserId -> m (Maybe Brig.User)
getUser buid = do
  resp :: Response (Maybe LBS) <- call
    $ method GET
    . path "/self"
    . header "Z-User" (toByteString' buid)

  if statusCode resp /= 200
    then pure Nothing
    else Just . Brig.selfUser <$> parseResponse @Brig.SelfProfile resp


-- | Check that a user locally created on spar exists on brig and has a team id.
confirmUserId :: (HasCallStack, MonadError ServantErr m, MonadSparToBrig m) => UserId -> m (Maybe UserId)
confirmUserId buid = do
  usr <- getUser buid
  maybe (pure Nothing) (const . pure . Just $ buid) (Brig.userTeam =<< usr)


-- | Get session token from brig and redirect user past login process.
forwardBrigLogin :: (HasCallStack, MonadError ServantErr m, SAML.HasConfig m, MonadSparToBrig m)
                 => UserId -> m (SetCookie, URI)
forwardBrigLogin buid = do
  resp :: Response (Maybe LBS) <- call
    $ method POST
    . path "/i/sso-login"
    . json (Brig.SsoLogin buid Nothing)
    . queryItem "persistent" "true"
    . expect2xx

  (,) <$> respToCookie resp <*> SAML.getLandingURI
