{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

-- | Spar talking to Brig.
module Spar.Intra.Brig where

-- TODO: when creating user, we need to be able to provide more
-- master data (first name, last name, ...)

import Bilge
import Control.Lens
import Control.Monad.Except
import Data.Aeson (eitherDecode')
import Data.Id (UserId)
import Data.String.Conversions
import GHC.Stack
import Network.HTTP.Types.Method
import URI.ByteString
import Servant hiding (URI)
import Web.Cookie

import qualified Network.HTTP.Types.Header as HTTP
import qualified Data.ByteString.Builder as LBS
import qualified Brig.Types.User as Brig
import qualified Brig.Types.User.Auth as Brig
import qualified SAML2.WebSSO as SAML


----------------------------------------------------------------------

toUserSSOId :: SAML.UserId -> Brig.UserSSOId
toUserSSOId (SAML.UserId tenant subject) =
  Brig.UserSSOId (cs $ SAML.encodeElem tenant) (cs $ SAML.encodeElem subject)

fromUserSSOId :: MonadError String m => Brig.UserSSOId -> m SAML.UserId
fromUserSSOId (Brig.UserSSOId (cs -> tenant) (cs -> subject)) =
  case (SAML.decodeElem tenant, SAML.decodeElem subject) of
    (Right t, Right s) -> pure $ SAML.UserId t s
    (Left msg, _)      -> throwError msg
    (_, Left msg)      -> throwError msg


parseResponse :: MonadError ServantErr m => Response (Maybe LBS) -> m UserId
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
createUser :: (HasCallStack, MonadError ServantErr m, MonadSparToBrig m) => SAML.UserId -> UserId -> m UserId
createUser suid _buid = do
  let newUser :: Brig.NewUser  -- TODO: set buid in NewUser (requires patch to brig).
      newUser = Brig.NewUser
        { Brig.newUserName           = Brig.Name . cs . SAML.encodeElem $ suid ^. SAML.uidSubject
        , Brig.newUserIdentity       = Just $ Brig.SSOIdentity (toUserSSOId suid) Nothing Nothing
        , Brig.newUserPict           = Nothing
        , Brig.newUserAssets         = []
        , Brig.newUserAccentId       = Nothing
        , Brig.newUserEmailCode      = Nothing
        , Brig.newUserPhoneCode      = Nothing
        , Brig.newUserOrigin         = Nothing  -- TODO: must carry team ID!
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


-- | Make sure we're on the same page with brig, a user spar knows locally has been successfully
-- created on brig (user exists) and galley (user has a team id).
confirmUserId :: (HasCallStack, MonadError ServantErr m, MonadSparToBrig m) => UserId -> m (Maybe UserId)
confirmUserId = undefined


-- | Get session token from brig and redirect user past login process.
forwardBrigLogin :: (HasCallStack, MonadError ServantErr m, SAML.HasConfig m, MonadSparToBrig m)
                 => UserId -> m SAML.Void
forwardBrigLogin buid = do
  resp :: Response (Maybe LBS) <- call
    $ method POST
    . path "/i/backdoor-login"
    . json (Brig.BackdoorLogin buid Nothing)
    . queryItem "persistent" "true"
    . expect2xx

  cki <- respToCookie resp
  target :: URI <- SAML.getLandingURI
  let hdrs :: [HTTP.Header] = [("Set-Cookie", cs . LBS.toLazyByteString . renderSetCookie $ cki)]
  SAML.redirect target hdrs
