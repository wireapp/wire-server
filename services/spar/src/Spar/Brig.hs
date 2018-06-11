{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Spar.Brig where

-- TODO: when creating user, we need to be able to provide more
-- master data (first name, last name, ...)

import Control.Monad.Except
import Data.String.Conversions
import Bilge
import Network.HTTP.Types.Method

import qualified Brig.Types.User as Brig
import qualified Data.Id as Brig
import qualified SAML2.WebSSO as SAML


toUserSSOId :: SAML.UserId -> Brig.UserSSOId
toUserSSOId (SAML.UserId tenant subject) =
  Brig.UserSSOId (cs $ SAML.encodeElem tenant) (cs $ SAML.encodeElem subject)

fromUserSSOId :: MonadError String m => Brig.UserSSOId -> m SAML.UserId
fromUserSSOId (Brig.UserSSOId (cs -> tenant) (cs -> subject)) =
  case (SAML.decodeElem tenant, SAML.decodeElem subject) of
    (Right t, Right s) -> pure $ SAML.UserId t s
    (Left msg, _)      -> throwError msg
    (_, Left msg)      -> throwError msg


class Monad m => MonadSparToBrig m where
  call :: (Request -> Request) -> m (Response (Maybe LBS))


getUser :: (MonadSparToBrig m) => SAML.UserId -> m (Maybe Brig.UserId)
getUser uid = do
  resp :: Response (Maybe LBS) <- call
    $ method GET
    . path ("/i/user/by-ssoid/" <> cs (show uid))  -- TODO: does anything like this exist?
  if statusCode resp == 200
    then do
      undefined  -- parse body and return user id from brig.  if parsing fails, log a warning and
                 -- return Nothing.
        -- (what should the body look like?  a full 'User' object?)
    else do
      pure Nothing

createUser :: (MonadSparToBrig m) => SAML.UserId -> m Brig.UserId
createUser _ = undefined

-- | Get session token from brig and redirect user past login process.
forwardBrigLogin :: (MonadSparToBrig m) => Brig.UserId -> m SAML.Void
forwardBrigLogin = undefined
