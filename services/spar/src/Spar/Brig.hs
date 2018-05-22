{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Spar.Brig where

import Control.Monad.Except
import Data.String.Conversions (ST, cs)

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


class MonadBrigClient (m :: * -> *)  -- no, use runHttpT
  reqGet :: (ToJSON reqbody, FromJSON respbody) => [ST] -> reqbody -> m respbody


getUser :: MonadBrigClient m => SAML.UserId -> m (Maybe Brig.UserId)
getUser = undefined

-- TODO: first name, last name, ...?
createUser :: MonadBrigClient m => SAML.UserId -> m Brig.UserId
createUser = undefined

forwardBrigLogin :: MonadBrigClient m => Brig.UserId -> m SAML.Void
forwardBrigLogin = undefined
