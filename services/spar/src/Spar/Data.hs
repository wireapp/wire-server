{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ViewPatterns               #-}

module Spar.Data where

import Cassandra
import Control.Exception
import Control.Lens ((<&>))
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Identity
import Data.Int
import Data.String.Conversions
import Data.Time
import GHC.Stack

import qualified Data.Id as Brig
import qualified Data.UUID as UUID
import qualified SAML2.WebSSO as SAML


----------------------------------------------------------------------
-- helpers

-- | (seconds)
newtype TTL = TTL Int32
  deriving (Eq, Ord, Show, Num)

-- TODO: make 'dataEnvMaxTTL' configurable via yaml config.
data Env = Env { dataEnvNow :: UTCTime, dataEnvMaxTTL :: TTL }
  deriving (Eq, Show)

data TTLError = TTLTooLong | TTLInPast
  deriving (Eq, Show)

mkTTL :: MonadError TTLError m => Env -> UTCTime -> m TTL
mkTTL (Env now maxttl) endOfLife = if
  | actualttl > maxttl -> throwError TTLTooLong
  | actualttl <= 0     -> throwError TTLInPast
  | otherwise          -> pure actualttl
  where
    actualttl = TTL . round @Double . realToFrac $ endOfLife `diffUTCTime` now

err2err :: (m ~ Either TTLError, MonadThrow m') => m a -> m' a
err2err = either (throwM . ErrorCall . show) pure


----------------------------------------------------------------------
-- saml state handling

storeRequest :: (HasCallStack, MonadClient m) => Env -> SAML.ID SAML.AuthnRequest -> SAML.Time -> m ()
storeRequest env (SAML.ID rid) (SAML.Time endOfLife) = do
    TTL actualEndOfLife <- err2err $ mkTTL env endOfLife
    retry x5 . write ins $ params Quorum (rid, endOfLife, actualEndOfLife)
  where
    ins :: PrepQuery W (ST, UTCTime, Int32) ()
    ins = "INSERT INTO authreq (req, end_of_life) VALUES (?, ?) USING TTL ?"

checkAgainstRequest :: (HasCallStack, MonadClient m) => Env -> SAML.ID SAML.AuthnRequest -> m Bool
checkAgainstRequest env (SAML.ID rid) = do
    (retry x1 . query1 sel . params Quorum $ Identity rid) <&> \case
        Just (Identity (Just endoflife)) -> endoflife >= dataEnvNow env
        _ -> False
  where
    sel :: PrepQuery R (Identity ST) (Identity (Maybe UTCTime))
    sel = "SELECT end_of_life FROM authreq WHERE req = ?"

storeAssertion :: (HasCallStack, MonadClient m) => Env -> SAML.ID SAML.Assertion -> SAML.Time -> m Bool
storeAssertion env (SAML.ID aid) (SAML.Time endOfLifeNew) = do
    TTL actualEndOfLife <- err2err $ mkTTL env endOfLifeNew
    notAReplay :: Bool <- (retry x1 . query1 sel . params Quorum $ Identity aid) <&> \case
        Just (Identity (Just endoflifeOld)) -> endoflifeOld < dataEnvNow env
        _ -> False
    when notAReplay $ do
        retry x5 . write ins $ params Quorum (aid, endOfLifeNew, actualEndOfLife)
    pure notAReplay
  where
    sel :: PrepQuery R (Identity ST) (Identity (Maybe UTCTime))
    sel = "SELECT end_of_life FROM authresp WHERE resp = ?"

    ins :: PrepQuery W (ST, UTCTime, Int32) ()
    ins = "INSERT INTO authresp (resp, end_of_life) VALUES (?, ?) USING TTL ?"


----------------------------------------------------------------------
-- user

-- | Add new user.  If user with this 'SAML.UserId' exists, overwrite it.
insertUser :: (HasCallStack, MonadClient m) => SAML.UserId -> Brig.UserId -> m ()
insertUser (SAML.UserId tenant subject) uid = retry x5 . write ins $ params Quorum (tenant', subject', uid')
  where
    tenant', subject', uid' :: ST
    tenant'  = cs $ SAML.encodeElem tenant
    subject' = cs $ SAML.encodeElem subject
    uid'     = Brig.idToText uid

    ins :: PrepQuery W (ST, ST, ST) ()
    ins = "INSERT INTO user (idp, sso_id, uid) VALUES (?, ?, ?)"

getUser :: (HasCallStack, MonadClient m) => SAML.UserId -> m (Maybe Brig.UserId)
getUser (SAML.UserId tenant subject) = (retry x1 . query1 sel $ params Quorum (tenant', subject')) <&> \case
  Just (Identity (Just (UUID.fromText -> Just uuid))) -> Just $ Brig.Id uuid
  _ -> Nothing
  where
    tenant', subject' :: ST
    tenant'  = cs $ SAML.encodeElem tenant
    subject' = cs $ SAML.encodeElem subject

    sel :: PrepQuery R (ST, ST) (Identity (Maybe ST))
    sel = "SELECT uid FROM authresp WHERE idp = ? AND sso_id = ?"

-- | Delete a user.  If no such user exists, do nothing.
deleteUser :: (HasCallStack, MonadClient m) => SAML.UserId -> m ()
deleteUser = undefined


----------------------------------------------------------------------
-- idp

-- NOTE TO FUTURE SELF: when storing IdPs, we need to handle tenant conflicts.  we need to rely on
-- the fact that the tenant in the 'SAML.UserId' is always unique.
