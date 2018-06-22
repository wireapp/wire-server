{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE RecordWildCards            #-}

module Spar.Data where

import Cassandra as Cas
import Control.Exception
import Control.Lens hiding (Level)
import Control.Monad.Catch
import Control.Monad.Except
import Data.Int
import Data.List.NonEmpty as NE
import Data.Misc ((<$$>))
import Data.String.Conversions
import Data.Time
import Data.X509 (SignedCertificate)
import GHC.Stack
import Spar.Data.Instances ()
import Spar.Options as Options
import Spar.Types
import System.Logger (Logger)
import URI.ByteString
import Util.Options (casEndpoint, casKeyspace, epHost, epPort)

import qualified Cassandra.Schema as Cas
import qualified Cassandra.Settings as Cas
import qualified Data.Id as Brig
import qualified Data.UUID as UUID
import qualified SAML2.WebSSO as SAML
import qualified Spar.Options as Opts
import qualified System.Logger as Log


----------------------------------------------------------------------
-- init

schemaVersion :: Int32
schemaVersion = 0

initCassandra :: Opts.Opts -> Logger -> IO ClientState
initCassandra opts lgr = do
    let connectString = cs (Opts.cassandra opts ^. casEndpoint . epHost) :| []
    cas <- Cas.init (Log.clone (Just "cassandra.spar") lgr) $ Cas.defSettings
      & Cas.setContacts (NE.head connectString) (NE.tail connectString)
      & Cas.setPortNumber (fromIntegral $ Options.cassandra opts ^. casEndpoint . epPort)
      & Cas.setKeyspace (Keyspace $ Options.cassandra opts ^. casKeyspace)
      & Cas.setMaxConnections 4
      & Cas.setPoolStripes 4
      & Cas.setSendTimeout 3
      & Cas.setResponseTimeout 10
      & Cas.setProtocolVersion V3
    runClient cas $ Cas.versionCheck schemaVersion
    pure cas


----------------------------------------------------------------------
-- helpers

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
insertUser :: (HasCallStack, MonadClient m) => SAML.UserRef -> Brig.UserId -> m ()
insertUser (SAML.UserRef tenant subject) uid = retry x5 . write ins $ params Quorum (tenant', subject', uid')
  where
    tenant', subject', uid' :: ST
    tenant'  = cs $ SAML.encodeElem tenant
    subject' = cs $ SAML.encodeElem subject
    uid'     = Brig.idToText uid

    ins :: PrepQuery W (ST, ST, ST) ()
    ins = "INSERT INTO user (idp, sso_id, uid) VALUES (?, ?, ?)"

getUser :: (HasCallStack, MonadClient m) => SAML.UserRef -> m (Maybe Brig.UserId)
getUser (SAML.UserRef tenant subject) = (retry x1 . query1 sel $ params Quorum (tenant', subject')) <&> \case
  Just (Identity (Just (UUID.fromText -> Just uuid))) -> Just $ Brig.Id uuid
  _ -> Nothing
  where
    tenant', subject' :: ST
    tenant'  = cs $ SAML.encodeElem tenant
    subject' = cs $ SAML.encodeElem subject

    sel :: PrepQuery R (ST, ST) (Identity (Maybe ST))
    sel = "SELECT uid FROM authresp WHERE idp = ? AND sso_id = ?"


----------------------------------------------------------------------
-- idp

type IdPConfigRow = (SAML.IdPId, URI, SAML.Issuer, URI, SignedCertificate, Brig.TeamId)

storeIdPConfig :: (HasCallStack, MonadClient m) => SAML.IdPConfig Brig.TeamId -> m ()
storeIdPConfig idp = retry x5 $ do
  write ins $ params Quorum
    ( idp ^. SAML.idpId
    , idp ^. SAML.idpMetadata
    , idp ^. SAML.idpIssuer
    , idp ^. SAML.idpRequestUri
    , idp ^. SAML.idpPublicKey
    , idp ^. SAML.idpExtraInfo
    )
  write ins' $ params Quorum
    ( idp ^. SAML.idpId
    , idp ^. SAML.idpIssuer
    )
  where
    ins :: PrepQuery W IdPConfigRow ()
    ins = "INSERT INTO idp (idp, metadata, issuer, request_uri, public_key, team) VALUES (?, ?, ?, ?, ?, ?)"

    ins' :: PrepQuery W (SAML.IdPId, SAML.Issuer) ()
    ins' = "INSERT INTO idp_by_issuer (idp, issuer) VALUES (?, ?)"

getIdPConfig :: (HasCallStack, MonadClient m) => SAML.IdPId -> m (Maybe IdP)
getIdPConfig idpid = toIdp <$$> retry x1 (query1 sel $ params Quorum (Identity idpid))
  where
    toIdp :: IdPConfigRow -> IdP
    toIdp ( _idpId
          , _idpMetadata
          , _idpIssuer
          , _idpRequestUri
          , _idpPublicKey
          , _idpExtraInfo
          ) = SAML.IdPConfig {..}

    sel :: PrepQuery R (Identity SAML.IdPId) IdPConfigRow
    sel = "SELECT idp, metadata, issuer, request_uri, public_key, team FROM idp WHERE idp = ?"

getIdPConfigByIssuer :: (HasCallStack, MonadClient m) => SAML.Issuer -> m (Maybe IdP)
getIdPConfigByIssuer issuer = do
  retry x1 (query1 sel $ params Quorum (Identity issuer)) >>= \case
    Nothing -> pure Nothing
    Just (Identity idpid) -> getIdPConfig idpid
  where
    sel :: PrepQuery R (Identity SAML.Issuer) (Identity SAML.IdPId)
    sel = "SELECT idp FROM idp_by_issuer WHERE issuer = ?"
