{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ViewPatterns               #-}

module Spar.Data where

import Cassandra as Cas
import Control.Exception
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Int
import Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import Data.Misc ((<$$>))
import Data.String.Conversions
import Data.Time
import Data.X509 (SignedCertificate)
import GHC.Stack
import Lens.Micro
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
    connectString <- maybe (return $ NE.fromList [cs $ Opts.cassandra opts ^.casEndpoint.epHost])
               (Cas.initialContacts "cassandra_spar")
               (cs <$> Opts.discoUrl opts)
    cas <- Cas.init (Log.clone (Just "cassandra.spar") lgr) $ Cas.defSettings
      & Cas.setContacts (NE.head connectString) (NE.tail connectString)
      & Cas.setPortNumber (fromIntegral $ Options.cassandra opts ^. casEndpoint . epPort)
      & Cas.setKeyspace (Keyspace $ Options.cassandra opts ^. casKeyspace)
      & Cas.setMaxConnections 4
      & Cas.setMaxStreams 128
      & Cas.setPoolStripes 4
      & Cas.setSendTimeout 3
      & Cas.setResponseTimeout 10
      & Cas.setProtocolVersion V3
    runClient cas $ Cas.versionCheck schemaVersion
    pure cas


----------------------------------------------------------------------
-- helpers

data Env = Env
  { dataEnvNow :: UTCTime
  , dataEnvMaxTTLAuthRequests :: TTL "authreq"
  , dataEnvMaxTTLAssertions   :: TTL "authresp"
  }
  deriving (Eq, Show)

data TTLError = TTLTooLong | TTLInPast
  deriving (Eq, Show)

mkTTLAuthnRequests :: MonadError TTLError m => Env -> UTCTime -> m (TTL "authreq")
mkTTLAuthnRequests (Env now maxttl _) = mkTTL now maxttl

mkTTLAssertions :: MonadError TTLError m => Env -> UTCTime -> m (TTL "authresp")
mkTTLAssertions (Env now _ maxttl) = mkTTL now maxttl

mkTTL :: MonadError TTLError m => UTCTime -> TTL a -> UTCTime -> m (TTL a)
mkTTL now maxttl endOfLife = if
  | actualttl > maxttl -> throwError TTLTooLong
  | actualttl <= 0     -> throwError TTLInPast
  | otherwise          -> pure actualttl
  where
    actualttl = TTL . round @Double . realToFrac $ endOfLife `diffUTCTime` now

err2err :: (m ~ Either TTLError, MonadThrow m') => m a -> m' a
err2err = either (throwM . ErrorCall . show) pure


----------------------------------------------------------------------
-- saml state handling

storeRequest :: (HasCallStack, MonadReader Env m, MonadClient m)
             => SAML.ID SAML.AuthnRequest -> SAML.Time -> m ()
storeRequest (SAML.ID rid) (SAML.Time endOfLife) = do
    env <- ask
    TTL actualEndOfLife <- err2err $ mkTTLAuthnRequests env endOfLife
    retry x5 . write ins $ params Quorum (rid, endOfLife, actualEndOfLife)
  where
    ins :: PrepQuery W (ST, UTCTime, Int32) ()
    ins = "INSERT INTO authreq (req, end_of_life) VALUES (?, ?) USING TTL ?"

checkAgainstRequest :: (HasCallStack, MonadReader Env m, MonadClient m)
                    => SAML.ID SAML.AuthnRequest -> m Bool
checkAgainstRequest (SAML.ID rid) = do
    env <- ask
    (retry x1 . query1 sel . params Quorum $ Identity rid) <&> \case
        Just (Identity (Just endoflife)) -> endoflife >= dataEnvNow env
        _ -> False
  where
    sel :: PrepQuery R (Identity ST) (Identity (Maybe UTCTime))
    sel = "SELECT end_of_life FROM authreq WHERE req = ?"

storeAssertion :: (HasCallStack, MonadReader Env m, MonadClient m)
               => SAML.ID SAML.Assertion -> SAML.Time -> m Bool
storeAssertion (SAML.ID aid) (SAML.Time endOfLifeNew) = do
    env <- ask
    TTL actualEndOfLife <- err2err $ mkTTLAssertions env endOfLifeNew
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
storeIdPConfig idp = retry x5 . batch $ do
  setType BatchLogged
  setConsistency Quorum
  addPrepQuery ins
    ( idp ^. SAML.idpId
    , idp ^. SAML.idpMetadata
    , idp ^. SAML.idpIssuer
    , idp ^. SAML.idpRequestUri
    , idp ^. SAML.idpPublicKey
    , idp ^. SAML.idpExtraInfo
    )
  addPrepQuery byIssuer
    ( idp ^. SAML.idpId
    , idp ^. SAML.idpIssuer
    )
  addPrepQuery byTeam
    ( idp ^. SAML.idpId
    , idp ^. SAML.idpExtraInfo
    )
  where
    ins :: PrepQuery W IdPConfigRow ()
    ins = "INSERT INTO idp (idp, metadata, issuer, request_uri, public_key, team) VALUES (?, ?, ?, ?, ?, ?)"

    byIssuer :: PrepQuery W (SAML.IdPId, SAML.Issuer) ()
    byIssuer = "INSERT INTO issuer_idp (idp, issuer) VALUES (?, ?)"

    byTeam :: PrepQuery W (SAML.IdPId, Brig.TeamId) ()
    byTeam = "INSERT INTO team_idp (idp, team) VALUES (?, ?)"

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
    sel = "SELECT idp FROM issuer_idp WHERE issuer = ?"

getIdPConfigsByTeam :: (HasCallStack, MonadClient m) => Brig.TeamId -> m [IdP]
getIdPConfigsByTeam team = do
    idpids <- runIdentity <$$> retry x1 (query sel $ params Quorum (Identity team))
    catMaybes <$> mapM getIdPConfig idpids
  where
    sel :: PrepQuery R (Identity Brig.TeamId) (Identity SAML.IdPId)
    sel = "SELECT idp FROM team_idp WHERE team = ?"

deleteIdPConfig :: (HasCallStack, MonadClient m) => SAML.IdPId -> SAML.Issuer -> Brig.TeamId -> m ()
deleteIdPConfig idp issuer team = retry x5 $ batch $ do
    setType BatchLogged
    setConsistency Quorum
    addPrepQuery delIdp (Identity idp)
    addPrepQuery delIssuerIdp (Identity issuer)
    addPrepQuery delTeamIdp (team, idp)
  where
    delIdp :: PrepQuery W (Identity SAML.IdPId) ()
    delIdp = "DELETE FROM idp WHERE idp = ?"

    delIssuerIdp :: PrepQuery W (Identity SAML.Issuer) ()
    delIssuerIdp = "DELETE FROM issuer_idp WHERE issuer = ?"

    delTeamIdp :: PrepQuery W (Brig.TeamId, SAML.IdPId) ()
    delTeamIdp = "DELETE FROM team_idp WHERE team = ? and idp = ?"
