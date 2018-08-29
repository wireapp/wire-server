{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE NamedFieldPuns             #-}
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
import Data.Id
import Data.Int
import Data.Maybe (catMaybes)
import Data.Misc ((<$$>))
import Data.String.Conversions
import Data.Time
import Data.X509 (SignedCertificate)
import GHC.Stack
import Lens.Micro
import Spar.Data.Instances (VerdictFormatRow, VerdictFormatCon, fromVerdictFormat, toVerdictFormat)
import Spar.Options as Options
import Spar.Types
import URI.ByteString

import qualified Data.UUID as UUID
import qualified SAML2.WebSSO as SAML
import qualified Data.ByteString.Char8 as BSC


----------------------------------------------------------------------
-- helpers

data Env = Env
  { dataEnvNow                :: UTCTime
  , dataEnvSPInfo             :: SPInfo
  , dataEnvMaxTTLAuthRequests :: TTL "authreq"
  , dataEnvMaxTTLAssertions   :: TTL "authresp"
  }
  deriving (Eq, Show)

mkEnv :: Options.Opts -> UTCTime -> Env
mkEnv opts now =
  Env { dataEnvNow                = now
      , dataEnvSPInfo             = Options.spInfo opts
      , dataEnvMaxTTLAuthRequests = Options.maxttlAuthreq opts
      , dataEnvMaxTTLAssertions   = Options.maxttlAuthresp opts
      }

mkTTLAuthnRequests :: MonadError TTLError m => Env -> UTCTime -> m (TTL "authreq")
mkTTLAuthnRequests (Env now _ maxttl _) = mkTTL now maxttl

mkTTLAssertions :: MonadError TTLError m => Env -> UTCTime -> m (TTL "authresp")
mkTTLAssertions (Env now _ _ maxttl) = mkTTL now maxttl

mkTTL :: MonadError TTLError m => UTCTime -> TTL a -> UTCTime -> m (TTL a)
mkTTL now maxttl endOfLife = if
  | actualttl > maxttl -> throwError TTLTooLong
  | actualttl <= 0     -> throwError TTLNegative
  | otherwise          -> pure actualttl
  where
    actualttl = TTL . nominalDiffToSeconds $ endOfLife `diffUTCTime` now

err2err :: (m ~ Either TTLError, MonadThrow m') => m a -> m' a
err2err = either (throwM . ErrorCall . show) pure

nominalDiffToSeconds :: NominalDiffTime -> Int32
nominalDiffToSeconds = round @Double . realToFrac


----------------------------------------------------------------------
-- saml state handling

storeRequest :: (HasCallStack, MonadReader Env m, MonadClient m)
             => AReqId -> SAML.Time -> m ()
storeRequest (SAML.ID rid) (SAML.Time endOfLife) = do
    env <- ask
    TTL actualEndOfLife <- err2err $ mkTTLAuthnRequests env endOfLife
    retry x5 . write ins $ params Quorum (rid, endOfLife, actualEndOfLife)
  where
    ins :: PrepQuery W (ST, UTCTime, Int32) ()
    ins = "INSERT INTO authreq (req, end_of_life) VALUES (?, ?) USING TTL ?"

checkAgainstRequest :: (HasCallStack, MonadReader Env m, MonadClient m)
                    => AReqId -> m Bool
checkAgainstRequest (SAML.ID rid) = do
    env <- ask
    (retry x1 . query1 sel . params Quorum $ Identity rid) <&>
        maybe False ((>= dataEnvNow env) . runIdentity)
  where
    sel :: PrepQuery R (Identity ST) (Identity UTCTime)
    sel = "SELECT end_of_life FROM authreq WHERE req = ?"

-- FUTUREWORK: is there a guarantee in cassandra that records are not returned once their TTL has
-- expired?  if yes, that would greatly simplify this table.  if no, we might still be able to push
-- the end_of_life comparison into the database, rather than retrieving the value and comparing it
-- in haskell.  (also check the other functions in this module.)
storeAssertion :: (HasCallStack, MonadReader Env m, MonadClient m)
               => SAML.ID SAML.Assertion -> SAML.Time -> m Bool
storeAssertion (SAML.ID aid) (SAML.Time endOfLifeNew) = do
    env <- ask
    TTL actualEndOfLife <- err2err $ mkTTLAssertions env endOfLifeNew
    notAReplay :: Bool <- (retry x1 . query1 sel . params Quorum $ Identity aid) <&>
        maybe True ((< dataEnvNow env) . runIdentity)
    when notAReplay $ do
        retry x5 . write ins $ params Quorum (aid, endOfLifeNew, actualEndOfLife)
    pure notAReplay
  where
    sel :: PrepQuery R (Identity ST) (Identity UTCTime)
    sel = "SELECT end_of_life FROM authresp WHERE resp = ?"

    ins :: PrepQuery W (ST, UTCTime, Int32) ()
    ins = "INSERT INTO authresp (resp, end_of_life) VALUES (?, ?) USING TTL ?"


----------------------------------------------------------------------
-- spar state handling (not visible to saml2-web-sso)

-- | First argument is the life expectancy of the request.  (We store the verdict format for twice
-- as long.  Reason: if there is some delay in processing a very old request, it would be bad for
-- error handling if we couldn't figure out where the error will land.)
storeVerdictFormat :: (HasCallStack, MonadClient m)
                   => NominalDiffTime -> AReqId -> VerdictFormat -> m ()
storeVerdictFormat diffTime req (fromVerdictFormat -> (fmtCon, fmtMobSucc, fmtMobErr)) = do
    let ttl = nominalDiffToSeconds diffTime * 2
    retry x5 . write cql $ params Quorum (req, fmtCon, fmtMobSucc, fmtMobErr, ttl)
  where
    cql :: PrepQuery W (AReqId, VerdictFormatCon, Maybe URI, Maybe URI, Int32) ()
    cql = "INSERT INTO verdict (req, format_con, format_mobile_success, format_mobile_error) VALUES (?, ?, ?, ?) USING TTL ?"

getVerdictFormat :: (HasCallStack, MonadClient m)
                   => AReqId -> m (Maybe VerdictFormat)
getVerdictFormat req = (>>= toVerdictFormat) <$>
  (retry x1 . query1 cql $ params Quorum (Identity req))
  where
    cql :: PrepQuery R (Identity AReqId) VerdictFormatRow
    cql = "SELECT format_con, format_mobile_success, format_mobile_error FROM verdict WHERE req = ?"


----------------------------------------------------------------------
-- user

-- | Add new user.  If user with this 'SAML.UserId' exists, overwrite it.
insertUser :: (HasCallStack, MonadClient m) => SAML.UserRef -> UserId -> m ()
insertUser (SAML.UserRef tenant subject) uid = retry x5 . write ins $ params Quorum (tenant, subject, uid)
  where
    ins :: PrepQuery W (SAML.Issuer, SAML.NameID, UserId) ()
    ins = "INSERT INTO user (issuer, sso_id, uid) VALUES (?, ?, ?)"

getUser :: (HasCallStack, MonadClient m) => SAML.UserRef -> m (Maybe UserId)
getUser (SAML.UserRef tenant subject) = fmap runIdentity <$>
  (retry x1 . query1 sel $ params Quorum (tenant, subject))
  where
    sel :: PrepQuery R (SAML.Issuer, SAML.NameID) (Identity UserId)
    sel = "SELECT uid FROM user WHERE issuer = ? AND sso_id = ?"


----------------------------------------------------------------------
-- idp

type IdPConfigRow = (SAML.IdPId, URI, SAML.Issuer, URI, SignedCertificate, TeamId)

storeIdPConfig :: (HasCallStack, MonadClient m) => SAML.IdPConfig IdPExtra -> m ()
storeIdPConfig idp = retry x5 . batch $ do
  setType BatchLogged
  setConsistency Quorum
  addPrepQuery ins
    ( idp ^. SAML.idpId
    , idp ^. SAML.idpMetadata
    , idp ^. SAML.idpIssuer
    , idp ^. SAML.idpRequestUri
    , idp ^. SAML.idpPublicKey
    , idp ^. SAML.idpExtraInfo . idpeTeam
    )
  addPrepQuery byIssuer
    ( idp ^. SAML.idpId
    , idp ^. SAML.idpIssuer
    )
  addPrepQuery byTeam
    ( idp ^. SAML.idpId
    , idp ^. SAML.idpExtraInfo . idpeTeam
    )
  where
    ins :: PrepQuery W IdPConfigRow ()
    ins = "INSERT INTO idp (idp, metadata, issuer, request_uri, public_key, team) VALUES (?, ?, ?, ?, ?, ?)"

    byIssuer :: PrepQuery W (SAML.IdPId, SAML.Issuer) ()
    byIssuer = "INSERT INTO issuer_idp (idp, issuer) VALUES (?, ?)"

    byTeam :: PrepQuery W (SAML.IdPId, TeamId) ()
    byTeam = "INSERT INTO team_idp (idp, team) VALUES (?, ?)"

getSPInfo :: MonadReader Env m => SAML.IdPId -> m SPInfo
getSPInfo (SAML.IdPId idp) = do
  env <- ask
  pure $ dataEnvSPInfo env & spiLoginURI . pathL %~ (<> BSC.pack (UUID.toString idp))

getIdPConfig
  :: forall m. (HasCallStack, MonadClient m, MonadReader Env m)
  => SAML.IdPId -> m (Maybe IdP)
getIdPConfig idpid =
  traverse toIdp =<< retry x1 (query1 sel $ params Quorum (Identity idpid))
  where
    toIdp :: IdPConfigRow -> m IdP
    toIdp ( _idpId
          , _idpMetadata
          , _idpIssuer
          , _idpRequestUri
          , _idpPublicKey
          -- extras
          , _idpeTeam
          ) = do
      _idpeSPInfo <- getSPInfo _idpId
      let _idpExtraInfo = IdPExtra { _idpeTeam, _idpeSPInfo }
      pure $ SAML.IdPConfig {..}

    sel :: PrepQuery R (Identity SAML.IdPId) IdPConfigRow
    sel = "SELECT idp, metadata, issuer, request_uri, public_key, team FROM idp WHERE idp = ?"

getIdPConfigByIssuer
  :: (HasCallStack, MonadClient m, MonadReader Env m)
  => SAML.Issuer -> m (Maybe IdP)
getIdPConfigByIssuer issuer = do
  getIdPIdByIssuer issuer >>= \case
    Nothing    -> pure Nothing
    Just idpid -> getIdPConfig idpid

getIdPIdByIssuer
  :: (HasCallStack, MonadClient m)
  => SAML.Issuer -> m (Maybe SAML.IdPId)
getIdPIdByIssuer issuer = do
  retry x1 (query1 sel $ params Quorum (Identity issuer)) <&> \case
    Nothing               -> Nothing
    Just (Identity idpid) -> Just idpid
  where
    sel :: PrepQuery R (Identity SAML.Issuer) (Identity SAML.IdPId)
    sel = "SELECT idp FROM issuer_idp WHERE issuer = ?"

getIdPConfigsByTeam
  :: (HasCallStack, MonadClient m, MonadReader Env m)
  => TeamId -> m [IdP]
getIdPConfigsByTeam team = do
    idpids <- runIdentity <$$> retry x1 (query sel $ params Quorum (Identity team))
    catMaybes <$> mapM getIdPConfig idpids
  where
    sel :: PrepQuery R (Identity TeamId) (Identity SAML.IdPId)
    sel = "SELECT idp FROM team_idp WHERE team = ?"

deleteIdPConfig :: (HasCallStack, MonadClient m) => SAML.IdPId -> SAML.Issuer -> TeamId -> m ()
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

    delTeamIdp :: PrepQuery W (TeamId, SAML.IdPId) ()
    delTeamIdp = "DELETE FROM team_idp WHERE team = ? and idp = ?"
