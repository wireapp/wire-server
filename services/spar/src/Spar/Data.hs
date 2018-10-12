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

module Spar.Data
  ( schemaVersion
  , Env(..)
  , mkEnv
  , mkTTLAssertions
  , storeRequest
  , checkAgainstRequest
  , storeAssertion
  , storeVerdictFormat
  , getVerdictFormat
  , insertUser
  , getUser
  , insertBindCookie
  , lookupBindCookie
  , storeIdPConfig
  , getIdPConfig
  , getIdPConfigByIssuer
  , getIdPIdByIssuer
  , getIdPConfigsByTeam
  , deleteIdPConfig
  ) where

import Cassandra as Cas
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
import GHC.TypeLits (KnownSymbol)
import Lens.Micro
import Spar.Data.Instances (VerdictFormatRow, VerdictFormatCon, fromVerdictFormat, toVerdictFormat)
import Spar.Types
import URI.ByteString

import qualified Data.List.NonEmpty as NL
import qualified SAML2.WebSSO as SAML
import qualified Web.Cookie as Cky


-- | NB: this is a lower bound (@<=@, not @==@).
schemaVersion :: Int32
schemaVersion = 3


----------------------------------------------------------------------
-- helpers

data Env = Env
  { dataEnvNow                :: UTCTime
  , dataEnvMaxTTLAuthRequests :: TTL "authreq"
  , dataEnvMaxTTLAssertions   :: TTL "authresp"
  }
  deriving (Eq, Show)

mkEnv :: Opts -> UTCTime -> Env
mkEnv opts now =
  Env { dataEnvNow                = now
      , dataEnvMaxTTLAuthRequests = maxttlAuthreq opts
      , dataEnvMaxTTLAssertions   = maxttlAuthresp opts
      }

mkTTLAuthnRequests :: MonadError TTLError m => Env -> UTCTime -> m (TTL "authreq")
mkTTLAuthnRequests (Env now maxttl _) = mkTTL now maxttl

mkTTLAuthnRequestsNDT :: MonadError TTLError m => Env -> NominalDiffTime -> m (TTL "authreq")
mkTTLAuthnRequestsNDT (Env _ maxttl _) = mkTTLNDT maxttl

mkTTLAssertions :: MonadError TTLError m => Env -> UTCTime -> m (TTL "authresp")
mkTTLAssertions (Env now _ maxttl) = mkTTL now maxttl

mkTTL :: (MonadError TTLError m, KnownSymbol a) => UTCTime -> TTL a -> UTCTime -> m (TTL a)
mkTTL now maxttl endOfLife = mkTTLNDT maxttl $ endOfLife `diffUTCTime` now

mkTTLNDT :: (MonadError TTLError m, KnownSymbol a) => TTL a -> NominalDiffTime -> m (TTL a)
mkTTLNDT maxttl ttlNDT = if
  | actualttl > maxttl -> throwError $ TTLTooLong (showTTL actualttl) (showTTL maxttl)
  | actualttl <= 0     -> throwError $ TTLNegative (showTTL actualttl)
  | otherwise          -> pure actualttl
  where
    actualttl = TTL . nominalDiffToSeconds $ ttlNDT

nominalDiffToSeconds :: NominalDiffTime -> Int32
nominalDiffToSeconds = round @Double . realToFrac


----------------------------------------------------------------------
-- saml state handling

storeRequest :: (HasCallStack, MonadReader Env m, MonadClient m, MonadError TTLError m)
             => AReqId -> SAML.Time -> m ()
storeRequest (SAML.ID rid) (SAML.Time endOfLife) = do
    env <- ask
    TTL ttl <- mkTTLAuthnRequests env endOfLife
    retry x5 . write ins $ params Quorum (rid, ttl)
  where
    ins :: PrepQuery W (ST, Int32) ()
    ins = "INSERT INTO authreq (req) VALUES (?) USING TTL ?"

checkAgainstRequest :: (HasCallStack, MonadClient m)
                    => AReqId -> m Bool
checkAgainstRequest (SAML.ID rid) =
    (==) (Just 1) <$> (retry x1 . query1 sel . params Quorum $ Identity rid)
  where
    sel :: PrepQuery R (Identity ST) (Identity Int64)
    sel = "SELECT COUNT(*) FROM authreq WHERE req = ?"

storeAssertion :: (HasCallStack, MonadReader Env m, MonadClient m, MonadError TTLError m)
               => SAML.ID SAML.Assertion -> SAML.Time -> m Bool
storeAssertion (SAML.ID aid) (SAML.Time endOfLifeNew) = do
    env <- ask
    TTL ttl <- mkTTLAssertions env endOfLifeNew
    notAReplay <- (/=) (Just 1) <$> (retry x1 . query1 sel . params Quorum $ Identity aid)
    when notAReplay $ do
        retry x5 . write ins $ params Quorum (aid, ttl)
    pure notAReplay
  where
    sel :: PrepQuery R (Identity ST) (Identity Int64)
    sel = "SELECT COUNT(*) FROM authresp WHERE resp = ?"

    ins :: PrepQuery W (ST, Int32) ()
    ins = "INSERT INTO authresp (resp) VALUES (?) USING TTL ?"


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
-- bind cookies

-- | Associate the value of a 'BindCookie' with its 'UserId'.  The 'TTL' of this entry should be the
-- same as the one of the 'AuthnRequest' sent with the cookie.
insertBindCookie :: (HasCallStack, MonadClient m, MonadReader Env m, MonadError TTLError m)
                 => BindCookie -> UserId -> NominalDiffTime -> m ()
insertBindCookie cky uid ttlNDT = do
  env <- ask
  TTL ttlInt32 <- mkTTLAuthnRequestsNDT env ttlNDT
  let ckyval = cs . Cky.setCookieValue . SAML.fromSimpleSetCookie $ cky
  retry x5 . write ins $ params Quorum (ckyval, uid, ttlInt32)
  where
    ins :: PrepQuery W (ST, UserId, Int32) ()
    ins = "INSERT INTO bind_cookie (cookie, session_owner) VALUES (?, ?) USING TTL ?"

-- | The counter-part of 'insertBindCookie'.
lookupBindCookie :: (HasCallStack, MonadClient m) => BindCookie -> m (Maybe UserId)
lookupBindCookie cky = fmap runIdentity <$> do
  let ckyval = cs . Cky.setCookieValue . SAML.fromSimpleSetCookie $ cky
  (retry x1 . query1 sel $ params Quorum (Identity ckyval))
  where
    sel :: PrepQuery R (Identity ST) (Identity UserId)
    sel = "SELECT session_owner FROM bind_cookie WHERE cookie = ?"


----------------------------------------------------------------------
-- idp

type IdPConfigRow = (SAML.IdPId, SAML.Issuer, URI, SignedCertificate, [SignedCertificate], TeamId)

storeIdPConfig :: (HasCallStack, MonadClient m) => SAML.IdPConfig TeamId -> m ()
storeIdPConfig idp = retry x5 . batch $ do
  setType BatchLogged
  setConsistency Quorum
  addPrepQuery ins
    ( idp ^. SAML.idpId
    , idp ^. SAML.idpMetadata . SAML.edIssuer
    , idp ^. SAML.idpMetadata . SAML.edRequestURI
    , NL.head (idp ^. SAML.idpMetadata . SAML.edCertAuthnResponse)
    , NL.tail (idp ^. SAML.idpMetadata . SAML.edCertAuthnResponse)
      -- (the 'List1' is split up into head and tail to make migration from one-element-only easier.)
    , idp ^. SAML.idpExtraInfo
    )
  addPrepQuery byIssuer
    ( idp ^. SAML.idpId
    , idp ^. SAML.idpMetadata . SAML.edIssuer
    )
  addPrepQuery byTeam
    ( idp ^. SAML.idpId
    , idp ^. SAML.idpExtraInfo
    )
  where
    ins :: PrepQuery W IdPConfigRow ()
    ins = "INSERT INTO idp (idp, issuer, request_uri, public_key, extra_public_keys, team) VALUES (?, ?, ?, ?, ?, ?)"

    byIssuer :: PrepQuery W (SAML.IdPId, SAML.Issuer) ()
    byIssuer = "INSERT INTO issuer_idp (idp, issuer) VALUES (?, ?)"

    byTeam :: PrepQuery W (SAML.IdPId, TeamId) ()
    byTeam = "INSERT INTO team_idp (idp, team) VALUES (?, ?)"

getIdPConfig
  :: forall m. (HasCallStack, MonadClient m, MonadReader Env m)
  => SAML.IdPId -> m (Maybe IdP)
getIdPConfig idpid =
  traverse toIdp =<< retry x1 (query1 sel $ params Quorum (Identity idpid))
  where
    toIdp :: IdPConfigRow -> m IdP
    toIdp ( _idpId
          -- metadata
          , _edIssuer
          , _edRequestURI
          , certsHead
          , certsTail
          -- extras
          , _idpExtraInfo
          ) = do
      let _edCertAuthnResponse = certsHead NL.:| certsTail
          _idpMetadata = SAML.IdPMetadata {..}
      pure $ SAML.IdPConfig {..}

    sel :: PrepQuery R (Identity SAML.IdPId) IdPConfigRow
    sel = "SELECT idp, issuer, request_uri, public_key, extra_public_keys, team FROM idp WHERE idp = ?"

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
