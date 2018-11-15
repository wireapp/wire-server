{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Spar.API
  ( app, api
  , API
  , APIMeta
  , APIAuthReqPrecheck
  , APIAuthReq
  , APIAuthResp
  , IdpGet
  , IdpGetAll
  , IdpCreate
  , IdpDelete
  ) where

import Imports
import Brig.Types.User as Brig
import Control.Lens
import Control.Monad.Except
import Data.Id
import Data.Proxy
import Data.String.Conversions
import Data.Time
import OpenSSL.Random (randBytes)
import Servant
import Servant.Swagger
import Spar.Orphans ()
import Spar.API.Swagger ()
import Spar.API.Types
import Spar.App
import Spar.Error
import Spar.Types
import Spar.SCIM

import qualified Data.ByteString as SBS
import qualified Data.ByteString.Base64 as ES
import qualified SAML2.WebSSO as SAML
import qualified Spar.Data as Data
import qualified Spar.Intra.Brig as Intra
import qualified URI.ByteString as URI
import qualified Web.Cookie as Cky


app :: Env -> Application
app ctx = SAML.setHttpCachePolicy
        $ serve (Proxy @API) (hoistServer (Proxy @API) (SAML.nt @SparError @Spar ctx) (api $ sparCtxOpts ctx) :: Server API)

-- TODO: re-enable SCIM.api once it's ready to use.
api :: Opts -> ServerT API Spar
api opts
     = apiSSO opts
  :<|> authreq (maxttlAuthreqDiffTime opts) DoInitiateBind
  :<|> apiIDP
  :<|> apiScim
  :<|> apiINTERNAL

apiSSO :: Opts -> ServerT APISSO Spar
apiSSO opts
     = pure (toSwagger (Proxy @OutsideWorldAPI))
  :<|> SAML.meta appName sparSPIssuer sparResponseURI
  :<|> authreqPrecheck
  :<|> authreq (maxttlAuthreqDiffTime opts) DoInitiateLogin
  :<|> authresp

apiIDP :: ServerT APIIDP Spar
apiIDP
     = idpGet
  :<|> idpGetAll
  :<|> idpCreate
  :<|> idpDelete

apiINTERNAL :: ServerT APIINTERNAL Spar
apiINTERNAL
     = pure NoContent


appName :: ST
appName = "spar"

authreqPrecheck :: Maybe URI.URI -> Maybe URI.URI -> SAML.IdPId -> Spar NoContent
authreqPrecheck msucc merr idpid = validateAuthreqParams msucc merr
                                *> SAML.getIdPConfig idpid
                                *> return NoContent

authreq :: NominalDiffTime -> DoInitiate -> Maybe UserId -> Maybe URI.URI -> Maybe URI.URI -> SAML.IdPId
        -> Spar (WithSetBindCookie (SAML.FormRedirect SAML.AuthnRequest))
authreq _ DoInitiateLogin (Just _) _ _ _ = throwSpar SparInitLoginWithAuth
authreq _ DoInitiateBind Nothing _ _ _   = throwSpar SparInitBindWithoutAuth
authreq authreqttl _ zusr msucc merr idpid = do
  vformat <- validateAuthreqParams msucc merr
  form@(SAML.FormRedirect _ ((^. SAML.rqID) -> reqid)) <- SAML.authreq authreqttl sparSPIssuer idpid
  wrapMonadClient $ Data.storeVerdictFormat authreqttl reqid vformat
  cky <- initializeBindCookie zusr authreqttl
  SAML.logger SAML.Debug $ "setting bind cookie: " <> show cky
  pure $ addHeader cky form

-- | If the user is already authenticated, create bind cookie with a given life expectancy and our
-- domain, and store it in C*.  If the user is not authenticated, return a deletion 'SetCookie'
-- value that deletes any bind cookies on the client.
initializeBindCookie :: Maybe UserId -> NominalDiffTime -> Spar SetBindCookie
initializeBindCookie zusr authreqttl = do
  DerivedOpts path domain <- asks (derivedOpts . sparCtxOpts)
  msecret <- if isJust zusr
             then liftIO $ Just . cs . ES.encode <$> randBytes 32
             else pure Nothing
  let updSetCkyDom (SAML.SimpleSetCookie raw) = SAML.SimpleSetCookie raw { Cky.setCookieDomain = Just domain }
  cky <- updSetCkyDom <$> (SAML.toggleCookie path $ (, authreqttl) <$> msecret :: Spar SetBindCookie)
  forM_ zusr $ \userid -> wrapMonadClientWithEnv $ Data.insertBindCookie cky userid authreqttl
  pure cky

redirectURLMaxLength :: Int
redirectURLMaxLength = 140

validateAuthreqParams :: Maybe URI.URI -> Maybe URI.URI -> Spar VerdictFormat
validateAuthreqParams msucc merr = case (msucc, merr) of
  (Nothing, Nothing) -> pure VerdictFormatWeb
  (Just ok, Just err) -> do
    validateRedirectURL `mapM_` [ok, err]
    pure $ VerdictFormatMobile ok err
  _ -> throwSpar $ SparBadInitiateLoginQueryParams "need-both-redirect-urls"

validateRedirectURL :: URI.URI -> Spar ()
validateRedirectURL uri = do
  unless ((SBS.take 4 . URI.schemeBS . URI.uriScheme $ uri) == "wire") $ do
    throwSpar $ SparBadInitiateLoginQueryParams "invalid-schema"
  unless ((SBS.length $ URI.serializeURIRef' uri) <= redirectURLMaxLength) $ do
    throwSpar $ SparBadInitiateLoginQueryParams "url-too-long"


authresp :: Maybe ST -> SAML.AuthnResponseBody -> Spar Void
authresp ((>>= bindCookieFromHeader) -> cky) = SAML.authresp sparSPIssuer sparResponseURI $
  \resp verdict -> throwError . SAML.CustomServant =<< verdictHandler cky resp verdict


idpGet :: Maybe UserId -> SAML.IdPId -> Spar IdP
idpGet zusr idpid = withDebugLog "idpGet" (Just . show . (^. SAML.idpId)) $ do
  idp <- SAML.getIdPConfig idpid
  authorizeIdP zusr idp
  pure idp

idpGetAll :: Maybe UserId -> Spar IdPList
idpGetAll zusr = withDebugLog "idpGetAll" (const Nothing) $ do
  teamid <- getZUsrOwnedTeam zusr
  _idplProviders <- wrapMonadClientWithEnv $ Data.getIdPConfigsByTeam teamid
  pure IdPList{..}

idpDelete :: Maybe UserId -> SAML.IdPId -> Spar NoContent
idpDelete zusr idpid = withDebugLog "idpDelete" (const Nothing) $ do
    idp <- SAML.getIdPConfig idpid
    authorizeIdP zusr idp
    wrapMonadClient $ Data.deleteIdPConfig idpid (idp ^. SAML.idpMetadata . SAML.edIssuer) (idp ^. SAML.idpExtraInfo)
    return NoContent

-- | We generate a new UUID for each IdP used as IdPConfig's path, thereby ensuring uniqueness.
idpCreate :: Maybe UserId -> SAML.IdPMetadata -> Spar IdP
idpCreate zusr idpmeta = withDebugLog "idpCreate" (Just . show . (^. SAML.idpId)) $ do
  teamid <- getZUsrOwnedTeam zusr
  idp <- validateNewIdP idpmeta teamid
  SAML.storeIdPConfig idp
  pure idp

withDebugLog :: SAML.SP m => String -> (a -> Maybe String) -> m a -> m a
withDebugLog msg showval action = do
  SAML.logger SAML.Debug $ "entering " ++ msg
  val <- action
  let mshowedval = showval val
  SAML.logger SAML.Debug $ "leaving " ++ msg ++ mconcat [": " ++ fromJust mshowedval | isJust mshowedval]
  pure val

-- | Called by get, put, delete handlers.
authorizeIdP :: (HasCallStack, MonadError SparError m, SAML.SP m, Intra.MonadSparToBrig m)
             => Maybe UserId -> IdP -> m ()
authorizeIdP zusr idp = do
  teamid <- getZUsrOwnedTeam zusr
  when (teamid /= idp ^. SAML.idpExtraInfo) $ throwSpar SparNotInTeam

-- | Called by post handler, and by 'authorizeIdP'.
getZUsrOwnedTeam :: (HasCallStack, MonadError SparError m, SAML.SP m, Intra.MonadSparToBrig m)
            => Maybe UserId -> m TeamId
getZUsrOwnedTeam Nothing = throwSpar SparMissingZUsr
getZUsrOwnedTeam (Just uid) = do
  usr <- Intra.getUser uid
  case Brig.userTeam =<< usr of
    Nothing -> throwSpar SparNotInTeam
    Just teamid -> teamid <$ Intra.assertIsTeamOwner uid teamid


-- | Check that issuer is fresh (see longer comment in source) and request URI is https.
--
-- FUTUREWORK: move this to the saml2-web-sso package.  (same probably goes for get, create,
-- update, delete of idps.)
validateNewIdP :: forall m. (HasCallStack, m ~ Spar)
               => SAML.IdPMetadata -> TeamId -> m IdP
validateNewIdP _idpMetadata _idpExtraInfo = do
  _idpId <- SAML.IdPId <$> SAML.createUUID

  let requri = _idpMetadata ^. SAML.edRequestURI
  unless (requri ^. URI.uriSchemeL == URI.Scheme "https") $ do
    throwSpar (SparNewIdPWantHttps . cs . SAML.renderURI $ requri)

  wrapMonadClient (Data.getIdPIdByIssuer (_idpMetadata ^. SAML.edIssuer)) >>= \case
    Nothing -> pure ()
    Just _ -> throwSpar SparNewIdPAlreadyInUse
    -- each idp (issuer) can only be created once.  if you want to update (one of) your team's
    -- idp(s), either use put (not implemented), or delete the old one before creating a new one
    -- (already works, even though it may create a brief time window in which users experience
    -- broken login behavior).
    --
    -- rationale: the issuer is how the idp self-identifies.  we can't allow the same idp to serve
    -- two teams because of implicit user creation: if an unknown user arrives, we use the
    -- idp-to-team mapping to decide which team to create the user in.  if we wanted to trust the
    -- idp to decide this for us, we would have to think of a way to prevent rogue idps from
    -- creating users in victim teams.

  pure SAML.IdPConfig {..}
