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

import Brig.Types.User as Brig
import Control.Monad.Except
import Data.Id
import Data.Maybe (isJust, fromJust)
import Data.Proxy
import Data.String.Conversions
import Data.Time
import GHC.Stack
import Lens.Micro
import Servant
import Servant.Swagger
import Spar.API.Instances ()
import Spar.API.Swagger ()
import Spar.API.Test
import Spar.API.Types
import Spar.App
import Spar.Error
import Spar.Options
import Spar.Types

import qualified Data.ByteString as SBS
import qualified SAML2.WebSSO as SAML
import qualified Spar.Data as Data
import qualified Spar.Intra.Brig as Intra
import qualified URI.ByteString as URI


app :: Env -> Application
app ctx = SAML.setHttpCachePolicy
        $ serve (Proxy @API) (hoistServer (Proxy @API) (SAML.nt @SparError @Spar ctx) (api $ sparCtxOpts ctx) :: Server API)

api :: Opts -> ServerT API Spar
api opts = apiSSO opts :<|> apiIDP :<|> apiINTERNAL

apiSSO :: Opts -> ServerT APISSO Spar
apiSSO opts
     = pure (toSwagger (Proxy @OutsideWorldAPI))
  :<|> SAML.meta appName sparRequestIssuer sparResponseURI
  :<|> authreqPrecheck
  :<|> authreq (maxttlAuthreqDiffTime opts)
  :<|> SAML.authresp sparRequestIssuer sparResponseURI (SAML.HandleVerdictRaw verdictHandler)

sparRequestIssuer :: SAML.HasConfig m => SAML.IdPId -> m SAML.Issuer
sparRequestIssuer = fmap SAML.Issuer <$> SAML.getSsoURI' p p
  where
    p = Proxy @("initiate-login" :> SAML.APIAuthReq)
      -- we can't use the 'APIAuthReq' route here because it has extra query params that translate
      -- into function arguments to 'safeLink', but 'SAML.getSsoURI'' does not support that.

sparResponseURI :: SAML.HasConfig m => SAML.IdPId -> m URI.URI
sparResponseURI = SAML.getSsoURI' (Proxy @APISSO) (Proxy @APIAuthResp)


apiIDP :: ServerT APIIDP Spar
apiIDP
     = idpGet
  :<|> idpGetAll
  :<|> idpCreate
  :<|> idpDelete

apiINTERNAL :: ServerT APIINTERNAL Spar
apiINTERNAL
     = pure NoContent
  :<|> integrationTests


appName :: ST
appName = "spar"

authreqPrecheck :: Maybe URI.URI -> Maybe URI.URI -> SAML.IdPId -> Spar NoContent
authreqPrecheck msucc merr idpid = validateAuthreqParams msucc merr
                                *> SAML.getIdPConfig idpid
                                *> return NoContent

authreq :: NominalDiffTime -> Maybe URI.URI -> Maybe URI.URI -> SAML.IdPId -> Spar (SAML.FormRedirect SAML.AuthnRequest)
authreq authreqttl msucc merr idpid = do
  vformat <- validateAuthreqParams msucc merr
  form@(SAML.FormRedirect _ ((^. SAML.rqID) -> reqid)) <- SAML.authreq authreqttl sparRequestIssuer idpid
  wrapMonadClient $ Data.storeVerdictFormat authreqttl reqid vformat
  pure form

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

type ZUsr = Maybe UserId

idpGet :: ZUsr -> SAML.IdPId -> Spar IdP
idpGet zusr idpid = withDebugLog "idpGet" (Just . show . (^. SAML.idpId)) $ do
  idp <- SAML.getIdPConfig idpid
  authorizeIdP zusr idp
  pure idp

idpGetAll :: ZUsr -> Spar IdPList
idpGetAll zusr = withDebugLog "idpGetAll" (const Nothing) $ do
  teamid <- getZUsrOwnedTeam zusr
  _idplProviders <- wrapMonadClientWithEnv $ Data.getIdPConfigsByTeam teamid
  pure IdPList{..}

idpDelete :: ZUsr -> SAML.IdPId -> Spar NoContent
idpDelete zusr idpid = withDebugLog "idpDelete" (const Nothing) $ do
    idp <- SAML.getIdPConfig idpid
    authorizeIdP zusr idp
    wrapMonadClient $ Data.deleteIdPConfig idpid (idp ^. SAML.idpMetadata . SAML.edIssuer) (idp ^. SAML.idpExtraInfo)
    return NoContent

-- | We generate a new UUID for each IdP used as IdPConfig's path, thereby ensuring uniqueness.
idpCreate :: ZUsr -> SAML.IdPMetadata -> Spar IdP
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
             => ZUsr -> IdP -> m ()
authorizeIdP zusr idp = do
  teamid <- getZUsrOwnedTeam zusr
  when (teamid /= idp ^. SAML.idpExtraInfo) $ throwSpar SparNotInTeam

-- | Called by post handler, and by 'authorizeIdP'.
getZUsrOwnedTeam :: (HasCallStack, MonadError SparError m, SAML.SP m, Intra.MonadSparToBrig m)
            => ZUsr -> m TeamId
getZUsrOwnedTeam Nothing = throwSpar SparMissingZUsr
getZUsrOwnedTeam (Just uid) = do
  usr <- Intra.getUser uid
  case Brig.userTeam =<< usr of
    Nothing -> throwSpar SparNotInTeam
    Just teamid -> teamid <$ Intra.assertIsTeamOwner uid teamid


-- | FUTUREWORK: move this to the saml2-web-sso package.  (same probably goes for get, create,
-- update, delete of idps.)
validateNewIdP :: forall m. (HasCallStack, m ~ Spar)
               => SAML.IdPMetadata -> TeamId -> m IdP
validateNewIdP _idpMetadata _idpExtraInfo = do
  _idpId <- SAML.IdPId <$> SAML.createUUID

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
