{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Spar.App
  ( Spar(..)
  , Env(..)
  , condenseLogMsg
  , toLevel
  , wrapMonadClientWithEnv
  , wrapMonadClient
  , verdictHandler
  , insertUser
  ) where

import Imports
import Bilge
import Brig.Types (Name)
import Cassandra
import Control.Exception (assert)
import Control.Lens hiding ((.=))
import Control.Monad.Except
import Data.Aeson as Aeson (encode, object, (.=))
import Data.EitherR (fmapL)
import Data.Id
import Data.String.Conversions
import SAML2.Util (renderURI)
import SAML2.WebSSO hiding (UserRef(..))
import Servant
import Servant.Server (errReasonPhrase, errBody)
import Spar.Orphans ()
import Spar.API.Swagger ()
import Spar.Error
import Spar.Types
import URI.ByteString as URI
import Web.Cookie (SetCookie, renderSetCookie)

import qualified Cassandra as Cas
import qualified Control.Monad.Catch as Catch
import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as ST
import qualified Data.UUID.V4 as UUID
import qualified Network.Wai.Utilities.Error as Wai
import qualified SAML2.WebSSO as SAML
import qualified Spar.Data as Data
import qualified Spar.Intra.Brig as Intra
import qualified Spar.Intra.Galley as Intra
import qualified System.Logger as Log


newtype Spar a = Spar { fromSpar :: ReaderT Env (ExceptT SparError IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadError SparError)

data Env = Env
  { sparCtxOpts         :: Opts
  , sparCtxLogger       :: Log.Logger
  , sparCtxCas          :: Cas.ClientState
  , sparCtxHttpManager  :: Bilge.Manager
  , sparCtxHttpBrig     :: Bilge.Request
  , sparCtxHttpGalley   :: Bilge.Request
  , sparCtxRequestId    :: RequestId
  }

instance HasConfig Spar where
  getConfig = asks (saml . sparCtxOpts)

instance HasNow Spar where
instance HasCreateUUID Spar where
instance HasLogger Spar where
  -- FUTUREWORK: optionally use 'field' to index user or idp ids for easier logfile processing.
  logger (toLevel -> lv) mg = do
    lg <- asks sparCtxLogger
    reqid <- asks sparCtxRequestId
    let fields, mg' :: Log.Msg -> Log.Msg
        fields = Log.field "request" (unRequestId reqid)
        mg'    = Log.msg . condenseLogMsg . cs $ mg
    Spar . Log.log lg lv $ fields Log.~~ mg'

condenseLogMsg :: ST -> ST
condenseLogMsg = ST.intercalate " "
               . filter (/= "")
               . map ST.strip
               . ST.split (`elem` (" \n\r\t\v\f" :: [Char]))

toLevel :: SAML.Level -> Log.Level
toLevel = \case
  SAML.Fatal -> Log.Fatal
  SAML.Error -> Log.Error
  SAML.Warn  -> Log.Warn
  SAML.Info  -> Log.Info
  SAML.Debug -> Log.Debug
  SAML.Trace -> Log.Trace

instance SPStoreID AuthnRequest Spar where
  storeID i r = wrapMonadClientWithEnv $ Data.storeAReqID i r
  unStoreID r = wrapMonadClient        $ Data.unStoreAReqID r
  isAliveID r = wrapMonadClient        $ Data.isAliveAReqID r

instance SPStoreID Assertion Spar where
  storeID i r = wrapMonadClientWithEnv $ Data.storeAssID i r
  unStoreID r = wrapMonadClient        $ Data.unStoreAssID r
  isAliveID r = wrapMonadClient        $ Data.isAliveAssID r

instance SPStoreIdP SparError Spar where
  type IdPConfigExtra Spar = TeamId

  storeIdPConfig :: IdPConfig TeamId -> Spar ()
  storeIdPConfig idp = wrapMonadClient $ Data.storeIdPConfig idp

  getIdPConfig :: IdPId -> Spar (IdPConfig TeamId)
  getIdPConfig = (>>= maybe (throwSpar SparNotFound) pure) . wrapMonadClientWithEnv . Data.getIdPConfig

  getIdPConfigByIssuer :: Issuer -> Spar (IdPConfig TeamId)
  getIdPConfigByIssuer = (>>= maybe (throwSpar SparNotFound) pure) . wrapMonadClientWithEnv . Data.getIdPConfigByIssuer

-- | 'wrapMonadClient' with an 'Env' in a 'ReaderT', and exceptions.
wrapMonadClientWithEnv :: forall a. ReaderT Data.Env (ExceptT TTLError Cas.Client) a -> Spar a
wrapMonadClientWithEnv action = do
  denv <- Data.mkEnv <$> (sparCtxOpts <$> ask) <*> (fromTime <$> getNow)
  either (throwSpar . SparCassandraTTLError) pure =<< wrapMonadClient (runExceptT $ action `runReaderT` denv)

-- | Call a cassandra command in the 'Spar' monad.  Catch all exceptions and re-throw them as 500 in
-- Handler.
wrapMonadClient :: Cas.Client a -> Spar a
wrapMonadClient action = do
  Spar $ do
    ctx <- asks sparCtxCas
    runClient ctx action `Catch.catch`
      (throwSpar . SparCassandraError . cs . show @SomeException)

insertUser :: SAML.UserRef -> UserId -> Spar ()
insertUser uref uid = wrapMonadClient $ Data.insertUser uref uid

-- | Look up user locally, then in brig, then return the 'UserId'.  If either lookup fails, return
-- 'Nothing'.  See also: 'Spar.App.createUser'.
--
-- ASSUMPTIONS: User creation on brig/galley is idempotent.  Any incomplete creation (because of
-- brig or galley crashing) will cause the lookup here to yield invalid user.
getUser :: SAML.UserRef -> Spar (Maybe UserId)
getUser uref = do
  muid <- wrapMonadClient $ Data.getUser uref
  case muid of
    Nothing -> pure Nothing
    Just uid -> do
      itis <- Intra.isTeamUser uid
      pure $ if itis then Just uid else Nothing

-- | Create a fresh 'Data.Id.UserId', store it on C* locally together with 'SAML.UserRef', then
-- create user on brig with that 'UserId'.  See also: 'Spar.App.getUser'.
--
-- The manual for the team admin should say this: when deleting a user, delete it on the IdP first,
-- then delete it on the team admin page in wire.  If a user is deleted in wire but not in the IdP,
-- it will be recreated on the next successful login attempt.
--
-- When an sso login succeeds for a user that is marked as deleted in brig, it is recreated by spar.
-- This is necessary because brig does not talk to spar when deleting users, and we may have
-- 'UserId' records on spar that are deleted on brig.  Without this lenient behavior, there would be
-- no way for admins to reuse a 'SAML.UserRef' if it has ever been associated with a deleted user in
-- the past.
--
-- FUTUREWORK: once we support <https://github.com/wireapp/hscim scim>, brig will refuse to delete
-- users that have an sso id, unless the request comes from spar.  then we can make users
-- undeletable in the team admin page, and ask admins to go talk to their IdP system.
createUser :: SAML.UserRef -> Maybe Name -> Spar UserId
createUser suid mbName = do
  buid <- Id <$> liftIO UUID.nextRandom
  teamid <- (^. idpExtraInfo) <$> getIdPConfigByIssuer (suid ^. uidTenant)
  insertUser suid buid
  buid' <- Intra.createUser suid buid teamid mbName
  assert (buid == buid') $ pure buid

-- | Check if 'UserId' is in the team that hosts the idp that owns the 'UserRef'.  If so, write the
-- 'UserRef' into the 'UserIdentity'.  Otherwise, throw an error.
bindUser :: UserId -> SAML.UserRef -> Spar UserId
bindUser buid userref = do
  teamid <- (^. idpExtraInfo) <$> getIdPConfigByIssuer (userref ^. uidTenant)
  uteamid <- Intra.getUserTeam buid
  unless (uteamid == Just teamid)
    (throwSpar . SparBindFromWrongOrNoTeam . cs . show $ uteamid)
  insertUser userref buid
  Intra.bindUser buid userref >>= \case
    True  -> pure buid
    False -> do
      SAML.logger SAML.Warn $ "SparBindUserDisappearedFromBrig: " <> show buid
      throwSpar SparBindUserDisappearedFromBrig


instance SPHandler SparError Spar where
  type NTCTX Spar = Env
  nt ctx (Spar action) = Handler . ExceptT . fmap (fmapL sparToServantErr) . runExceptT $ runReaderT action ctx

instance MonadHttp Spar where
  getManager = asks sparCtxHttpManager

instance Intra.MonadSparToBrig Spar where
  call modreq = do
    req <- asks sparCtxHttpBrig
    httpLbs req modreq

instance Intra.MonadSparToGalley Spar where
  call modreq = do
    req <- asks sparCtxHttpGalley
    httpLbs req modreq


-- | The from of the response on the finalize-login request depends on the verdict (denied or
-- granted), plus the choice that the client has made during the initiate-login request.  Here we
-- call either 'verdictHandlerWeb' or 'verdictHandlerMobile', resp., on the 'SAML.AccessVerdict'.
--
-- NB: there are at least two places in the 'SAML.AuthnResponse' that can contain the request id:
-- the response header and every assertion.  Since saml2-web-sso validation guarantees that the
-- signed in-response-to info in the assertions matches the unsigned in-response-to field in the
-- 'SAML.Response', and fills in the response id in the header if missing, we can just go for the
-- latter.
verdictHandler :: HasCallStack => Maybe BindCookie -> SAML.AuthnResponse -> SAML.AccessVerdict -> Spar SAML.ResponseVerdict
verdictHandler cky aresp verdict = do
  -- [3/4.1.4.2]
  -- <SubjectConfirmation> [...] If the containing message is in response to an <AuthnRequest>, then
  -- the InResponseTo attribute MUST match the request's ID.
  reqid <- either (throwSpar . SparNoRequestRefInResponse . cs) pure $ SAML.rspInResponseTo aresp
  format :: Maybe VerdictFormat <- wrapMonadClient $ Data.getVerdictFormat reqid
  case format of
    Just (VerdictFormatWeb)
      -> verdictHandlerResult cky verdict >>= verdictHandlerWeb
    Just (VerdictFormatMobile granted denied)
      -> verdictHandlerResult cky verdict >>= verdictHandlerMobile granted denied
    Nothing -> throwError $ SAML.BadSamlResponse "AuthRequest seems to have disappeared (could not find verdict format)."
               -- (this shouldn't happen too often, see 'storeVerdictFormat')

data VerdictHandlerResult
  = VerifyHandlerGranted { _vhrCookie :: SetCookie, _vhrUserId :: UserId }
  | VerifyHandlerDenied { _vhrReasons :: [SAML.DeniedReason] }
  | VerifyHandlerError { _vhrLabel :: ST, _vhrMessage :: ST }

catchVerdictErrors :: Spar VerdictHandlerResult -> Spar VerdictHandlerResult
catchVerdictErrors = (`catchError` pure . hndlr)
  where
    hndlr :: SparError -> VerdictHandlerResult
    hndlr err = case sparToWaiError err of
      Right (werr :: Wai.Error) -> VerifyHandlerError (cs $ Wai.label werr) (cs $ Wai.message werr)
      Left (serr :: ServantErr) -> VerifyHandlerError "unknown-error" (cs (errReasonPhrase serr) <> " " <> cs (errBody serr))

verdictHandlerResult :: HasCallStack => Maybe BindCookie -> SAML.AccessVerdict -> Spar VerdictHandlerResult
verdictHandlerResult bindCky = catchVerdictErrors . \case
  denied@(SAML.AccessDenied reasons) -> do
    SAML.logger SAML.Debug (show denied)
    pure $ VerifyHandlerDenied reasons

  granted@(SAML.AccessGranted userref) -> do
    uid :: UserId <- do
      SAML.logger SAML.Debug (show granted)
      viaBindCookie <- maybe (pure Nothing) (wrapMonadClient . Data.lookupBindCookie) bindCky
      viaSparCass   <- getUser userref
        -- race conditions: if the user has been created on spar, but not on brig, 'getUser'
        -- returns 'Nothing'.  this is ok assuming 'createUser', 'bindUser' (called below) are
        -- idempotent.

      case (viaBindCookie, viaSparCass) of
        (Nothing,  Nothing)   -> createUser userref Nothing  -- first sso authentication
        (Nothing,  Just uid)  -> pure uid                -- sso re-authentication
        (Just uid, Nothing)   -> bindUser uid userref    -- bind existing user (non-sso or sso) to ssoid
        (Just uid, Just uid')
          | uid == uid' -> pure uid                      -- redundant binding (no change to brig or spar)
          | otherwise -> throwSpar SparBindUserRefTaken  -- attempt to use ssoid for a second wire user

    cky :: SetCookie <- Intra.ssoLogin uid
      -- (creating users is synchronous and does a quorum vote, so there is no race condition here.)
    pure $ VerifyHandlerGranted cky uid

-- | If the client is web, it will be served with an HTML page that it can process to decide whether
-- to log the user in or show an error.
--
-- The HTML page is empty and has two ways to communicate the verdict to the js app:
-- - A title element with contents @wire:sso:<outcome>@.  This is chosen to be easily parseable and
--   not be the title of any page sent by the IdP while it negotiates with the user.
-- - The page broadcasts a message to '*', to be picked up by the app.
verdictHandlerWeb :: HasCallStack => VerdictHandlerResult -> Spar SAML.ResponseVerdict
verdictHandlerWeb = pure . \case
    VerifyHandlerGranted cky _uid -> successPage cky
    VerifyHandlerDenied reasons   -> forbiddenPage "forbidden" (explainDeniedReason <$> reasons)
    VerifyHandlerError lbl msg    -> forbiddenPage lbl [msg]
  where
    forbiddenPage :: ST -> [ST] -> SAML.ResponseVerdict
    forbiddenPage errlbl reasons = ServantErr
      { errHTTPCode     = 200
      , errReasonPhrase = cs errlbl  -- (not sure what this is used for)
      , errBody         = easyHtml $
                          "<head>" <>
                          "  <title>wire:sso:error:" <> cs errlbl <> "</title>" <>
                          "   <script type=\"text/javascript\">" <>
                          "       const receiverOrigin = '*';" <>
                          "       window.opener.postMessage(" <> Aeson.encode errval <> ", receiverOrigin);" <>
                          "   </script>" <>
                          "</head>"
      , errHeaders      = []
      }
      where
        errval = object [ "type" .= ("AUTH_ERROR" :: ST)
                        , "payload" .= object [ "label" .= ("forbidden" :: ST)
                                              , "errors" .= reasons
                                              ]
                        ]

    successPage :: SetCookie -> SAML.ResponseVerdict
    successPage cky = ServantErr
      { errHTTPCode     = 200
      , errReasonPhrase = "success"
      , errBody         = easyHtml $
                          "<head>" <>
                          "  <title>wire:sso:success</title>" <>
                          "   <script type=\"text/javascript\">" <>
                          "       const receiverOrigin = '*';" <>
                          "       window.opener.postMessage({type: 'AUTH_SUCCESS'}, receiverOrigin);" <>
                          "   </script>" <>
                          "</head>"
      , errHeaders      = [("Set-Cookie", cs . Builder.toLazyByteString . renderSetCookie $ cky)]
      }

easyHtml :: LBS -> LBS
easyHtml doc =
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" <>
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">" <>
  "<html xml:lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\">" <> doc <> "</html>"

-- | If the client is mobile, it has picked error and success redirect urls (see
-- 'mkVerdictGrantedFormatMobile', 'mkVerdictDeniedFormatMobile'); variables in these URLs are here
-- substituted and the client is redirected accordingly.
verdictHandlerMobile :: HasCallStack => URI.URI -> URI.URI -> VerdictHandlerResult -> Spar SAML.ResponseVerdict
verdictHandlerMobile granted denied = \case
    VerifyHandlerGranted cky uid
      -> mkVerdictGrantedFormatMobile granted cky uid &
         either (throwSpar . SparCouldNotSubstituteSuccessURI . cs)
                (pure . successPage cky)
    VerifyHandlerDenied reasons
      -> mkVerdictDeniedFormatMobile denied "forbidden" &
         either (throwSpar . SparCouldNotSubstituteFailureURI . cs)
                (pure . forbiddenPage "forbidden" (explainDeniedReason <$> reasons))
    VerifyHandlerError lbl msg
      -> mkVerdictDeniedFormatMobile denied lbl &
         either (throwSpar . SparCouldNotSubstituteFailureURI . cs)
                (pure . forbiddenPage lbl [msg])
  where
    forbiddenPage :: ST -> [ST] -> URI.URI -> SAML.ResponseVerdict
    forbiddenPage errlbl errs uri = err303
      { errReasonPhrase = cs errlbl
      , errHeaders = [ ("Location", cs $ renderURI uri)
                     , ("Content-Type", "application/json")
                     ]
      , errBody = Aeson.encode errs
      }

    successPage :: SetCookie -> URI.URI -> SAML.ResponseVerdict
    successPage cky uri = err303
      { errReasonPhrase = "success"
      , errHeaders = [ ("Location", cs $ renderURI uri)
                     , ("Set-Cookie", cs . Builder.toLazyByteString . renderSetCookie $ cky)
                     ]
      }
