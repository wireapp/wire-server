{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

-- | The 'Spar' monad and a set of actions (e.g. 'createUser') that can be performed in it.
module Spar.App
  ( Spar (..),
    Env (..),
    toLevel,
    wrapMonadClientWithEnv,
    wrapMonadClient,
    verdictHandler,
    getUserByUref,
    getUserByScimExternalId,
    insertUser,
    autoprovisionSamlUser,
    autoprovisionSamlUserWithId,
    validateEmailIfExists,
    errorPage,
  )
where

import Bilge
import Brig.Types (ManagedBy (..), userTeam)
import Brig.Types.Intra (AccountStatus (..), accountStatus, accountUser)
import Cassandra
import qualified Cassandra as Cas
import Control.Exception (assert)
import Control.Lens hiding ((.=))
import qualified Control.Monad.Catch as Catch
import Control.Monad.Except
import Data.Aeson as Aeson (encode, object, (.=))
import Data.Aeson.Text as Aeson (encodeToLazyText)
import qualified Data.ByteString.Builder as Builder
import Data.Id
import Data.String.Conversions
import Data.Text.Ascii (encodeBase64, toText)
import qualified Data.Text.Lazy as LT
import qualified Data.UUID.V4 as UUID
import Imports hiding (log)
import qualified Network.HTTP.Types.Status as Http
import qualified Network.Wai.Utilities.Error as Wai
import SAML2.Util (renderURI)
import SAML2.WebSSO
  ( Assertion (..),
    AuthnRequest (..),
    HasConfig (..),
    HasCreateUUID (..),
    HasLogger (..),
    HasNow (..),
    IdPId (..),
    Issuer (..),
    SPHandler (..),
    SPStoreID (..),
    SPStoreIdP (..),
    UnqualifiedNameID (..),
    explainDeniedReason,
    fromTime,
    idpExtraInfo,
    idpId,
    uidTenant,
  )
import qualified SAML2.WebSSO as SAML
import Servant
import qualified Servant.Multipart as Multipart
import Spar.API.Swagger ()
import qualified Spar.Data as Data
import Spar.Error
import qualified Spar.Intra.Brig as Intra
import qualified Spar.Intra.Galley as Intra
import Spar.Orphans ()
import Spar.Scim.Types (ValidExternalId (..))
import Spar.Types
import qualified System.Logger as Log
import System.Logger.Class (MonadLogger (log))
import URI.ByteString as URI
import Web.Cookie (SetCookie, renderSetCookie)
import Wire.API.User.Identity (Email (..))

newtype Spar a = Spar {fromSpar :: ReaderT Env (ExceptT SparError IO) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadError SparError)

data Env = Env
  { sparCtxOpts :: Opts,
    sparCtxLogger :: Log.Logger,
    sparCtxCas :: Cas.ClientState,
    sparCtxHttpManager :: Bilge.Manager,
    sparCtxHttpBrig :: Bilge.Request,
    sparCtxHttpGalley :: Bilge.Request,
    sparCtxRequestId :: RequestId
  }

instance HasConfig Spar where
  getConfig = asks (saml . sparCtxOpts)

instance HasNow Spar

instance HasCreateUUID Spar

instance HasLogger Spar where
  -- FUTUREWORK: optionally use 'field' to index user or idp ids for easier logfile processing.
  logger lv = log (toLevel lv) . Log.msg

instance MonadLogger Spar where
  log level mg = do
    lg <- asks sparCtxLogger
    reqid <- asks sparCtxRequestId
    let fields = Log.field "request" (unRequestId reqid)
    Spar . Log.log lg level $ fields Log.~~ mg

toLevel :: SAML.Level -> Log.Level
toLevel = \case
  SAML.Fatal -> Log.Fatal
  SAML.Error -> Log.Error
  SAML.Warn -> Log.Warn
  SAML.Info -> Log.Info
  SAML.Debug -> Log.Debug
  SAML.Trace -> Log.Trace

instance SPStoreID AuthnRequest Spar where
  storeID i r = wrapMonadClientWithEnv $ Data.storeAReqID i r
  unStoreID r = wrapMonadClient $ Data.unStoreAReqID r
  isAliveID r = wrapMonadClient $ Data.isAliveAReqID r

instance SPStoreID Assertion Spar where
  storeID i r = wrapMonadClientWithEnv $ Data.storeAssID i r
  unStoreID r = wrapMonadClient $ Data.unStoreAssID r
  isAliveID r = wrapMonadClient $ Data.isAliveAssID r

instance SPStoreIdP SparError Spar where
  type IdPConfigExtra Spar = WireIdP

  storeIdPConfig :: IdP -> Spar ()
  storeIdPConfig idp = wrapMonadClient $ Data.storeIdPConfig idp

  getIdPConfig :: IdPId -> Spar IdP
  getIdPConfig = (>>= maybe (throwSpar SparIdPNotFound) pure) . wrapMonadClientWithEnv . Data.getIdPConfig

  getIdPConfigByIssuer :: Issuer -> Spar IdP
  getIdPConfigByIssuer = (>>= maybe (throwSpar SparIdPNotFound) pure) . wrapMonadClientWithEnv . Data.getIdPConfigByIssuer

-- | 'wrapMonadClient' with an 'Env' in a 'ReaderT', and exceptions. If you
-- don't need either of those, 'wrapMonadClient' will suffice.
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
    runClient ctx action
      `Catch.catch` (throwSpar . SparCassandraError . cs . show @SomeException)

insertUser :: SAML.UserRef -> UserId -> Spar ()
insertUser uref uid = wrapMonadClient $ Data.insertSAMLUser uref uid

-- | Look up user locally in table @spar.user@ or @spar.scim_user@ (depending on the
-- argument), then in brig, then return the 'UserId'.  If either lookup fails, or user is not
-- in a team, return 'Nothing'.
--
-- It makes sense to require that users are required to be team members: both IdPs and SCIM
-- tokens are created in the context of teams, and the only way for users to be created is as
-- team members.  If a user is not a team member, it cannot have been created using SAML or
-- SCIM.
--
-- If a user has been created via scim invite (ie., no IdP present), and has status
-- 'PendingInvitation', its 'UserId' will be returned here, since for SCIM purposes it is an
-- existing (if inactive) user.  If 'getUser' is called during SAML authentication, this may
-- cause an inactive user to log in, but that's ok: `PendingActivation` means that email and
-- password handshake have not been completed; it's still ok for the user to gain access to
-- the team with valid SAML credentials.
--
-- FUTUREWORK: Remove and reinstatate getUser, in AuthID refactoring PR.  (in https://github.com/wireapp/wire-server/pull/1410, undo https://github.com/wireapp/wire-server/pull/1418)
getUserByUref :: SAML.UserRef -> Spar (Maybe UserId)
getUserByUref uref = do
  muid <- wrapMonadClient $ Data.getSAMLUser uref
  case muid of
    Nothing -> pure Nothing
    Just uid -> do
      let withpending = Intra.WithPendingInvitations -- see haddocks above
      itis <- isJust <$> Intra.getBrigUserTeam withpending uid
      pure $ if itis then Just uid else Nothing

-- FUTUREWORK: Remove and reinstatate getUser, in AuthID refactoring PR
getUserByScimExternalId :: TeamId -> Email -> Spar (Maybe UserId)
getUserByScimExternalId tid email = do
  muid <- wrapMonadClient $ (Data.lookupScimExternalId tid email)
  case muid of
    Nothing -> pure Nothing
    Just uid -> do
      let withpending = Intra.WithPendingInvitations -- see haddocks above
      itis <- isJust <$> Intra.getBrigUserTeam withpending uid
      pure $ if itis then Just uid else Nothing

-- | Create a fresh 'UserId', store it on C* locally together with 'SAML.UserRef', then
-- create user on brig.
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
createSamlUserWithId :: UserId -> SAML.UserRef -> ManagedBy -> Spar ()
createSamlUserWithId buid suid managedBy = do
  teamid <- (^. idpExtraInfo . wiTeam) <$> getIdPConfigByIssuer (suid ^. uidTenant)
  uname <- either (throwSpar . SparBadUserName . cs) pure $ Intra.mkUserName Nothing (UrefOnly suid)
  buid' <- Intra.createBrigUserSAML suid buid teamid uname managedBy
  assert (buid == buid') $ pure ()
  insertUser suid buid

-- | If the team has no scim token, call 'createSamlUser'.  Otherwise, raise "invalid
-- credentials".
autoprovisionSamlUser :: SAML.UserRef -> ManagedBy -> Spar UserId
autoprovisionSamlUser suid managedBy = do
  buid <- Id <$> liftIO UUID.nextRandom
  autoprovisionSamlUserWithId buid suid managedBy
  pure buid

-- | Like 'autoprovisionSamlUser', but for an already existing 'UserId'.
autoprovisionSamlUserWithId :: UserId -> SAML.UserRef -> ManagedBy -> Spar ()
autoprovisionSamlUserWithId buid suid managedBy = do
  idp <- getIdPConfigByIssuer (suid ^. uidTenant)
  unless (isNothing $ idp ^. idpExtraInfo . wiReplacedBy) $ do
    throwSpar $ SparCannotCreateUsersOnReplacedIdP (cs . SAML.idPIdToST $ idp ^. idpId)
  let teamid = idp ^. idpExtraInfo . wiTeam
  scimtoks <- wrapMonadClient $ Data.getScimTokens teamid
  if null scimtoks
    then do
      createSamlUserWithId buid suid managedBy
      validateEmailIfExists buid suid
    else
      throwError . SAML.Forbidden $
        "bad credentials (note that your team uses SCIM, "
          <> "which disables saml auto-provisioning)"

-- | If user's 'NameID' is an email address and the team has email validation for SSO enabled,
-- make brig initiate the email validate procedure.
validateEmailIfExists :: UserId -> SAML.UserRef -> Spar ()
validateEmailIfExists uid = \case
  (SAML.UserRef _ (view SAML.nameID -> UNameIDEmail email)) -> doValidate email
  _ -> pure ()
  where
    doValidate :: SAML.Email -> Spar ()
    doValidate email = do
      enabled <- do
        tid <- Intra.getBrigUserTeam Intra.NoPendingInvitations uid
        maybe (pure False) Intra.isEmailValidationEnabledTeam tid
      when enabled $ do
        Intra.updateEmail uid (Intra.emailFromSAML email)

-- | Check if 'UserId' is in the team that hosts the idp that owns the 'UserRef'.  If so,
-- register a the user under its SAML credentials and write the 'UserRef' into the
-- 'UserIdentity'.  Otherwise, throw an error.
--
-- Before returning, change account status or fail if account is nto active or pending an
-- invitation.
bindUser :: UserId -> SAML.UserRef -> Spar UserId
bindUser buid userref = do
  oldStatus <- do
    let err :: Spar a
        err = throwSpar . SparBindFromWrongOrNoTeam . cs . show $ buid
    teamid :: TeamId <- (^. idpExtraInfo . wiTeam) <$> getIdPConfigByIssuer (userref ^. uidTenant)
    acc <- Intra.getBrigUserAccount Intra.WithPendingInvitations buid >>= maybe err pure
    teamid' :: TeamId <- userTeam (accountUser acc) & maybe err pure
    unless (teamid' == teamid) err
    pure (accountStatus acc)
  insertUser userref buid
  buid <$ do
    Intra.setBrigUserVeid buid (UrefOnly userref)
    let err = throwSpar . SparBindFromBadAccountStatus . cs . show
    case oldStatus of
      Active -> pure ()
      Suspended -> err oldStatus
      Deleted -> err oldStatus
      Ephemeral -> err oldStatus
      PendingInvitation -> Intra.setStatus buid Active

instance SPHandler SparError Spar where
  type NTCTX Spar = Env
  nt :: forall a. Env -> Spar a -> Handler a
  nt ctx (Spar action) = do
    err <- actionHandler
    throwErrorAsHandlerException err
    where
      actionHandler :: Handler (Either SparError a)
      actionHandler = liftIO $ runExceptT $ runReaderT action ctx
      throwErrorAsHandlerException :: Either SparError a -> Handler a
      throwErrorAsHandlerException (Left err) =
        sparToServerErrorWithLogging (sparCtxLogger ctx) err >>= throwError
      throwErrorAsHandlerException (Right a) = pure a

instance MonadHttp Spar where
  handleRequestWithCont req handler = do
    manager <- asks sparCtxHttpManager
    liftIO $ withResponse req manager handler

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
    Just (VerdictFormatWeb) ->
      verdictHandlerResult cky verdict >>= verdictHandlerWeb
    Just (VerdictFormatMobile granted denied) ->
      verdictHandlerResult cky verdict >>= verdictHandlerMobile granted denied
    Nothing -> throwSpar SparNoSuchRequest

-- (this shouldn't happen too often, see 'storeVerdictFormat')

data VerdictHandlerResult
  = VerifyHandlerGranted {_vhrCookie :: SetCookie, _vhrUserId :: UserId}
  | VerifyHandlerDenied {_vhrReasons :: [SAML.DeniedReason]}
  | VerifyHandlerError {_vhrLabel :: ST, _vhrMessage :: ST}
  deriving (Eq, Show)

verdictHandlerResult :: HasCallStack => Maybe BindCookie -> SAML.AccessVerdict -> Spar VerdictHandlerResult
verdictHandlerResult bindCky verdict = do
  result <- catchVerdictErrors $ verdictHandlerResultCore bindCky verdict
  SAML.logger SAML.Debug (show result)
  pure result

catchVerdictErrors :: Spar VerdictHandlerResult -> Spar VerdictHandlerResult
catchVerdictErrors = (`catchError` hndlr)
  where
    hndlr :: SparError -> Spar VerdictHandlerResult
    hndlr err = do
      logr <- asks sparCtxLogger
      waiErr <- renderSparErrorWithLogging logr err
      pure $ case waiErr of
        Right (werr :: Wai.Error) -> VerifyHandlerError (cs $ Wai.label werr) (cs $ Wai.message werr)
        Left (serr :: ServerError) -> VerifyHandlerError "unknown-error" (cs (errReasonPhrase serr) <> " " <> cs (errBody serr))

-- | If a user attempts to login presenting a new IdP issuer, but there is no entry in
-- @"spar.user"@ for her: lookup @"old_issuers"@ from @"spar.idp"@ for the new IdP, and
-- traverse the old IdPs in search for the old entry.  Return that old entry.
findUserWithOldIssuer :: SAML.UserRef -> Spar (Maybe (SAML.UserRef, UserId))
findUserWithOldIssuer (SAML.UserRef issuer subject) = do
  idp <- getIdPConfigByIssuer issuer
  let tryFind :: Maybe (SAML.UserRef, UserId) -> Issuer -> Spar (Maybe (SAML.UserRef, UserId))
      tryFind found@(Just _) _ = pure found
      tryFind Nothing oldIssuer = (uref,) <$$> getUserByUref uref
        where
          uref = SAML.UserRef oldIssuer subject
  foldM tryFind Nothing (idp ^. idpExtraInfo . wiOldIssuers)

-- | After a user has been found using 'findUserWithOldIssuer', update it everywhere so that
-- the old IdP is not needed any more next time.
moveUserToNewIssuer :: SAML.UserRef -> SAML.UserRef -> UserId -> Spar ()
moveUserToNewIssuer oldUserRef newUserRef uid = do
  wrapMonadClient $ Data.insertSAMLUser newUserRef uid
  Intra.setBrigUserVeid uid (UrefOnly newUserRef)
  wrapMonadClient $ Data.deleteSAMLUser oldUserRef

verdictHandlerResultCore :: HasCallStack => Maybe BindCookie -> SAML.AccessVerdict -> Spar VerdictHandlerResult
verdictHandlerResultCore bindCky = \case
  SAML.AccessDenied reasons -> do
    pure $ VerifyHandlerDenied reasons
  SAML.AccessGranted userref -> do
    uid :: UserId <- do
      viaBindCookie <- maybe (pure Nothing) (wrapMonadClient . Data.lookupBindCookie) bindCky
      viaSparCassandra <- getUserByUref userref
      -- race conditions: if the user has been created on spar, but not on brig, 'getUser'
      -- returns 'Nothing'.  this is ok assuming 'createUser', 'bindUser' (called below) are
      -- idempotent.
      viaSparCassandraOldIssuer <-
        if isJust viaSparCassandra
          then pure Nothing
          else findUserWithOldIssuer userref
      case (viaBindCookie, viaSparCassandra, viaSparCassandraOldIssuer) of
        -- This is the first SSO authentication, so we auto-create a user. We know the user
        -- has not been created via SCIM because then we would've ended up in the
        -- "reauthentication" branch, so we pass 'ManagedByWire'.
        (Nothing, Nothing, Nothing) -> autoprovisionSamlUser userref ManagedByWire
        -- If the user is only found under an old (previous) issuer, move it here.
        (Nothing, Nothing, Just (oldUserRef, uid)) -> moveUserToNewIssuer oldUserRef userref uid >> pure uid
        -- SSO re-authentication (the most common case).
        (Nothing, Just uid, _) -> pure uid
        -- Bind existing user (non-SSO or SSO) to ssoid
        (Just uid, Nothing, Nothing) -> bindUser uid userref
        (Just uid, Just uid', Nothing)
          -- Redundant binding (no change to Brig or Spar)
          | uid == uid' -> pure uid
          -- Attempt to use ssoid for a second Wire user
          | otherwise -> throwSpar SparBindUserRefTaken
        -- same two cases as above, but between last login and bind there was an issuer update.
        (Just uid, Nothing, Just (oldUserRef, uid'))
          | uid == uid' -> moveUserToNewIssuer oldUserRef userref uid >> pure uid
          | otherwise -> throwSpar SparBindUserRefTaken
        (Just _, Just _, Just _) ->
          -- to see why, consider the condition on the call to 'findUserWithOldIssuer' above.
          error "impossible."
    SAML.logger SAML.Debug ("granting sso login for " <> show uid)
    cky <- Intra.ssoLogin uid
    pure $ VerifyHandlerGranted cky uid

-- | If the client is web, it will be served with an HTML page that it can process to decide whether
-- to log the user in or show an error.
--
-- The HTML page is empty and has two ways to communicate the verdict to the js app:
-- - A title element with contents @wire:sso:<outcome>@.  This is chosen to be easily parseable and
--   not be the title of any page sent by the IdP while it negotiates with the user.
-- - The page broadcasts a message to '*', to be picked up by the app.
verdictHandlerWeb :: HasCallStack => VerdictHandlerResult -> Spar SAML.ResponseVerdict
verdictHandlerWeb =
  pure . \case
    VerifyHandlerGranted cky _uid -> successPage cky
    VerifyHandlerDenied reasons -> forbiddenPage "forbidden" (explainDeniedReason <$> reasons)
    VerifyHandlerError lbl msg -> forbiddenPage lbl [msg]
  where
    forbiddenPage :: ST -> [ST] -> SAML.ResponseVerdict
    forbiddenPage errlbl reasons =
      ServerError
        { errHTTPCode = 200,
          errReasonPhrase = cs errlbl, -- (not sure what this is used for)
          errBody =
            easyHtml $
              "<head>"
                <> "  <title>wire:sso:error:"
                <> cs errlbl
                <> "</title>"
                <> "   <script type=\"text/javascript\">"
                <> "       const receiverOrigin = '*';"
                <> "       window.opener.postMessage("
                <> Aeson.encodeToLazyText errval
                <> ", receiverOrigin);"
                <> "   </script>"
                <> "</head>",
          errHeaders =
            [ ("Content-Type", "text/html;charset=utf-8")
            ]
        }
      where
        errval =
          object
            [ "type" .= ("AUTH_ERROR" :: ST),
              "payload"
                .= object
                  [ "label" .= ("forbidden" :: ST),
                    "errors" .= reasons
                  ]
            ]
    successPage :: SetCookie -> SAML.ResponseVerdict
    successPage cky =
      ServerError
        { errHTTPCode = 200,
          errReasonPhrase = "success",
          errBody =
            easyHtml $
              "<head>"
                <> "  <title>wire:sso:success</title>"
                <> "   <script type=\"text/javascript\">"
                <> "       const receiverOrigin = '*';"
                <> "       window.opener.postMessage({type: 'AUTH_SUCCESS'}, receiverOrigin);"
                <> "   </script>"
                <> "</head>",
          errHeaders =
            [ ("Content-Type", "text/html;charset=utf-8"),
              ("Set-Cookie", cs . Builder.toLazyByteString . renderSetCookie $ cky)
            ]
        }

easyHtml :: LT -> LBS
easyHtml doc =
  cs $
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
      <> "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"
      <> "<html xml:lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\">"
      <> doc
      <> "</html>"

-- | If the client is mobile, it has picked error and success redirect urls (see
-- 'mkVerdictGrantedFormatMobile', 'mkVerdictDeniedFormatMobile'); variables in these URLs are here
-- substituted and the client is redirected accordingly.
verdictHandlerMobile :: HasCallStack => URI.URI -> URI.URI -> VerdictHandlerResult -> Spar SAML.ResponseVerdict
verdictHandlerMobile granted denied = \case
  VerifyHandlerGranted cky uid ->
    mkVerdictGrantedFormatMobile granted cky uid
      & either
        (throwSpar . SparCouldNotSubstituteSuccessURI . cs)
        (pure . successPage cky)
  VerifyHandlerDenied reasons ->
    mkVerdictDeniedFormatMobile denied "forbidden"
      & either
        (throwSpar . SparCouldNotSubstituteFailureURI . cs)
        (pure . forbiddenPage "forbidden" (explainDeniedReason <$> reasons))
  VerifyHandlerError lbl msg ->
    mkVerdictDeniedFormatMobile denied lbl
      & either
        (throwSpar . SparCouldNotSubstituteFailureURI . cs)
        (pure . forbiddenPage lbl [msg])
  where
    forbiddenPage :: ST -> [ST] -> URI.URI -> SAML.ResponseVerdict
    forbiddenPage errlbl errs uri =
      err303
        { errReasonPhrase = cs errlbl,
          errHeaders =
            [ ("Location", cs $ renderURI uri),
              ("Content-Type", "application/json")
            ],
          errBody = Aeson.encode errs
        }
    successPage :: SetCookie -> URI.URI -> SAML.ResponseVerdict
    successPage cky uri =
      err303
        { errReasonPhrase = "success",
          errHeaders =
            [ ("Location", cs $ renderURI uri),
              ("Set-Cookie", cs . Builder.toLazyByteString . renderSetCookie $ cky)
            ]
        }

-- | When getting stuck during login finalization, show a nice HTML error rather than the json
-- blob.  Show lots of debugging info for the customer to paste in any issue they might open.
errorPage :: SparError -> [Multipart.Input] -> Maybe Text -> ServerError
errorPage err inputs mcky =
  ServerError
    { errHTTPCode = Http.statusCode $ Wai.code werr,
      errReasonPhrase = cs $ Wai.label werr,
      errBody = easyHtml $ LT.intercalate "\n" errbody,
      errHeaders = [("Content-Type", "text/html")]
    }
  where
    werr = either forceWai id $ renderSparError err
    forceWai ServerError {..} = Wai.Error (Http.Status errHTTPCode "") (cs errReasonPhrase) (cs errBody)
    errbody :: [LT]
    errbody =
      [ "<head>",
        "  <title>wire:sso:error:" <> cs (Wai.label werr) <> "</title>",
        "</head>",
        "</body>",
        "  sorry, something went wrong :(<br>",
        "  please copy the following debug information to your clipboard and provide it when opening an issue in our customer support.<br><br>",
        "  <pre>" <> (cs . toText . encodeBase64 . cs . show $ (err, inputs, mcky)) <> "</pre>",
        "</body>"
      ]
