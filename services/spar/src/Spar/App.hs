{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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
  ( Env (..),
    throwSparSem,
    verdictHandler,
    GetUserResult (..),
    getUserIdByUref,
    getUserIdByScimExternalId,
    insertUser,
    validateEmailIfExists,
    validateEmail,
    errorPage,
    getIdPIdByIssuer,
    getIdPConfigByIssuer,
    getIdPConfigByIssuerAllowOld,
    deleteTeam,
    getIdPConfig,
    storeIdPConfig,
    getIdPConfigByIssuerOptionalSPId,
    sparToServerErrorWithLogging,
    renderSparErrorWithLogging,
  )
where

import Bilge
import Brig.Types (ManagedBy (..), User, userId, userTeam)
import Brig.Types.Intra (AccountStatus (..), accountStatus, accountUser)
import qualified Cassandra as Cas
import Control.Exception (assert)
import Control.Lens hiding ((.=))
import Control.Monad.Except
import Data.Aeson as Aeson (encode, object, (.=))
import Data.Aeson.Text as Aeson (encodeToLazyText)
import qualified Data.ByteString.Builder as Builder
import qualified Data.CaseInsensitive as CI
import Data.Id
import Data.String.Conversions
import Data.Text.Ascii (encodeBase64, toText)
import qualified Data.Text.Lazy as LT
import Imports hiding (MonadReader, asks, log)
import qualified Network.HTTP.Types.Status as Http
import qualified Network.Wai.Utilities.Error as Wai
import Polysemy
import Polysemy.Error
import SAML2.Util (renderURI)
import SAML2.WebSSO
  ( IdPId (..),
    Issuer (..),
    UnqualifiedNameID (..),
    explainDeniedReason,
    idpExtraInfo,
    idpId,
    uidTenant,
  )
import qualified SAML2.WebSSO as SAML
import Servant
import qualified Servant.Multipart as Multipart
import Spar.Error hiding (sparToServerErrorWithLogging)
import qualified Spar.Intra.BrigApp as Intra
import Spar.Orphans ()
import Spar.Sem.AReqIDStore (AReqIDStore)
import Spar.Sem.BindCookieStore (BindCookieStore)
import qualified Spar.Sem.BindCookieStore as BindCookieStore
import Spar.Sem.BrigAccess (BrigAccess)
import qualified Spar.Sem.BrigAccess as BrigAccess
import Spar.Sem.GalleyAccess (GalleyAccess)
import qualified Spar.Sem.GalleyAccess as GalleyAccess
import Spar.Sem.IdPConfigStore (GetIdPResult (..), IdPConfigStore)
import qualified Spar.Sem.IdPConfigStore as IdPConfigStore
import Spar.Sem.Reporter (Reporter)
import qualified Spar.Sem.Reporter as Reporter
import Spar.Sem.SAMLUserStore (SAMLUserStore)
import qualified Spar.Sem.SAMLUserStore as SAMLUserStore
import Spar.Sem.ScimExternalIdStore (ScimExternalIdStore)
import qualified Spar.Sem.ScimExternalIdStore as ScimExternalIdStore
import Spar.Sem.ScimTokenStore (ScimTokenStore)
import qualified Spar.Sem.ScimTokenStore as ScimTokenStore
import Spar.Sem.VerdictFormatStore (VerdictFormatStore)
import qualified Spar.Sem.VerdictFormatStore as VerdictFormatStore
import qualified System.Logger as TinyLog
import URI.ByteString as URI
import Web.Cookie (SetCookie, renderSetCookie)
import Wire.API.Cookie
import Wire.API.User.Identity (Email (..))
import Wire.API.User.IdentityProvider
import Wire.API.User.Saml
import Wire.API.User.Scim (ValidExternalId (..))
import Wire.Sem.Logger (Logger)
import qualified Wire.Sem.Logger as Logger
import Wire.Sem.Random (Random)
import qualified Wire.Sem.Random as Random

throwSparSem :: Member (Error SparError) r => SparCustomError -> Sem r a
throwSparSem = throw . SAML.CustomError

data Env = Env
  { sparCtxOpts :: Opts,
    sparCtxLogger :: TinyLog.Logger,
    sparCtxCas :: Cas.ClientState,
    sparCtxHttpManager :: Bilge.Manager,
    sparCtxHttpBrig :: Bilge.Request,
    sparCtxHttpGalley :: Bilge.Request,
    sparCtxRequestId :: RequestId
  }

getIdPConfig ::
  Members
    '[ IdPConfigStore,
       Error SparError
     ]
    r =>
  IdPId ->
  Sem r IdP
getIdPConfig = (>>= maybe (throwSparSem (SparIdPNotFound mempty)) pure) . IdPConfigStore.getConfig

storeIdPConfig :: Member IdPConfigStore r => IdP -> Sem r ()
storeIdPConfig idp = IdPConfigStore.storeConfig idp

getIdPConfigByIssuerOptionalSPId :: Members '[IdPConfigStore, Error SparError] r => Issuer -> Maybe TeamId -> Sem r IdP
getIdPConfigByIssuerOptionalSPId issuer mbteam = do
  getIdPConfigByIssuerAllowOld issuer mbteam >>= \case
    GetIdPFound idp -> pure idp
    GetIdPNotFound -> throwSparSem $ SparIdPNotFound mempty
    res@(GetIdPDanglingId _) -> throwSparSem $ SparIdPNotFound (cs $ show res)
    res@(GetIdPNonUnique _) -> throwSparSem $ SparIdPNotFound (cs $ show res)
    res@(GetIdPWrongTeam _) -> throwSparSem $ SparIdPNotFound (cs $ show res)

insertUser :: Member SAMLUserStore r => SAML.UserRef -> UserId -> Sem r ()
insertUser uref uid = SAMLUserStore.insert uref uid

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
getUserIdByUref :: Members '[BrigAccess, SAMLUserStore] r => Maybe TeamId -> SAML.UserRef -> Sem r (GetUserResult UserId)
getUserIdByUref mbteam uref = userId <$$> getUserByUref mbteam uref

getUserByUref :: Members '[BrigAccess, SAMLUserStore] r => Maybe TeamId -> SAML.UserRef -> Sem r (GetUserResult User)
getUserByUref mbteam uref = do
  muid <- SAMLUserStore.get uref
  case muid of
    Nothing -> pure GetUserNotFound
    Just uid -> do
      let withpending = Intra.WithPendingInvitations -- see haddocks above
      Intra.getBrigUser withpending uid >>= \case
        Nothing -> pure GetUserNotFound
        Just user
          | isNothing (userTeam user) -> pure GetUserNoTeam
          | isJust mbteam && mbteam /= userTeam user -> pure GetUserWrongTeam
          | otherwise -> pure $ GetUserFound user

data GetUserResult usr
  = GetUserFound usr
  | GetUserNotFound
  | GetUserNoTeam
  | GetUserWrongTeam
  deriving (Eq, Show)

instance Functor GetUserResult where
  fmap f (GetUserFound usr) = GetUserFound (f usr)
  fmap _ GetUserNotFound = GetUserNotFound
  fmap _ GetUserNoTeam = GetUserNoTeam
  fmap _ GetUserWrongTeam = GetUserWrongTeam

-- FUTUREWORK: Remove and reinstatate getUser, in AuthID refactoring PR
getUserIdByScimExternalId :: Members '[BrigAccess, ScimExternalIdStore] r => TeamId -> Email -> Sem r (Maybe UserId)
getUserIdByScimExternalId tid email = do
  muid <- (ScimExternalIdStore.lookup tid email)
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
createSamlUserWithId ::
  Members
    '[ Error SparError,
       BrigAccess,
       SAMLUserStore
     ]
    r =>
  TeamId ->
  UserId ->
  SAML.UserRef ->
  Sem r ()
createSamlUserWithId teamid buid suid = do
  uname <- either (throwSparSem . SparBadUserName . cs) pure $ Intra.mkUserName Nothing (UrefOnly suid)
  buid' <- BrigAccess.createSAML suid buid teamid uname ManagedByWire
  assert (buid == buid') $ pure ()
  insertUser suid buid

-- | If the team has no scim token, call 'createSamlUser'.  Otherwise, raise "invalid
-- credentials".
autoprovisionSamlUser ::
  Members
    '[ Random,
       GalleyAccess,
       BrigAccess,
       ScimTokenStore,
       IdPConfigStore,
       Error SparError,
       SAMLUserStore
     ]
    r =>
  Maybe TeamId ->
  SAML.UserRef ->
  Sem r UserId
autoprovisionSamlUser mbteam suid = do
  buid <- Id <$> Random.uuid
  autoprovisionSamlUserWithId mbteam buid suid
  pure buid

-- | Like 'autoprovisionSamlUser', but for an already existing 'UserId'.
autoprovisionSamlUserWithId ::
  forall r.
  Members
    '[ GalleyAccess,
       BrigAccess,
       ScimTokenStore,
       IdPConfigStore,
       Error SparError,
       SAMLUserStore
     ]
    r =>
  Maybe TeamId ->
  UserId ->
  SAML.UserRef ->
  Sem r ()
autoprovisionSamlUserWithId mbteam buid suid = do
  idp <- getIdPConfigByIssuerOptionalSPId (suid ^. uidTenant) mbteam
  guardReplacedIdP idp
  guardScimTokens idp
  createSamlUserWithId (idp ^. idpExtraInfo . wiTeam) buid suid
  validateEmailIfExists buid suid
  where
    -- Replaced IdPs are not allowed to create new wire accounts.
    guardReplacedIdP :: IdP -> Sem r ()
    guardReplacedIdP idp = do
      unless (isNothing $ idp ^. idpExtraInfo . wiReplacedBy) $ do
        throwSparSem $ SparCannotCreateUsersOnReplacedIdP (cs . SAML.idPIdToST $ idp ^. idpId)

    -- IdPs in teams with scim tokens are not allowed to auto-provision.
    guardScimTokens :: IdP -> Sem r ()
    guardScimTokens idp = do
      let teamid = idp ^. idpExtraInfo . wiTeam
      scimtoks <- ScimTokenStore.lookupByTeam teamid
      unless (null scimtoks) $ do
        throwSparSem SparSamlCredentialsNotFound

-- | If user's 'NameID' is an email address and the team has email validation for SSO enabled,
-- make brig initiate the email validate procedure.
validateEmailIfExists :: forall r. Members '[GalleyAccess, BrigAccess] r => UserId -> SAML.UserRef -> Sem r ()
validateEmailIfExists uid = \case
  (SAML.UserRef _ (view SAML.nameID -> UNameIDEmail email)) -> do
    mbTid <- Intra.getBrigUserTeam Intra.NoPendingInvitations uid
    validateEmail mbTid uid . Intra.emailFromSAML . CI.original $ email
  _ -> pure ()

validateEmail :: forall r. Members '[GalleyAccess, BrigAccess] r => Maybe TeamId -> UserId -> Email -> Sem r ()
validateEmail mbTid uid email = do
  enabled <- maybe (pure False) GalleyAccess.isEmailValidationEnabledTeam mbTid
  when enabled $ do
    BrigAccess.updateEmail uid email

-- | Check if 'UserId' is in the team that hosts the idp that owns the 'UserRef'.  If so,
-- register a the user under its SAML credentials and write the 'UserRef' into the
-- 'UserIdentity'.  Otherwise, throw an error.
--
-- Before returning, change account status or fail if account is nto active or pending an
-- invitation.
bindUser ::
  forall r.
  Members
    '[ BrigAccess,
       IdPConfigStore,
       Error SparError,
       SAMLUserStore
     ]
    r =>
  UserId ->
  SAML.UserRef ->
  Sem r UserId
bindUser buid userref = do
  oldStatus <- do
    let err :: Sem r a
        err = throwSparSem . SparBindFromWrongOrNoTeam . cs . show $ buid
    teamid :: TeamId <-
      getIdPConfigByIssuerAllowOld (userref ^. uidTenant) Nothing >>= \case
        GetIdPFound idp -> pure $ idp ^. idpExtraInfo . wiTeam
        GetIdPNotFound -> err
        GetIdPDanglingId _ -> err -- database inconsistency
        GetIdPNonUnique is -> throwSparSem $ SparUserRefInNoOrMultipleTeams (cs $ show (buid, is))
        GetIdPWrongTeam _ -> err -- impossible
    acc <- BrigAccess.getAccount Intra.WithPendingInvitations buid >>= maybe err pure
    teamid' :: TeamId <- userTeam (accountUser acc) & maybe err pure
    unless (teamid' == teamid) err
    pure (accountStatus acc)
  insertUser userref buid
  buid <$ do
    BrigAccess.setVeid buid (UrefOnly userref)
    let err = throwSparSem . SparBindFromBadAccountStatus . cs . show
    case oldStatus of
      Active -> pure ()
      Suspended -> err oldStatus
      Deleted -> err oldStatus
      Ephemeral -> err oldStatus
      PendingInvitation -> BrigAccess.setStatus buid Active

-- | The from of the response on the finalize-login request depends on the verdict (denied or
-- granted), plus the choice that the client has made during the initiate-login request.  Here we
-- call either 'verdictHandlerWeb' or 'verdictHandlerMobile', resp., on the 'SAML.AccessVerdict'.
--
-- NB: there are at least two places in the 'SAML.AuthnResponse' that can contain the request id:
-- the response header and every assertion.  Since saml2-web-sso validation guarantees that the
-- signed in-response-to info in the assertions matches the unsigned in-response-to field in the
-- 'SAML.Response', and fills in the response id in the header if missing, we can just go for the
-- latter.
verdictHandler ::
  HasCallStack =>
  Members
    '[ Random,
       Logger String,
       GalleyAccess,
       BrigAccess,
       BindCookieStore,
       AReqIDStore,
       VerdictFormatStore,
       ScimTokenStore,
       IdPConfigStore,
       Error SparError,
       Reporter,
       SAMLUserStore
     ]
    r =>
  Maybe BindCookie ->
  Maybe TeamId ->
  SAML.AuthnResponse ->
  SAML.AccessVerdict ->
  Sem r SAML.ResponseVerdict
verdictHandler cky mbteam aresp verdict = do
  -- [3/4.1.4.2]
  -- <SubjectConfirmation> [...] If the containing message is in response to an <AuthnRequest>, then
  -- the InResponseTo attribute MUST match the request's ID.
  Logger.log Logger.Debug $ "entering verdictHandler: " <> show (fromBindCookie <$> cky, aresp, verdict)
  reqid <- either (throwSparSem . SparNoRequestRefInResponse . cs) pure $ SAML.rspInResponseTo aresp
  format :: Maybe VerdictFormat <- VerdictFormatStore.get reqid
  resp <- case format of
    Just (VerdictFormatWeb) ->
      verdictHandlerResult cky mbteam verdict >>= verdictHandlerWeb
    Just (VerdictFormatMobile granted denied) ->
      verdictHandlerResult cky mbteam verdict >>= verdictHandlerMobile granted denied
    Nothing ->
      -- (this shouldn't happen too often, see 'storeVerdictFormat')
      throwSparSem SparNoSuchRequest
  Logger.log Logger.Debug $ "leaving verdictHandler: " <> show resp
  pure resp

data VerdictHandlerResult
  = VerifyHandlerGranted {_vhrCookie :: SetCookie, _vhrUserId :: UserId}
  | VerifyHandlerDenied {_vhrReasons :: [SAML.DeniedReason]}
  | VerifyHandlerError {_vhrLabel :: ST, _vhrMessage :: ST}
  deriving (Eq, Show)

verdictHandlerResult ::
  HasCallStack =>
  Members
    '[ Random,
       Logger String,
       GalleyAccess,
       BrigAccess,
       BindCookieStore,
       ScimTokenStore,
       IdPConfigStore,
       Error SparError,
       Reporter,
       SAMLUserStore
     ]
    r =>
  Maybe BindCookie ->
  Maybe TeamId ->
  SAML.AccessVerdict ->
  Sem r VerdictHandlerResult
verdictHandlerResult bindCky mbteam verdict = do
  Logger.log Logger.Debug $ "entering verdictHandlerResult: " <> show (fromBindCookie <$> bindCky)
  result <- catchVerdictErrors $ verdictHandlerResultCore bindCky mbteam verdict
  Logger.log Logger.Debug $ "leaving verdictHandlerResult" <> show result
  pure result

catchVerdictErrors ::
  forall r.
  Members
    '[ Reporter,
       Error SparError
     ]
    r =>
  Sem r VerdictHandlerResult ->
  Sem r VerdictHandlerResult
catchVerdictErrors = (`catch` hndlr)
  where
    hndlr :: SparError -> Sem r VerdictHandlerResult
    hndlr err = do
      waiErr <- renderSparErrorWithLogging err
      pure $ case waiErr of
        Right (werr :: Wai.Error) -> VerifyHandlerError (cs $ Wai.label werr) (cs $ Wai.message werr)
        Left (serr :: ServerError) -> VerifyHandlerError "unknown-error" (cs (errReasonPhrase serr) <> " " <> cs (errBody serr))

-- | If a user attempts to login presenting a new IdP issuer, but there is no entry in
-- @"spar.user"@ for her: lookup @"old_issuers"@ from @"spar.idp"@ for the new IdP, and
-- traverse the old IdPs in search for the old entry.  Return that old entry.
findUserIdWithOldIssuer ::
  forall r.
  Members
    '[ BrigAccess,
       IdPConfigStore,
       SAMLUserStore,
       Error SparError
     ]
    r =>
  Maybe TeamId ->
  SAML.UserRef ->
  Sem r (GetUserResult (SAML.UserRef, UserId))
findUserIdWithOldIssuer mbteam (SAML.UserRef issuer subject) = do
  idp <- getIdPConfigByIssuerOptionalSPId issuer mbteam
  let tryFind :: GetUserResult (SAML.UserRef, UserId) -> Issuer -> Sem r (GetUserResult (SAML.UserRef, UserId))
      tryFind found@(GetUserFound _) _ = pure found
      tryFind _ oldIssuer = (uref,) <$$> getUserIdByUref mbteam uref
        where
          uref = SAML.UserRef oldIssuer subject
  foldM tryFind GetUserNotFound (idp ^. idpExtraInfo . wiOldIssuers)

-- | After a user has been found using 'findUserWithOldIssuer', update it everywhere so that
-- the old IdP is not needed any more next time.
moveUserToNewIssuer :: Members '[BrigAccess, SAMLUserStore] r => SAML.UserRef -> SAML.UserRef -> UserId -> Sem r ()
moveUserToNewIssuer oldUserRef newUserRef uid = do
  SAMLUserStore.insert newUserRef uid
  BrigAccess.setVeid uid (UrefOnly newUserRef)
  SAMLUserStore.delete uid oldUserRef

verdictHandlerResultCore ::
  HasCallStack =>
  Members
    '[ Random,
       Logger String,
       GalleyAccess,
       BrigAccess,
       BindCookieStore,
       ScimTokenStore,
       IdPConfigStore,
       Error SparError,
       SAMLUserStore
     ]
    r =>
  Maybe BindCookie ->
  Maybe TeamId ->
  SAML.AccessVerdict ->
  Sem r VerdictHandlerResult
verdictHandlerResultCore bindCky mbteam = \case
  SAML.AccessDenied reasons -> do
    pure $ VerifyHandlerDenied reasons
  SAML.AccessGranted userref -> do
    uid :: UserId <- do
      viaBindCookie <- maybe (pure Nothing) (BindCookieStore.lookup) bindCky
      viaSparCassandra <- getUserIdByUref mbteam userref
      -- race conditions: if the user has been created on spar, but not on brig, 'getUser'
      -- returns 'Nothing'.  this is ok assuming 'createUser', 'bindUser' (called below) are
      -- idempotent.
      viaSparCassandraOldIssuer <-
        case viaSparCassandra of
          GetUserFound _ -> pure GetUserNotFound
          _ -> findUserIdWithOldIssuer mbteam userref
      let err =
            SparUserRefInNoOrMultipleTeams . cs $
              show (userref, viaBindCookie, viaSparCassandra, viaSparCassandraOldIssuer)
      case (viaBindCookie, viaSparCassandra, viaSparCassandraOldIssuer) of
        (_, GetUserNoTeam, _) -> throwSparSem err
        (_, GetUserWrongTeam, _) -> throwSparSem err
        (_, _, GetUserNoTeam) -> throwSparSem err
        (_, _, GetUserWrongTeam) -> throwSparSem err
        -- This is the first SSO authentication, so we auto-create a user. We know the user
        -- has not been created via SCIM because then we would've ended up in the
        -- "reauthentication" branch.
        (Nothing, GetUserNotFound, GetUserNotFound) -> autoprovisionSamlUser mbteam userref
        -- If the user is only found under an old (previous) issuer, move it here.
        (Nothing, GetUserNotFound, GetUserFound (oldUserRef, uid)) -> moveUserToNewIssuer oldUserRef userref uid >> pure uid
        -- SSO re-authentication (the most common case).
        (Nothing, GetUserFound uid, _) -> pure uid
        -- Bind existing user (non-SSO or SSO) to ssoid
        (Just uid, GetUserNotFound, GetUserNotFound) -> bindUser uid userref
        (Just uid, GetUserFound uid', GetUserNotFound)
          -- Redundant binding (no change to Brig or Spar)
          | uid == uid' -> pure uid
          -- Attempt to use ssoid for a second Wire user
          | otherwise -> throwSparSem SparBindUserRefTaken
        -- same two cases as above, but between last login and bind there was an issuer update.
        (Just uid, GetUserNotFound, GetUserFound (oldUserRef, uid'))
          | uid == uid' -> moveUserToNewIssuer oldUserRef userref uid >> pure uid
          | otherwise -> throwSparSem SparBindUserRefTaken
        (Just _, GetUserFound _, GetUserFound _) ->
          -- to see why, consider the condition on the call to 'findUserWithOldIssuer' above.
          error "impossible."
    Logger.log Logger.Debug ("granting sso login for " <> show uid)
    cky <- BrigAccess.ssoLogin uid
    pure $ VerifyHandlerGranted cky uid

-- | If the client is web, it will be served with an HTML page that it can process to decide whether
-- to log the user in or show an error.
--
-- The HTML page is empty and has two ways to communicate the verdict to the js app:
-- - A title element with contents @wire:sso:<outcome>@.  This is chosen to be easily parseable and
--   not be the title of any page sent by the IdP while it negotiates with the user.
-- - The page broadcasts a message to '*', to be picked up by the app.
verdictHandlerWeb :: HasCallStack => VerdictHandlerResult -> Sem r SAML.ResponseVerdict
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
verdictHandlerMobile :: (HasCallStack, Member (Error SparError) r) => URI.URI -> URI.URI -> VerdictHandlerResult -> Sem r SAML.ResponseVerdict
verdictHandlerMobile granted denied = \case
  VerifyHandlerGranted cky uid ->
    mkVerdictGrantedFormatMobile granted cky uid
      & either
        (throwSparSem . SparCouldNotSubstituteSuccessURI . cs)
        (pure . successPage cky)
  VerifyHandlerDenied reasons ->
    mkVerdictDeniedFormatMobile denied "forbidden"
      & either
        (throwSparSem . SparCouldNotSubstituteFailureURI . cs)
        (pure . forbiddenPage "forbidden" (explainDeniedReason <$> reasons))
  VerifyHandlerError lbl msg ->
    mkVerdictDeniedFormatMobile denied lbl
      & either
        (throwSparSem . SparCouldNotSubstituteFailureURI . cs)
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
errorPage err mpInputs mcky =
  ServerError
    { errHTTPCode = Http.statusCode $ Wai.code werr,
      errReasonPhrase = cs $ Wai.label werr,
      errBody = easyHtml $ LT.intercalate "\n" errbody,
      errHeaders = [("Content-Type", "text/html")]
    }
  where
    werr = either forceWai id $ renderSparError err
    forceWai ServerError {..} = Wai.mkError (Http.Status errHTTPCode "") (cs errReasonPhrase) (cs errBody)
    errbody :: [LT]
    errbody =
      [ "<head>",
        "  <title>wire:sso:error:" <> cs (Wai.label werr) <> "</title>",
        "</head>",
        "</body>",
        "  sorry, something went wrong :(<br>",
        "  please copy the following debug information to your clipboard and provide it when opening an issue in our customer support.<br><br>",
        "  <pre>" <> (cs . toText . encodeBase64 . cs . show $ (err, mpInputs, mcky)) <> "</pre>",
        "</body>"
      ]

-- | Like 'getIdPIdByIssuer', but do not require a 'TeamId'.  If none is provided, see if a
-- single solution can be found without.
getIdPIdByIssuerAllowOld ::
  (HasCallStack) =>
  Member IdPConfigStore r =>
  SAML.Issuer ->
  Maybe TeamId ->
  Sem r (GetIdPResult SAML.IdPId)
getIdPIdByIssuerAllowOld issuer mbteam = do
  mbv2 <- maybe (pure Nothing) (IdPConfigStore.getIdByIssuerWithTeam issuer) mbteam
  mbv1v2 <- maybe (IdPConfigStore.getIdByIssuerWithoutTeam issuer) (pure . GetIdPFound) mbv2
  case (mbv1v2, mbteam) of
    (GetIdPFound idpid, Just team) -> do
      IdPConfigStore.getConfig idpid >>= \case
        Nothing -> do
          pure $ GetIdPDanglingId idpid
        Just idp ->
          pure $
            if idp ^. SAML.idpExtraInfo . wiTeam /= team
              then GetIdPWrongTeam idpid
              else mbv1v2
    _ -> pure mbv1v2

-- | See 'getIdPIdByIssuer'.
getIdPConfigByIssuer ::
  (HasCallStack, Member IdPConfigStore r) =>
  SAML.Issuer ->
  TeamId ->
  Sem r (GetIdPResult IdP)
getIdPConfigByIssuer issuer =
  getIdPIdByIssuer issuer >=> mapGetIdPResult

-- | See 'getIdPIdByIssuerAllowOld'.
getIdPConfigByIssuerAllowOld ::
  (HasCallStack, Member IdPConfigStore r) =>
  SAML.Issuer ->
  Maybe TeamId ->
  Sem r (GetIdPResult IdP)
getIdPConfigByIssuerAllowOld issuer = do
  getIdPIdByIssuerAllowOld issuer >=> mapGetIdPResult

-- | Lookup idp in table `issuer_idp_v2` (using both issuer entityID and teamid); if nothing
-- is found there or if teamid is 'Nothing', lookup under issuer in `issuer_idp`.
getIdPIdByIssuer ::
  (HasCallStack, Member IdPConfigStore r) =>
  SAML.Issuer ->
  TeamId ->
  Sem r (GetIdPResult SAML.IdPId)
getIdPIdByIssuer issuer = getIdPIdByIssuerAllowOld issuer . Just

-- | (There are probably category theoretical models for what we're doing here, but it's more
-- straight-forward to just handle the one instance we need.)
mapGetIdPResult :: (HasCallStack, Member IdPConfigStore r) => GetIdPResult SAML.IdPId -> Sem r (GetIdPResult IdP)
mapGetIdPResult (GetIdPFound i) = IdPConfigStore.getConfig i <&> maybe (GetIdPDanglingId i) GetIdPFound
mapGetIdPResult GetIdPNotFound = pure GetIdPNotFound
mapGetIdPResult (GetIdPDanglingId i) = pure (GetIdPDanglingId i)
mapGetIdPResult (GetIdPNonUnique is) = pure (GetIdPNonUnique is)
mapGetIdPResult (GetIdPWrongTeam i) = pure (GetIdPWrongTeam i)

-- | Delete all tokens belonging to a team.
deleteTeam ::
  (HasCallStack, Members '[ScimTokenStore, SAMLUserStore, IdPConfigStore] r) =>
  TeamId ->
  Sem r ()
deleteTeam team = do
  ScimTokenStore.deleteByTeam team
  -- Since IdPs are not shared between teams, we can look at the set of IdPs
  -- used by the team, and remove everything related to those IdPs, too.
  idps <- IdPConfigStore.getConfigsByTeam team
  for_ idps $ \idp -> do
    let issuer = idp ^. SAML.idpMetadata . SAML.edIssuer
    SAMLUserStore.deleteByIssuer issuer
    IdPConfigStore.deleteConfig idp

sparToServerErrorWithLogging :: Member Reporter r => SparError -> Sem r ServerError
sparToServerErrorWithLogging err = do
  let errServant = sparToServerError err
  Reporter.report Nothing (servantToWaiError errServant)
  pure errServant

renderSparErrorWithLogging :: Member Reporter r => SparError -> Sem r (Either ServerError Wai.Error)
renderSparErrorWithLogging err = do
  let errPossiblyWai = renderSparError err
  Reporter.report Nothing (either servantToWaiError id $ errPossiblyWai)
  pure errPossiblyWai
