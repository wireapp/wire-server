-- Disabling to stop warnings on HasCallStack
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
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
    getUserByUrefUnsafe,
    getUserByUrefViaOldIssuerUnsafe,
    getUserIdByScimExternalId,
    validateEmail,
    errorPage,
    deleteTeam,
    sparToServerErrorWithLogging,
    renderSparErrorWithLogging,
  )
where

import Bilge
import qualified Cassandra as Cas
import Control.Exception (assert)
import Control.Lens hiding ((.=))
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Aeson as Aeson (encode, object, (.=))
import Data.Aeson.Text as Aeson (encodeToLazyText)
import Data.ByteString (toStrict)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.CaseInsensitive as CI
import Data.Id
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import Data.Text.Ascii (encodeBase64, toText)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LText
import Data.These
import Imports hiding (MonadReader, asks, log)
import qualified Network.HTTP.Types.Status as Http
import qualified Network.Wai.Utilities.Error as Wai
import Polysemy
import Polysemy.Error
import SAML2.Util (renderURI)
import SAML2.WebSSO
  ( Issuer (..),
    UnqualifiedNameID (..),
    explainDeniedReason,
    idpExtraInfo,
    idpId,
  )
import qualified SAML2.WebSSO as SAML
import Servant
import qualified Servant.Multipart as Multipart
import Spar.Error hiding (sparToServerErrorWithLogging)
import qualified Spar.Intra.RpcApp as Intra
import Spar.Options
import Spar.Orphans ()
import Spar.Sem.AReqIDStore (AReqIDStore)
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
import System.Logger (Msg)
import qualified System.Logger as Log
import qualified System.Logger as TinyLog
import URI.ByteString as URI
import Web.Cookie (SetCookie, renderSetCookie)
import Wire.API.Team.Role (Role, defaultRole)
import Wire.API.User
import Wire.API.User.Auth
import Wire.API.User.IdentityProvider
import Wire.API.User.Saml
import Wire.BrigAPIAccess (BrigAPIAccess)
import qualified Wire.BrigAPIAccess as BrigAPIAccess
import Wire.Error
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import qualified Wire.GalleyAPIAccess as GalleyAPIAccess
import Wire.IdPConfigStore (IdPConfigStore)
import qualified Wire.IdPConfigStore as IdPConfigStore
import Wire.ScimSubsystem.Interpreter
import Wire.Sem.Logger (Logger)
import qualified Wire.Sem.Logger as Logger
import Wire.Sem.Random (Random)
import qualified Wire.Sem.Random as Random

throwSparSem :: (Member (Error SparError) r) => SparCustomError -> Sem r a
throwSparSem = throw . SAML.CustomError

data Env = Env
  { sparCtxOpts :: Opts,
    sparCtxLogger :: TinyLog.Logger,
    sparCtxCas :: Cas.ClientState,
    sparCtxHttpManager :: Bilge.Manager,
    sparCtxHttpBrig :: Bilge.Request,
    sparCtxHttpGalley :: Bilge.Request,
    sparCtxRequestId :: RequestId,
    sparCtxScimSubsystemConfig :: ScimSubsystemConfig
  }

-- | Get a user by UserRef, no matter what the team.
--
-- Look up user locally in table @spar.user@ or @spar.scim_user@ (depending on the
-- argument), then in brig, then return the 'User'.  If either lookup fails, or user is not
-- in a team, return 'Nothing'.
--
-- If a user has been created via scim invite (ie., no IdP present), and has status
-- 'PendingInvitation', it will be returned here, since for SCIM purposes it is an
-- existing (if inactive) user.  If 'getUser' is called during SAML authentication, this may
-- cause an inactive user to log in, but that's ok: `PendingActivation` means that email and
-- password handshake have not been completed; it's still ok for the user to gain access to
-- the team with valid SAML credentials.
--
-- FUTUREWORK: Remove and reinstate getUser, in AuthID refactoring PR.  (in
-- https://github.com/wireapp/wire-server/pull/1410, undo
-- https://github.com/wireapp/wire-server/pull/1418)
--
-- FUTUREWORK: https://wearezeta.atlassian.net/browse/SQSERVICES-1655
getUserByUrefUnsafe ::
  ( Member BrigAPIAccess r,
    Member SAMLUserStore r
  ) =>
  SAML.UserRef ->
  Sem r (Maybe User)
getUserByUrefUnsafe uref = do
  maybe (pure Nothing) (BrigAPIAccess.getAccount Intra.WithPendingInvitations) =<< SAMLUserStore.get uref

-- FUTUREWORK: Remove and reinstatate getUser, in AuthID refactoring PR
getUserIdByScimExternalId ::
  ( Member BrigAPIAccess r,
    Member ScimExternalIdStore r
  ) =>
  TeamId ->
  Text ->
  Sem r (Maybe UserId)
getUserIdByScimExternalId tid eid = do
  muid <- ScimExternalIdStore.lookup tid eid
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
  ( Member (Error SparError) r,
    Member BrigAPIAccess r,
    Member SAMLUserStore r
  ) =>
  TeamId ->
  UserId ->
  SAML.UserRef ->
  Role ->
  Sem r ()
createSamlUserWithId teamid buid suid role = do
  uname <-
    either (throwSparSem . SparBadUserName . LText.pack) pure $
      Intra.mkUserName Nothing (That suid)
  buid' <- BrigAPIAccess.createSAML suid buid teamid uname ManagedByWire Nothing Nothing Nothing role
  assert (buid == buid') $ pure ()
  SAMLUserStore.insert suid buid

-- | If the team has no scim token, call 'createSamlUser'.  Otherwise, raise "invalid
-- credentials".  (FUTUREWORK: Assumes that `UserRef` is still available globally.  See
-- https://wearezeta.atlassian.net/browse/SQSERVICES-1655)
autoprovisionSamlUser ::
  forall r.
  ( Member GalleyAPIAccess r,
    Member BrigAPIAccess r,
    Member ScimTokenStore r,
    Member IdPConfigStore r,
    Member (Error SparError) r,
    Member SAMLUserStore r
  ) =>
  IdP ->
  UserId ->
  SAML.UserRef ->
  Sem r ()
autoprovisionSamlUser idp buid suid = do
  guardReplacedIdP
  guardScimTokens
  createSamlUserWithId (idp ^. idpExtraInfo . team) buid suid defaultRole
  where
    -- Replaced IdPs are not allowed to create new wire accounts.
    guardReplacedIdP :: Sem r ()
    guardReplacedIdP = do
      unless (isNothing $ idp ^. idpExtraInfo . replacedBy) $ do
        throwSparSem $ SparCannotCreateUsersOnReplacedIdP (LText.fromStrict . SAML.idPIdToST $ idp ^. idpId)

    -- IdPs in teams with scim tokens are not allowed to auto-provision.
    guardScimTokens :: Sem r ()
    guardScimTokens = do
      let teamid = idp ^. idpExtraInfo . team
      scimtoks <- ScimTokenStore.lookupByTeam teamid
      unless (null scimtoks) $ do
        throwSparSem SparSamlCredentialsNotFound

-- | If user's 'NameID' is an email address and the team has email validation for SSO enabled,
-- make brig initiate the email validate procedure.
validateSamlEmailIfExists ::
  forall r.
  ( Member GalleyAPIAccess r,
    Member BrigAPIAccess r
  ) =>
  UserId ->
  SAML.UserRef ->
  Sem r ()
validateSamlEmailIfExists uid = \case
  (SAML.UserRef _ (view SAML.nameID -> UNameIDEmail email)) -> do
    mbTid <- Intra.getBrigUserTeam Intra.NoPendingInvitations uid
    validateEmail mbTid uid . Intra.emailFromSAML . CI.original $ email
  _ -> pure ()

validateEmail ::
  forall r.
  ( Member GalleyAPIAccess r,
    Member BrigAPIAccess r
  ) =>
  Maybe TeamId ->
  UserId ->
  EmailAddress ->
  Sem r ()
validateEmail (Just tid) uid email = do
  enabled <- GalleyAPIAccess.isEmailValidationEnabledTeam tid
  let activation = if enabled then SendActivationEmail else AutoActivate
  BrigAPIAccess.updateEmail uid email activation
validateEmail _ _ _ = pure ()

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
  (HasCallStack) =>
  ( Member Random r,
    Member (Logger (Msg -> Msg)) r,
    Member GalleyAPIAccess r,
    Member BrigAPIAccess r,
    Member AReqIDStore r,
    Member VerdictFormatStore r,
    Member ScimTokenStore r,
    Member ScimExternalIdStore r,
    Member IdPConfigStore r,
    Member (Error SparError) r,
    Member Reporter r,
    Member SAMLUserStore r
  ) =>
  NonEmpty SAML.Assertion ->
  SAML.AccessVerdict ->
  IdP ->
  SAML.Config ->
  Maybe Text ->
  Sem r SAML.ResponseVerdict
verdictHandler aresp verdict idp samlConfig mbHost = do
  -- [3/4.1.4.2]
  -- <SubjectConfirmation> [...] If the containing message is in response to an <AuthnRequest>, then
  -- the InResponseTo attribute MUST match the request's ID.
  Logger.debug $ Log.msg ("entering verdictHandler" :: String) . Log.field "aresp" (show aresp) . Log.field "verdict" (show verdict)
  reqid <- do
    let xs = SAML.assertionToInResponseTo `mapM` aresp
    case NonEmpty.nub <$> xs of
      Right (x :| []) -> pure x
      Left err -> throwSparSem (SparNoRequestRefInResponse $ "missing or incoherent requestIDs: " <> LText.pack err)
      _ -> throwSparSem SparNoSuchRequest
  format :: Maybe VerdictFormat <- VerdictFormatStore.get reqid
  resp <- case format of
    Just (VerdictFormatWeb mlabel) ->
      verdictHandlerResult verdict idp mlabel samlConfig mbHost >>= verdictHandlerWeb
    Just (VerdictFormatMobile granted denied mlabel) ->
      verdictHandlerResult verdict idp mlabel samlConfig mbHost >>= verdictHandlerMobile granted denied
    Nothing ->
      -- (this shouldn't happen too often, see 'storeVerdictFormat')
      throwSparSem SparNoSuchRequest
  Logger.debug $ Log.msg ("leaving verdictHandler" :: String) . Log.field "resp" (show resp)
  pure resp

data VerdictHandlerResult
  = VerifyHandlerGranted {_vhrCookie :: SetCookie, _vhrUserId :: UserId}
  | VerifyHandlerDenied {_vhrReasons :: [SAML.DeniedReason]}
  | VerifyHandlerError {_vhrLabel :: Text, _vhrMessage :: Text}
  deriving (Eq, Show)

verdictHandlerResult ::
  (HasCallStack) =>
  ( Member Random r,
    Member (Logger (Msg -> Msg)) r,
    Member GalleyAPIAccess r,
    Member BrigAPIAccess r,
    Member ScimTokenStore r,
    Member ScimExternalIdStore r,
    Member IdPConfigStore r,
    Member (Error SparError) r,
    Member Reporter r,
    Member SAMLUserStore r
  ) =>
  SAML.AccessVerdict ->
  IdP ->
  Maybe CookieLabel ->
  SAML.Config ->
  Maybe Text ->
  Sem r VerdictHandlerResult
verdictHandlerResult verdict idp mlabel samlConfig mbHost = do
  Logger.debug $ Log.msg ("entering verdictHandlerResult" :: String)
  result <- catchVerdictErrors $ verdictHandlerResultCore idp verdict mlabel samlConfig mbHost
  Logger.debug $ Log.msg ("leaving verdictHandlerResult" :: String) . Log.field "result" (show result)
  pure result

catchVerdictErrors ::
  forall r.
  ( Member Reporter r,
    Member (Error SparError) r
  ) =>
  Sem r VerdictHandlerResult ->
  Sem r VerdictHandlerResult
catchVerdictErrors = (`catch` hndlr)
  where
    hndlr :: SparError -> Sem r VerdictHandlerResult
    hndlr err = do
      werr <- renderSparErrorWithLogging err <&> httpErrorToWaiError
      -- TODO: we don't want to include the RichError part of
      -- HttpError in the response, but maybe we should log it?
      pure $
        VerifyHandlerError
          (LText.toStrict $ Wai.label werr)
          (LText.toStrict $ Wai.message werr)

-- | If a user attempts to login presenting a new IdP issuer, but there is no entry in
-- @"spar.user"@ for her: lookup @"old_issuers"@ from @"spar.idp"@ for the new IdP, and
-- traverse the old issuers in search for the old entry.
--
-- FUTUREWORK: https://wearezeta.atlassian.net/browse/SQSERVICES-1655
getUserByUrefViaOldIssuerUnsafe ::
  forall r.
  ( Member BrigAPIAccess r,
    Member SAMLUserStore r
  ) =>
  IdP ->
  SAML.UserRef ->
  Sem r (Maybe (SAML.UserRef, User))
getUserByUrefViaOldIssuerUnsafe idp (SAML.UserRef _ subject) = do
  let tryFind :: Maybe (SAML.UserRef, User) -> Issuer -> Sem r (Maybe (SAML.UserRef, User))
      tryFind found@(Just _) _ = pure found
      tryFind Nothing oldIssuer = (uref,) <$$> getUserByUrefUnsafe uref
        where
          uref = SAML.UserRef oldIssuer subject

  foldM tryFind Nothing (idp ^. idpExtraInfo . oldIssuers)

-- | After a user has been found using 'findUserWithOldIssuer', update it everywhere so that
-- the old IdP is not needed any more next time.
moveUserToNewIssuer ::
  ( Member BrigAPIAccess r,
    Member SAMLUserStore r
  ) =>
  SAML.UserRef ->
  SAML.UserRef ->
  UserId ->
  Sem r ()
moveUserToNewIssuer oldUserRef newUserRef uid = do
  SAMLUserStore.insert newUserRef uid
  BrigAPIAccess.setSSOId uid (UserSSOId newUserRef)
  SAMLUserStore.delete uid oldUserRef

-- TODO: Ideally, we would leave this function untouched to make obvious that the behaviour hasn't changed.
-- As it has side-effects, this ideal can probably not be reached. However, we could consider to let it return a result and act accordingly.
verdictHandlerResultCore ::
  forall r.
  (HasCallStack) =>
  ( Member Random r,
    Member (Logger (Msg -> Msg)) r,
    Member GalleyAPIAccess r,
    Member BrigAPIAccess r,
    Member ScimTokenStore r,
    Member ScimExternalIdStore r,
    Member IdPConfigStore r,
    Member (Error SparError) r,
    Member SAMLUserStore r
  ) =>
  IdP ->
  SAML.AccessVerdict ->
  Maybe CookieLabel ->
  SAML.Config ->
  Maybe Text ->
  Sem r VerdictHandlerResult
verdictHandlerResultCore idp verdict mlabel samlConfig mbHost = case verdict of
  SAML.AccessDenied reasons -> do
    pure $ VerifyHandlerDenied reasons
  SAML.AccessGranted uref -> do
    uid :: UserId <- do
      let team' = idp ^. idpExtraInfo . team
      findUserWithUref idp team' uref >>= \case
        Just uid -> pure uid
        Nothing
          | SAML.isMultiIngressConfig samlConfig ->
              multiIngressFlow team'
        Nothing -> provisionNewUser
    Logger.debug $ Log.msg ("granting sso login" :: String) . Log.field "user" (idToText uid)
    cky <- BrigAPIAccess.ssoLogin uid mlabel
    pure $ VerifyHandlerGranted cky uid
    where
      provisionNewUser :: Sem r UserId
      provisionNewUser = do
        buid <- Id <$> Random.uuid
        autoprovisionSamlUser idp buid uref
        validateSamlEmailIfExists buid uref
        pure buid

      -- Try to find a user by UserRef, with fallback to old issuers.
      -- Returns the UserId if found and in the correct team, Nothing if not found.
      -- Throws SparUserRefInNoOrMultipleTeams if user is found but in the wrong team.
      -- Side effect: Old-style users (found via old issuers) are migrated to the new issuer.
      findUserWithUref :: IdP -> TeamId -> SAML.UserRef -> Sem r (Maybe UserId)
      findUserWithUref idp' team'' uref' = do
        let err = SparUserRefInNoOrMultipleTeams . LText.pack . show $ uref'
        getUserByUrefUnsafe uref' >>= \case
          Just usr -> do
            if userTeam usr == Just team''
              then pure (Just (userId usr))
              else throwSparSem err
          Nothing -> do
            getUserByUrefViaOldIssuerUnsafe idp' uref' >>= \case
              Just (olduref, usr) -> do
                let uid = userId usr
                if userTeam usr == Just team''
                  then moveUserToNewIssuer olduref uref' uid >> pure (Just uid)
                  else throwSparSem err
              Nothing -> pure Nothing

      -- In multi-ingress scenarios users can be already assigned to one IdP,
      -- but try to authenticate with another. We allow this, when the new IdP
      -- is configured for the user's team and the used domain. Additionally,
      -- the provided NameId must be an email address (no username) to prevent
      -- ambiguities (though, we know this won't be guarding against all
      -- ambiguity cases).
      -- When we've found the matching IdP and the user's old one, we migrate
      -- the user to the new one to not have to run this search again when the
      -- user logs in with this IdP.
      multiIngressFlow :: TeamId -> Sem r UserId
      multiIngressFlow team' =
        case uref of
          -- Cross-IdP SSO migration only for email-based NameIDs in
          -- multi-ingress mode. We may consider to lower the email-only
          -- constraint in future. For now, Emil and Sven decided that emails
          -- might be a bit more consistent across IdPs then usernames.
          SAML.UserRef _ (view SAML.nameID -> UNameIDEmail _) -> do
            teamIdPs <- IdPConfigStore.getConfigsByTeam team'
            let urefIssuer = uref ^. SAML.uidTenant

            -- Select the authenticating IdP from the team's IdPs
            selectAuthenticatingIdP teamIdPs urefIssuer mbHost >>= \case
              Nothing -> do
                -- No matching IdP found and it's not a singleton case
                let issuerText = urefIssuer ^. SAML.fromIssuer . to URI.serializeURIRef'
                    domainText = fromMaybe "default" mbHost
                    errorMsg =
                      LText.pack $
                        "IdP with issuer '"
                          <> show issuerText
                          <> "' for domain '"
                          <> Text.unpack domainText
                          <> "' is not configured for this team"
                throwSparSem $ SparIdPNotFound errorMsg
              Just multiIngressIdp -> do
                -- Try to authenticate the potential user against ALL team IdPs
                -- (including other domains) When we found one succeeding IdP
                -- for this user in this team, we consider them authenticated
                -- and migrate them to the other (requesting) IdP.
                let subject = uref ^. SAML.uidSubject
                findUserInTeamIdPs team' subject teamIdPs >>= \case
                  Nothing -> do
                    Logger.info $
                      Log.msg ("Multi-ingress SSO: IdP found but user does not exist, provisioning new user" :: String)
                        . Log.field "team" (idToText (idp ^. idpExtraInfo . team))
                        . Log.field "issuer" (uref ^. SAML.uidTenant . SAML.fromIssuer . to URI.serializeURIRef')
                        . Log.field "multi_ingress_idp" (multiIngressIdp ^. SAML.idpId . to SAML.fromIdPId . to show)
                        . Log.field "authenticating_idp" (idp ^. SAML.idpId . to SAML.fromIdPId . to show)
                        . Log.field "domain" (mbHost & fromMaybe "None")
                    provisionNewUser
                  Just (uid, oldUref) ->
                    do
                      Logger.info $
                        Log.msg ("Multi-ingress SSO: user found via different IdP, migrating issuer" :: String)
                          . Log.field "team" (idToText (idp ^. idpExtraInfo . team))
                          . Log.field "user" (idToText uid)
                          . Log.field "old_issuer" (oldUref ^. SAML.uidTenant . SAML.fromIssuer . to URI.serializeURIRef')
                          . Log.field "new_issuer" (uref ^. SAML.uidTenant . SAML.fromIssuer . to URI.serializeURIRef')
                          . Log.field "authenticating_idp" (idp ^. SAML.idpId . to SAML.fromIdPId . to show)
                          . Log.field "multi_ingress_idp" (multiIngressIdp ^. SAML.idpId . to SAML.fromIdPId . to show)
                          . Log.field "domain" (mbHost & fromMaybe "None")
                      moveUserToNewIssuer oldUref uref uid
                      pure uid
          _ ->
            throwSparSem . SparMultiIngressIdPConfiguration $
              "Multi-ingress SSO only supports email-based NameIDs for cross-IdP migration. "
                <> "Username-based NameIDs are not allowed."

      -- Try to authenticate against all IdPs. In case, return the UserId and the old UserRef.
      findUserInTeamIdPs :: TeamId -> SAML.NameID -> [IdP] -> Sem r (Maybe (UserId, SAML.UserRef))
      findUserInTeamIdPs team'' subject idps = runMaybeT $ asum $ map tryIdP idps
        where
          tryIdP :: IdP -> MaybeT (Sem r) (UserId, SAML.UserRef)
          tryIdP idp' = do
            let oldIssuer = idp' ^. SAML.idpMetadata . SAML.edIssuer
                oldUref = SAML.UserRef oldIssuer subject
            uid <- MaybeT $ findUserWithUref idp' team'' oldUref
            pure (uid, oldUref)

      -- \| Select the authenticating IdP for multi-ingress SSO.
      --
      -- Rules:
      -- 1. If an IdP matches both issuer AND domain, use it (exact match)
      -- 2. If no exact match and there's only ONE IdP for the team, use it (singleton)
      -- 3. If no exact match and multiple IdPs exist, return Nothing (error case)
      selectAuthenticatingIdP :: [IdP] -> Issuer -> Maybe Text -> Sem r (Maybe IdP)
      selectAuthenticatingIdP teamIdPs issuer mbDomain =
        case find matchesIssuerAndDomain teamIdPs of
          Just matchingIdp -> pure $ Just matchingIdp
          Nothing ->
            -- No exact match. Check if singleton IdP case.
            case teamIdPs of
              [singleIdP] -> pure $ Just singleIdP -- Singleton: use for all domains
              _ -> pure Nothing -- Multiple IdPs but no match: error
        where
          matchesIssuerAndDomain idp' =
            idp' ^. SAML.idpMetadata . SAML.edIssuer == issuer
              && idp' ^. idpExtraInfo . domain == mbDomain

-- | If the client is web, it will be served with an HTML page that it can process to decide whether
-- to log the user in or show an error.
--
-- The HTML page is empty and has two ways to communicate the verdict to the js app:
-- - A title element with contents @wire:sso:<outcome>@.  This is chosen to be easily parseable and
--   not be the title of any page sent by the IdP while it negotiates with the user.
-- - The page broadcasts a message to '*', to be picked up by the app.
verdictHandlerWeb :: (HasCallStack) => VerdictHandlerResult -> Sem r SAML.ResponseVerdict
verdictHandlerWeb =
  pure . \case
    VerifyHandlerGranted cky _uid -> successPage cky
    VerifyHandlerDenied reasons -> forbiddenPage "forbidden" (explainDeniedReason <$> reasons)
    VerifyHandlerError lbl msg -> forbiddenPage lbl [msg]
  where
    forbiddenPage :: Text -> [Text] -> SAML.ResponseVerdict
    forbiddenPage errlbl reasons =
      SAML.ResponseVerdict
        ServerError
          { errHTTPCode = 200,
            errReasonPhrase = Text.unpack errlbl, -- (not sure what this is used for)
            errBody =
              easyHtml $
                "<head>"
                  <> "  <title>wire:sso:error:"
                  <> LText.fromStrict errlbl
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
            [ "type" .= ("AUTH_ERROR" :: Text),
              "payload"
                .= object
                  [ "label" .= ("forbidden" :: Text),
                    "errors" .= reasons
                  ]
            ]
    successPage :: SetCookie -> SAML.ResponseVerdict
    successPage cky =
      SAML.ResponseVerdict
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
                ("Set-Cookie", toStrict . Builder.toLazyByteString . renderSetCookie $ cky)
              ]
          }

easyHtml :: LText -> LByteString
easyHtml doc =
  LText.encodeUtf8 $
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
        (throwSparSem . SparCouldNotSubstituteSuccessURI . LText.pack)
        (pure . successPage cky)
  VerifyHandlerDenied reasons ->
    mkVerdictDeniedFormatMobile denied "forbidden"
      & either
        (throwSparSem . SparCouldNotSubstituteFailureURI . LText.pack)
        (pure . forbiddenPage "forbidden" (explainDeniedReason <$> reasons))
  VerifyHandlerError lbl msg ->
    mkVerdictDeniedFormatMobile denied lbl
      & either
        (throwSparSem . SparCouldNotSubstituteFailureURI . LText.pack)
        (pure . forbiddenPage lbl [msg])
  where
    forbiddenPage :: Text -> [Text] -> URI.URI -> SAML.ResponseVerdict
    forbiddenPage errlbl errs uri =
      SAML.ResponseVerdict
        err303
          { errReasonPhrase = Text.unpack errlbl,
            errHeaders =
              [ ("Location", Text.encodeUtf8 $ renderURI uri),
                ("Content-Type", "application/json")
              ],
            errBody = Aeson.encode errs
          }
    successPage :: SetCookie -> URI.URI -> SAML.ResponseVerdict
    successPage cky uri =
      SAML.ResponseVerdict
        err303
          { errReasonPhrase = "success",
            errHeaders =
              [ ("Location", Text.encodeUtf8 $ renderURI uri),
                ("Set-Cookie", toStrict . Builder.toLazyByteString . renderSetCookie $ cky)
              ]
          }

-- | When getting stuck during login finalization, show a nice HTML error rather than the json
-- blob.  Show lots of debugging info for the customer to paste in any issue they might open.
errorPage :: SparError -> [Multipart.Input] -> ServerError
errorPage err mpInputs =
  ServerError
    { errHTTPCode = Http.statusCode $ Wai.code werr,
      errReasonPhrase = LText.unpack $ Wai.label werr,
      errBody = easyHtml $ LText.intercalate "\n" errbody,
      errHeaders = [("Content-Type", "text/html")]
    }
  where
    werr = httpErrorToWaiError $ renderSparError err
    errbody :: [LText]
    errbody =
      [ "<head>",
        "  <title>wire:sso:error:" <> Wai.label werr <> "</title>",
        "</head>",
        "</body>",
        "  sorry, something went wrong :(<br>",
        "  please copy the following debug information to your clipboard and provide it when opening an issue in our customer support.<br><br>",
        "  <pre>"
          <> ( LText.fromStrict
                 . toText
                 . encodeBase64
                 . UTF8.fromString
                 . show
                 $ (err, mpInputs)
             )
          <> "</pre>",
        "</body>"
      ]

-- | Delete all tokens belonging to a team.
deleteTeam ::
  ( HasCallStack,
    ( Member ScimTokenStore r,
      Member SAMLUserStore r,
      Member IdPConfigStore r
    )
  ) =>
  TeamId ->
  Sem r ()
deleteTeam team' = do
  ScimTokenStore.deleteByTeam team'
  -- Since IdPs are not shared between teams, we can look at the set of IdPs
  -- used by the team, and remove everything related to those IdPs, too.
  idps <- IdPConfigStore.getConfigsByTeam team'
  for_ idps $ \idp -> do
    let issuer = idp ^. SAML.idpMetadata . SAML.edIssuer
    SAMLUserStore.deleteByIssuer issuer
    IdPConfigStore.deleteConfig idp

sparToServerErrorWithLogging :: (Member Reporter r) => SparError -> Sem r ServerError
sparToServerErrorWithLogging = fmap httpErrorToServerError . renderSparErrorWithLogging

renderSparErrorWithLogging :: (Member Reporter r) => SparError -> Sem r HttpError
renderSparErrorWithLogging err = do
  let serr = renderSparError err
  Reporter.report Nothing (httpErrorToWaiError serr)
  pure serr
