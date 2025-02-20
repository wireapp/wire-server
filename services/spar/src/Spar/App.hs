{-# LANGUAGE RecordWildCards #-}
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
import Data.Aeson as Aeson (encode, object, (.=))
import Data.Aeson.Text as Aeson (encodeToLazyText)
import Data.ByteString (toStrict)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.CaseInsensitive as CI
import Data.Id
import qualified Data.Text as Text
import Data.Text.Ascii (encodeBase64, toText)
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error
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
import qualified Spar.Intra.BrigApp as Intra
import Spar.Options
import Spar.Orphans ()
import Spar.Sem.AReqIDStore (AReqIDStore)
import Spar.Sem.BrigAccess (BrigAccess, getAccount)
import qualified Spar.Sem.BrigAccess as BrigAccess
import Spar.Sem.GalleyAccess (GalleyAccess)
import qualified Spar.Sem.GalleyAccess as GalleyAccess
import Spar.Sem.IdPConfigStore (IdPConfigStore)
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
import Wire.API.Team.Role (Role, defaultRole)
import Wire.API.User
import Wire.API.User.IdentityProvider
import Wire.API.User.Saml
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
    sparCtxRequestId :: RequestId
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
  ( Member BrigAccess r,
    Member SAMLUserStore r
  ) =>
  SAML.UserRef ->
  Sem r (Maybe User)
getUserByUrefUnsafe uref = do
  maybe (pure Nothing) (getAccount Intra.WithPendingInvitations) =<< SAMLUserStore.get uref

-- FUTUREWORK: Remove and reinstatate getUser, in AuthID refactoring PR
getUserIdByScimExternalId ::
  ( Member BrigAccess r,
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
    Member BrigAccess r,
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
  buid' <- BrigAccess.createSAML suid buid teamid uname ManagedByWire Nothing Nothing Nothing role
  assert (buid == buid') $ pure ()
  SAMLUserStore.insert suid buid

-- | If the team has no scim token, call 'createSamlUser'.  Otherwise, raise "invalid
-- credentials".  (FUTUREWORK: Assumes that `UserRef` is still available globally.  See
-- https://wearezeta.atlassian.net/browse/SQSERVICES-1655)
autoprovisionSamlUser ::
  forall r.
  ( Member GalleyAccess r,
    Member BrigAccess r,
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
  ( Member GalleyAccess r,
    Member BrigAccess r
  ) =>
  UserId ->
  SAML.UserRef ->
  Sem r ()
validateSamlEmailIfExists uid = \case
  (SAML.UserRef _ (view SAML.nameID -> UNameIDEmail samlEmail)) -> do
    mbTid <- Intra.getBrigUserTeam Intra.NoPendingInvitations uid
    let email = Intra.emailFromSAML . CI.original $ samlEmail
    unlessM (Intra.emailDomainIsRegisteredForSSO email) $
      validateEmail mbTid uid email
  _ -> pure ()

validateEmail ::
  forall r.
  ( Member GalleyAccess r,
    Member BrigAccess r
  ) =>
  Maybe TeamId ->
  UserId ->
  EmailAddress ->
  Sem r ()
validateEmail mbTid uid email = do
  enabled <- maybe (pure False) GalleyAccess.isEmailValidationEnabledTeam mbTid
  when enabled $ do
    BrigAccess.updateEmail uid email

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
    Member (Logger String) r,
    Member GalleyAccess r,
    Member BrigAccess r,
    Member AReqIDStore r,
    Member VerdictFormatStore r,
    Member ScimTokenStore r,
    Member IdPConfigStore r,
    Member (Error SparError) r,
    Member Reporter r,
    Member SAMLUserStore r
  ) =>
  SAML.AuthnResponse ->
  SAML.AccessVerdict ->
  IdP ->
  Sem r SAML.ResponseVerdict
verdictHandler aresp verdict idp = do
  -- [3/4.1.4.2]
  -- <SubjectConfirmation> [...] If the containing message is in response to an <AuthnRequest>, then
  -- the InResponseTo attribute MUST match the request's ID.
  Logger.log Logger.Debug $ "entering verdictHandler: " <> show (aresp, verdict)
  reqid <- either (throwSparSem . SparNoRequestRefInResponse . LText.pack) pure $ SAML.rspInResponseTo aresp
  format :: Maybe VerdictFormat <- VerdictFormatStore.get reqid
  resp <- case format of
    Just VerdictFormatWeb ->
      verdictHandlerResult verdict idp >>= verdictHandlerWeb
    Just (VerdictFormatMobile granted denied) ->
      verdictHandlerResult verdict idp >>= verdictHandlerMobile granted denied
    Nothing ->
      -- (this shouldn't happen too often, see 'storeVerdictFormat')
      throwSparSem SparNoSuchRequest
  Logger.log Logger.Debug $ "leaving verdictHandler: " <> show resp
  pure resp

data VerdictHandlerResult
  = VerifyHandlerGranted {_vhrCookie :: SetCookie, _vhrUserId :: UserId}
  | VerifyHandlerDenied {_vhrReasons :: [SAML.DeniedReason]}
  | VerifyHandlerError {_vhrLabel :: Text, _vhrMessage :: Text}
  deriving (Eq, Show)

verdictHandlerResult ::
  (HasCallStack) =>
  ( Member Random r,
    Member (Logger String) r,
    Member GalleyAccess r,
    Member BrigAccess r,
    Member ScimTokenStore r,
    Member IdPConfigStore r,
    Member (Error SparError) r,
    Member Reporter r,
    Member SAMLUserStore r
  ) =>
  SAML.AccessVerdict ->
  IdP ->
  Sem r VerdictHandlerResult
verdictHandlerResult verdict idp = do
  Logger.log Logger.Debug $ "entering verdictHandlerResult"
  result <- catchVerdictErrors $ verdictHandlerResultCore idp verdict
  Logger.log Logger.Debug $ "leaving verdictHandlerResult" <> show result
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
      waiErr <- renderSparErrorWithLogging err
      pure $ case waiErr of
        Right (werr :: Wai.Error) ->
          VerifyHandlerError
            (LText.toStrict $ Wai.label werr)
            (LText.toStrict $ Wai.message werr)
        Left (serr :: ServerError) ->
          VerifyHandlerError
            "unknown-error"
            ( Text.pack (errReasonPhrase serr)
                <> " "
                <> (Text.decodeUtf8With lenientDecode . toStrict . errBody $ serr)
            )

-- | If a user attempts to login presenting a new IdP issuer, but there is no entry in
-- @"spar.user"@ for her: lookup @"old_issuers"@ from @"spar.idp"@ for the new IdP, and
-- traverse the old issuers in search for the old entry.
--
-- FUTUREWORK: https://wearezeta.atlassian.net/browse/SQSERVICES-1655
getUserByUrefViaOldIssuerUnsafe ::
  forall r.
  ( Member BrigAccess r,
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
  ( Member BrigAccess r,
    Member SAMLUserStore r
  ) =>
  SAML.UserRef ->
  SAML.UserRef ->
  UserId ->
  Sem r ()
moveUserToNewIssuer oldUserRef newUserRef uid = do
  SAMLUserStore.insert newUserRef uid
  BrigAccess.setSSOId uid (UserSSOId newUserRef)
  SAMLUserStore.delete uid oldUserRef

verdictHandlerResultCore ::
  (HasCallStack) =>
  ( Member Random r,
    Member (Logger String) r,
    Member GalleyAccess r,
    Member BrigAccess r,
    Member ScimTokenStore r,
    Member IdPConfigStore r,
    Member (Error SparError) r,
    Member SAMLUserStore r
  ) =>
  IdP ->
  SAML.AccessVerdict ->
  Sem r VerdictHandlerResult
verdictHandlerResultCore idp = \case
  SAML.AccessDenied reasons -> do
    pure $ VerifyHandlerDenied reasons
  SAML.AccessGranted uref -> do
    uid :: UserId <- do
      let team' = idp ^. idpExtraInfo . team
          err = SparUserRefInNoOrMultipleTeams . LText.pack . show $ uref
      getUserByUrefUnsafe uref >>= \case
        Just usr -> do
          if userTeam usr == Just team'
            then pure (userId usr)
            else throwSparSem err
        Nothing -> do
          getUserByUrefViaOldIssuerUnsafe idp uref >>= \case
            Just (olduref, usr) -> do
              let uid = userId usr
              if userTeam usr == Just team'
                then moveUserToNewIssuer olduref uref uid >> pure uid
                else throwSparSem err
            Nothing -> do
              buid <- Id <$> Random.uuid
              autoprovisionSamlUser idp buid uref
              validateSamlEmailIfExists buid uref
              pure buid

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
verdictHandlerWeb :: (HasCallStack) => VerdictHandlerResult -> Sem r SAML.ResponseVerdict
verdictHandlerWeb =
  pure . \case
    VerifyHandlerGranted cky _uid -> successPage cky
    VerifyHandlerDenied reasons -> forbiddenPage "forbidden" (explainDeniedReason <$> reasons)
    VerifyHandlerError lbl msg -> forbiddenPage lbl [msg]
  where
    forbiddenPage :: Text -> [Text] -> SAML.ResponseVerdict
    forbiddenPage errlbl reasons =
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
    werr = either forceWai id $ renderSparError err
    forceWai ServerError {..} =
      Wai.mkError
        (Http.Status errHTTPCode "")
        (LText.pack errReasonPhrase)
        (LText.decodeUtf8With lenientDecode errBody)
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
sparToServerErrorWithLogging err = do
  let errServant = sparToServerError err
  Reporter.report Nothing (servantToWaiError errServant)
  pure errServant

renderSparErrorWithLogging :: (Member Reporter r) => SparError -> Sem r (Either ServerError Wai.Error)
renderSparErrorWithLogging err = do
  let errPossiblyWai = renderSparError err
  Reporter.report Nothing (either servantToWaiError id $ errPossiblyWai)
  pure errPossiblyWai
