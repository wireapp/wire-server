{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

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
    GetUserResult (..),
    getUserIdByUref,
    getUserIdByScimExternalId,
    insertUser,
    validateEmailIfExists,
    errorPage,
  )
where

import Bilge
import Brig.Types (ManagedBy (..), User, userId, userTeam)
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
import qualified Data.CaseInsensitive as CI
import Data.Id
import Data.String.Conversions
import Data.Text.Ascii (encodeBase64, toText)
import qualified Data.Text.Lazy as LT
import qualified Data.UUID.V4 as UUID
import Imports hiding (log)
import qualified Network.HTTP.Types.Status as Http
import qualified Network.Wai.Utilities.Error as Wai
import Polysemy
import Polysemy.Final
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
import qualified SAML2.WebSSO.Types.Email as SAMLEmail
import Servant
import qualified Servant.Multipart as Multipart
import qualified Spar.Data as Data hiding (deleteSAMLUser, deleteSAMLUsersByIssuer, getSAMLAnyUserByIssuer, getSAMLSomeUsersByIssuer, getSAMLUser, insertSAMLUser)
import Spar.Error
import qualified Spar.Intra.Brig as Intra
import qualified Spar.Intra.Galley as Intra
import Spar.Orphans ()
import Spar.Sem.SAMLUser (SAMLUser)
import qualified Spar.Sem.SAMLUser as SAMLUser
import Spar.Sem.SAMLUser.Cassandra (interpretClientToIO, samlUserToCassandra)
import qualified System.Logger as Log
import System.Logger.Class (MonadLogger (log))
import URI.ByteString as URI
import Web.Cookie (SetCookie, renderSetCookie)
import Wire.API.Cookie
import Wire.API.User.Identity (Email (..))
import Wire.API.User.IdentityProvider
import Wire.API.User.Saml
import Wire.API.User.Scim (ValidExternalId (..))

newtype Spar r a = Spar {fromSpar :: Member (Final IO) r => ReaderT Env (ExceptT SparError (Sem r)) a}
  deriving (Functor)

liftSem :: (Member (Final IO) r => Sem r a) -> Spar r a
liftSem r = Spar $ lift $ lift r

instance Applicative (Spar r) where
  pure a = Spar $ pure a
  liftA2 f a b = Spar $ liftA2 f (fromSpar a) (fromSpar b)

instance Monad (Spar r) where
  return = pure
  f >>= a = Spar $ fromSpar f >>= fromSpar . a

instance MonadReader Env (Spar r) where
  ask = Spar ask
  local f m = Spar $ local f $ fromSpar m

instance MonadError SparError (Spar r) where
  throwError err = Spar $ throwError err
  catchError m handler = Spar $ catchError (fromSpar m) $ fromSpar . handler

instance MonadIO (Spar r) where
  liftIO m = liftSem $ embedFinal m

data Env = Env
  { sparCtxOpts :: Opts,
    sparCtxLogger :: Log.Logger,
    sparCtxCas :: Cas.ClientState,
    sparCtxHttpManager :: Bilge.Manager,
    sparCtxHttpBrig :: Bilge.Request,
    sparCtxHttpGalley :: Bilge.Request,
    sparCtxRequestId :: RequestId
  }

instance HasConfig (Spar r) where
  getConfig = asks (saml . sparCtxOpts)

instance HasNow (Spar r)

instance HasCreateUUID (Spar r)

instance HasLogger (Spar r) where
  -- FUTUREWORK: optionally use 'field' to index user or idp ids for easier logfile processing.
  logger lv = log (toLevel lv) . Log.msg

instance MonadLogger (Spar r) where
  log level mg = do
    lg <- asks sparCtxLogger
    reqid <- asks sparCtxRequestId
    let fields = Log.field "request" (unRequestId reqid)
    liftSem $ embedFinal $ Log.log lg level $ fields Log.~~ mg

toLevel :: SAML.Level -> Log.Level
toLevel = \case
  SAML.Fatal -> Log.Fatal
  SAML.Error -> Log.Error
  SAML.Warn -> Log.Warn
  SAML.Info -> Log.Info
  SAML.Debug -> Log.Debug
  SAML.Trace -> Log.Trace

instance SPStoreID AuthnRequest (Spar r) where
  storeID i r = wrapMonadClientWithEnv $ Data.storeAReqID i r
  unStoreID r = wrapMonadClient $ Data.unStoreAReqID r
  isAliveID r = wrapMonadClient $ Data.isAliveAReqID r

instance SPStoreID Assertion (Spar r) where
  storeID i r = wrapMonadClientWithEnv $ Data.storeAssID i r
  unStoreID r = wrapMonadClient $ Data.unStoreAssID r
  isAliveID r = wrapMonadClient $ Data.isAliveAssID r

instance SPStoreIdP SparError (Spar r) where
  type IdPConfigExtra (Spar r) = WireIdP
  type IdPConfigSPId (Spar r) = TeamId

  storeIdPConfig :: IdP -> Spar r ()
  storeIdPConfig idp = wrapMonadClient $ Data.storeIdPConfig idp

  getIdPConfig :: IdPId -> Spar r IdP
  getIdPConfig = (>>= maybe (throwSpar (SparIdPNotFound mempty)) pure) . wrapMonadClientWithEnv . Data.getIdPConfig

  getIdPConfigByIssuerOptionalSPId :: Issuer -> Maybe TeamId -> Spar r IdP
  getIdPConfigByIssuerOptionalSPId issuer mbteam = do
    wrapMonadClientWithEnv (Data.getIdPConfigByIssuerAllowOld issuer mbteam) >>= \case
      Data.GetIdPFound idp -> pure idp
      Data.GetIdPNotFound -> throwSpar $ SparIdPNotFound mempty
      res@(Data.GetIdPDanglingId _) -> throwSpar $ SparIdPNotFound (cs $ show res)
      res@(Data.GetIdPNonUnique _) -> throwSpar $ SparIdPNotFound (cs $ show res)
      res@(Data.GetIdPWrongTeam _) -> throwSpar $ SparIdPNotFound (cs $ show res)

-- | 'wrapMonadClient' with an 'Env' in a 'ReaderT', and exceptions. If you
-- don't need either of those, 'wrapMonadClient' will suffice.
wrapMonadClientWithEnv :: forall r a. ReaderT Data.Env (ExceptT TTLError Cas.Client) a -> Spar r a
wrapMonadClientWithEnv action = do
  denv <- Data.mkEnv <$> (sparCtxOpts <$> ask) <*> (fromTime <$> getNow)
  either (throwSpar . SparCassandraTTLError) pure =<< wrapMonadClient (runExceptT $ action `runReaderT` denv)

instance Member (Final IO) r => Catch.MonadThrow (Sem r) where
  throwM = embedFinal . Catch.throwM @IO

instance Member (Final IO) r => Catch.MonadCatch (Sem r) where
  catch m handler = withStrategicToFinal @IO $ do
    m' <- runS m
    st <- getInitialStateS
    handler' <- bindS handler
    pure $ m' `Catch.catch` \e -> handler' $ e <$ st

-- | Call a cassandra command in the 'Spar' monad.  Catch all exceptions and re-throw them as 500 in
-- Handler.
wrapMonadClient :: Cas.Client a -> Spar r a
wrapMonadClient action =
  Spar $ do
    ctx <- asks sparCtxCas
    fromSpar $ wrapMonadClientSem $ embedFinal @IO $ runClient ctx action

-- | Call a 'Sem' command in the 'Spar' monad.  Catch all (IO) exceptions and
-- re-throw them as 500 in Handler.
wrapMonadClientSem :: Sem r a -> Spar r a
wrapMonadClientSem action =
  Spar $
    (lift $ lift action)
      `Catch.catch` (throwSpar . SparCassandraError . cs . show @SomeException)

insertUser :: Member SAMLUser r => SAML.UserRef -> UserId -> Spar r ()
insertUser uref uid = wrapMonadClientSem $ SAMLUser.insert uref uid

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
getUserIdByUref :: Member SAMLUser r => Maybe TeamId -> SAML.UserRef -> Spar r (GetUserResult UserId)
getUserIdByUref mbteam uref = userId <$$> getUserByUref mbteam uref

getUserByUref :: Member SAMLUser r => Maybe TeamId -> SAML.UserRef -> Spar r (GetUserResult User)
getUserByUref mbteam uref = do
  muid <- wrapMonadClientSem $ SAMLUser.get uref
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
getUserIdByScimExternalId :: TeamId -> Email -> Spar r (Maybe UserId)
getUserIdByScimExternalId tid email = do
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
createSamlUserWithId :: Member SAMLUser r => TeamId -> UserId -> SAML.UserRef -> Spar r ()
createSamlUserWithId teamid buid suid = do
  uname <- either (throwSpar . SparBadUserName . cs) pure $ Intra.mkUserName Nothing (UrefOnly suid)
  buid' <- Intra.createBrigUserSAML suid buid teamid uname ManagedByWire
  assert (buid == buid') $ pure ()
  insertUser suid buid

-- | If the team has no scim token, call 'createSamlUser'.  Otherwise, raise "invalid
-- credentials".
autoprovisionSamlUser :: Member SAMLUser r => Maybe TeamId -> SAML.UserRef -> Spar r UserId
autoprovisionSamlUser mbteam suid = do
  buid <- Id <$> liftIO UUID.nextRandom
  autoprovisionSamlUserWithId mbteam buid suid
  pure buid

-- | Like 'autoprovisionSamlUser', but for an already existing 'UserId'.
autoprovisionSamlUserWithId :: forall r. Member SAMLUser r => Maybe TeamId -> UserId -> SAML.UserRef -> Spar r ()
autoprovisionSamlUserWithId mbteam buid suid = do
  idp <- getIdPConfigByIssuerOptionalSPId (suid ^. uidTenant) mbteam
  guardReplacedIdP idp
  guardScimTokens idp
  createSamlUserWithId (idp ^. idpExtraInfo . wiTeam) buid suid
  validateEmailIfExists buid suid
  where
    -- Replaced IdPs are not allowed to create new wire accounts.
    guardReplacedIdP :: IdP -> Spar r ()
    guardReplacedIdP idp = do
      unless (isNothing $ idp ^. idpExtraInfo . wiReplacedBy) $ do
        throwSpar $ SparCannotCreateUsersOnReplacedIdP (cs . SAML.idPIdToST $ idp ^. idpId)

    -- IdPs in teams with scim tokens are not allowed to auto-provision.
    guardScimTokens :: IdP -> Spar r ()
    guardScimTokens idp = do
      let teamid = idp ^. idpExtraInfo . wiTeam
      scimtoks <- wrapMonadClient $ Data.getScimTokens teamid
      unless (null scimtoks) $ do
        throwSpar SparSamlCredentialsNotFound

-- | If user's 'NameID' is an email address and the team has email validation for SSO enabled,
-- make brig initiate the email validate procedure.
validateEmailIfExists :: forall r. UserId -> SAML.UserRef -> Spar r ()
validateEmailIfExists uid = \case
  (SAML.UserRef _ (view SAML.nameID -> UNameIDEmail email)) -> doValidate (CI.original email)
  _ -> pure ()
  where
    doValidate :: SAMLEmail.Email -> Spar r ()
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
bindUser :: Member SAMLUser r => UserId -> SAML.UserRef -> Spar r UserId
bindUser buid userref = do
  oldStatus <- do
    let err :: Spar r a
        err = throwSpar . SparBindFromWrongOrNoTeam . cs . show $ buid
    teamid :: TeamId <-
      wrapMonadClient (Data.getIdPConfigByIssuerAllowOld (userref ^. uidTenant) Nothing) >>= \case
        Data.GetIdPFound idp -> pure $ idp ^. idpExtraInfo . wiTeam
        Data.GetIdPNotFound -> err
        Data.GetIdPDanglingId _ -> err -- database inconsistency
        Data.GetIdPNonUnique is -> throwSpar $ SparUserRefInNoOrMultipleTeams (cs $ show (buid, is))
        Data.GetIdPWrongTeam _ -> err -- impossible
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

instance (r ~ '[SAMLUser, Embed (Cas.Client), Embed IO, Final IO]) => SPHandler SparError (Spar r) where
  type NTCTX (Spar r) = Env
  nt :: forall a. Env -> Spar r a -> Handler a
  nt ctx (Spar action) = do
    err <- actionHandler
    throwErrorAsHandlerException err
    where
      actionHandler :: Handler (Either SparError a)
      actionHandler = liftIO $ runFinal $ embedToFinal @IO $ interpretClientToIO (sparCtxCas ctx) $ samlUserToCassandra @Cas.Client $ runExceptT $ runReaderT action ctx
      throwErrorAsHandlerException :: Either SparError a -> Handler a
      throwErrorAsHandlerException (Left err) =
        sparToServerErrorWithLogging (sparCtxLogger ctx) err >>= throwError
      throwErrorAsHandlerException (Right a) = pure a

instance MonadHttp (Spar r) where
  handleRequestWithCont req handler = do
    manager <- asks sparCtxHttpManager
    liftIO $ withResponse req manager handler

instance Intra.MonadSparToBrig (Spar r) where
  call modreq = do
    req <- asks sparCtxHttpBrig
    httpLbs req modreq

instance Intra.MonadSparToGalley (Spar r) where
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
verdictHandler :: HasCallStack => Member SAMLUser r => Maybe BindCookie -> Maybe TeamId -> SAML.AuthnResponse -> SAML.AccessVerdict -> Spar r SAML.ResponseVerdict
verdictHandler cky mbteam aresp verdict = do
  -- [3/4.1.4.2]
  -- <SubjectConfirmation> [...] If the containing message is in response to an <AuthnRequest>, then
  -- the InResponseTo attribute MUST match the request's ID.
  SAML.logger SAML.Debug $ "entering verdictHandler: " <> show (fromBindCookie <$> cky, aresp, verdict)
  reqid <- either (throwSpar . SparNoRequestRefInResponse . cs) pure $ SAML.rspInResponseTo aresp
  format :: Maybe VerdictFormat <- wrapMonadClient $ Data.getVerdictFormat reqid
  resp <- case format of
    Just (VerdictFormatWeb) ->
      verdictHandlerResult cky mbteam verdict >>= verdictHandlerWeb
    Just (VerdictFormatMobile granted denied) ->
      verdictHandlerResult cky mbteam verdict >>= verdictHandlerMobile granted denied
    Nothing ->
      -- (this shouldn't happen too often, see 'storeVerdictFormat')
      throwSpar SparNoSuchRequest
  SAML.logger SAML.Debug $ "leaving verdictHandler: " <> show resp
  pure resp

data VerdictHandlerResult
  = VerifyHandlerGranted {_vhrCookie :: SetCookie, _vhrUserId :: UserId}
  | VerifyHandlerDenied {_vhrReasons :: [SAML.DeniedReason]}
  | VerifyHandlerError {_vhrLabel :: ST, _vhrMessage :: ST}
  deriving (Eq, Show)

verdictHandlerResult :: HasCallStack => Member SAMLUser r => Maybe BindCookie -> Maybe TeamId -> SAML.AccessVerdict -> Spar r VerdictHandlerResult
verdictHandlerResult bindCky mbteam verdict = do
  SAML.logger SAML.Debug $ "entering verdictHandlerResult: " <> show (fromBindCookie <$> bindCky)
  result <- catchVerdictErrors $ verdictHandlerResultCore bindCky mbteam verdict
  SAML.logger SAML.Debug $ "leaving verdictHandlerResult" <> show result
  pure result

catchVerdictErrors :: Spar r VerdictHandlerResult -> Spar r VerdictHandlerResult
catchVerdictErrors = (`catchError` hndlr)
  where
    hndlr :: SparError -> Spar r VerdictHandlerResult
    hndlr err = do
      logr <- asks sparCtxLogger
      waiErr <- renderSparErrorWithLogging logr err
      pure $ case waiErr of
        Right (werr :: Wai.Error) -> VerifyHandlerError (cs $ Wai.label werr) (cs $ Wai.message werr)
        Left (serr :: ServerError) -> VerifyHandlerError "unknown-error" (cs (errReasonPhrase serr) <> " " <> cs (errBody serr))

-- | If a user attempts to login presenting a new IdP issuer, but there is no entry in
-- @"spar.user"@ for her: lookup @"old_issuers"@ from @"spar.idp"@ for the new IdP, and
-- traverse the old IdPs in search for the old entry.  Return that old entry.
findUserIdWithOldIssuer :: forall r. Member SAMLUser r => Maybe TeamId -> SAML.UserRef -> Spar r (GetUserResult (SAML.UserRef, UserId))
findUserIdWithOldIssuer mbteam (SAML.UserRef issuer subject) = do
  idp <- getIdPConfigByIssuerOptionalSPId issuer mbteam
  let tryFind :: GetUserResult (SAML.UserRef, UserId) -> Issuer -> Spar r (GetUserResult (SAML.UserRef, UserId))
      tryFind found@(GetUserFound _) _ = pure found
      tryFind _ oldIssuer = (uref,) <$$> getUserIdByUref mbteam uref
        where
          uref = SAML.UserRef oldIssuer subject
  foldM tryFind GetUserNotFound (idp ^. idpExtraInfo . wiOldIssuers)

-- | After a user has been found using 'findUserWithOldIssuer', update it everywhere so that
-- the old IdP is not needed any more next time.
moveUserToNewIssuer :: Member SAMLUser r => SAML.UserRef -> SAML.UserRef -> UserId -> Spar r ()
moveUserToNewIssuer oldUserRef newUserRef uid = do
  wrapMonadClientSem $ SAMLUser.insert newUserRef uid
  Intra.setBrigUserVeid uid (UrefOnly newUserRef)
  wrapMonadClientSem $ SAMLUser.delete uid oldUserRef

verdictHandlerResultCore :: HasCallStack => Member SAMLUser r => Maybe BindCookie -> Maybe TeamId -> SAML.AccessVerdict -> Spar r VerdictHandlerResult
verdictHandlerResultCore bindCky mbteam = \case
  SAML.AccessDenied reasons -> do
    pure $ VerifyHandlerDenied reasons
  SAML.AccessGranted userref -> do
    uid :: UserId <- do
      viaBindCookie <- maybe (pure Nothing) (wrapMonadClient . Data.lookupBindCookie) bindCky
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
        (_, GetUserNoTeam, _) -> throwSpar err
        (_, GetUserWrongTeam, _) -> throwSpar err
        (_, _, GetUserNoTeam) -> throwSpar err
        (_, _, GetUserWrongTeam) -> throwSpar err
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
          | otherwise -> throwSpar SparBindUserRefTaken
        -- same two cases as above, but between last login and bind there was an issuer update.
        (Just uid, GetUserNotFound, GetUserFound (oldUserRef, uid'))
          | uid == uid' -> moveUserToNewIssuer oldUserRef userref uid >> pure uid
          | otherwise -> throwSpar SparBindUserRefTaken
        (Just _, GetUserFound _, GetUserFound _) ->
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
verdictHandlerWeb :: HasCallStack => VerdictHandlerResult -> Spar r SAML.ResponseVerdict
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
verdictHandlerMobile :: HasCallStack => URI.URI -> URI.URI -> VerdictHandlerResult -> Spar r SAML.ResponseVerdict
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
    forceWai ServerError {..} = Wai.mkError (Http.Status errHTTPCode "") (cs errReasonPhrase) (cs errBody)
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
