{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Wire.EnterpriseLoginSubsystem.Interpreter
  ( runEnterpriseLoginSubsystemWithConfig,
    runEnterpriseLoginSubsystem,
    EnterpriseLoginSubsystemConfig (..),
    EnterpriseLoginSubsystemEmailConfig (..),
  )
where

import Bilge hiding (delete)
import Control.Lens ((^.), (^..), (^?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.ByteString.Conversion (toByteString')
import Data.ByteString.Lazy qualified as BL
import Data.Default
import Data.Domain
import Data.Id
import Data.Misc (HttpsUrl (..))
import Data.Qualified
import Data.Text.Encoding as T
import Data.Text.Encoding qualified as Text
import Data.Text.Internal.Builder (fromLazyText, fromText, toLazyText)
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Encoding as LT
import Imports hiding (lookup)
import Network.HTTP.Types.Method
import Network.Mail.Mime (Address (Address), Mail (mailHeaders, mailParts, mailTo), emptyMail, plainPart)
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
import SAML2.WebSSO qualified as SAML
import System.Logger.Message qualified as Log
import Text.Email.Parser qualified as Email
import Util.Options
import Wire.API.EnterpriseLogin
import Wire.API.Routes.Public.Brig.DomainVerification
import Wire.API.Team.Feature
import Wire.API.Team.Member
import Wire.API.User hiding (NewUser)
import Wire.API.User.IdentityProvider (providers)
import Wire.DomainRegistrationStore
import Wire.EmailSending (EmailSending, sendMail)
import Wire.EnterpriseLoginSubsystem
import Wire.EnterpriseLoginSubsystem.Error
import Wire.GalleyAPIAccess
import Wire.ParseException
import Wire.Rpc
import Wire.Sem.Random
import Wire.SparAPIAccess
import Wire.UserKeyStore
import Wire.UserSubsystem

data EnterpriseLoginSubsystemEmailConfig = EnterpriseLoginSubsystemEmailConfig
  { auditEmailSender :: EmailAddress,
    auditEmailRecipient :: EmailAddress
  }

data EnterpriseLoginSubsystemConfig = EnterpriseLoginSubsystemConfig
  { emailConfig :: Maybe EnterpriseLoginSubsystemEmailConfig,
    wireServerEnterpriseEndpoint :: Endpoint
  }

runEnterpriseLoginSubsystemWithConfig ::
  ( Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member (Error ParseException) r,
    Member GalleyAPIAccess r,
    Member SparAPIAccess r,
    Member TinyLog r,
    Member EmailSending r,
    Member Random r,
    Member Rpc r,
    Member UserKeyStore r,
    Member UserSubsystem r
  ) =>
  EnterpriseLoginSubsystemConfig ->
  Sem (EnterpriseLoginSubsystem ': r) a ->
  Sem r a
runEnterpriseLoginSubsystemWithConfig config =
  runInputConst config
    . runEnterpriseLoginSubsystem
    . raiseUnder

runEnterpriseLoginSubsystem ::
  ( Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member (Error ParseException) r,
    Member GalleyAPIAccess r,
    Member SparAPIAccess r,
    Member TinyLog r,
    Member (Input EnterpriseLoginSubsystemConfig) r,
    Member EmailSending r,
    Member Random r,
    Member Rpc r,
    Member UserKeyStore r,
    Member UserSubsystem r
  ) =>
  Sem (EnterpriseLoginSubsystem ': r) a ->
  Sem r a
runEnterpriseLoginSubsystem = interpret $
  \case
    LockDomain domain -> lockDomainImpl domain
    UnlockDomain domain -> unlockDomainImpl domain
    PreAuthorizeDomain domain -> preAuthorizeImpl domain
    UnAuthorizeDomain domain -> unauthorizeImpl domain
    UpdateDomainRegistration domain update -> updateDomainRegistrationImpl domain update
    DeleteDomain domain -> deleteDomainImpl domain
    GetDomainRegistration domain -> getDomainRegistrationImpl domain
    GuardEmailDomainRegistrationTeamInvitation flow tid email -> guardEmailDomainRegistrationTeamInvitationImpl flow tid email
    GuardEmailDomainRegistrationRegister email -> guardEmailDomainRegistrationRegisterImpl email
    TryGetDomainRegistration domain -> tryGetDomainRegistrationImpl domain
    RequestDomainVerificationToken mAuthToken domain ->
      runInputSem (wireServerEnterpriseEndpoint <$> input) $
        requestDomainVerificationTokenImpl mAuthToken domain
    RequestDomainVerificationTeamToken lusr domain ->
      runInputSem (wireServerEnterpriseEndpoint <$> input) $
        requestDomainVerificationTeamTokenImpl lusr domain
    UpdateDomainRedirect mAuthToken domain config ->
      runInputSem (wireServerEnterpriseEndpoint <$> input) $
        updateDomainRedirectImpl mAuthToken domain config
    UpdateTeamInvite lusr domain config ->
      runInputSem (wireServerEnterpriseEndpoint <$> input) $
        updateTeamInviteImpl lusr domain config
    GetDomainRegistrationPublic req ->
      getDomainRegistrationPublicImpl req

deleteDomainImpl ::
  ( Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member TinyLog r,
    Member (Input EnterpriseLoginSubsystemConfig) r,
    Member EmailSending r
  ) =>
  Domain ->
  Sem r ()
deleteDomainImpl domain = do
  mOld <- tryGetDomainRegistrationImpl domain
  sendAuditMail url "Domain deleted" mOld Nothing
  delete domain
  where
    url :: Builder
    url =
      "DELETE /i/domain-registration/"
        <> fromText (domainText domain)

unauthorizeImpl ::
  forall r.
  ( Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member TinyLog r,
    Member (Input EnterpriseLoginSubsystemConfig) r,
    Member EmailSending r
  ) =>
  Domain ->
  Sem r ()
unauthorizeImpl domain = do
  old <- getDomainRegistrationImpl domain
  let new = old {domainRedirect = None} :: DomainRegistration
  case old.domainRedirect of
    PreAuthorized -> audit old new *> upsert (toStored new)
    Backend _ -> audit old new *> upsert (toStored new)
    NoRegistration -> audit old new *> upsert (toStored new)
    None -> pure ()
    Locked -> throw EnterpriseLoginSubsystemUnAuthorizeError
    SSO _ -> throw EnterpriseLoginSubsystemUnAuthorizeError
  where
    audit :: DomainRegistration -> DomainRegistration -> Sem r ()
    audit old new = sendAuditMail url "Domain unauthorized" (Just old) (Just new)

    url :: Builder
    url =
      "POST /i/domain-registration/"
        <> fromText (domainText domain)
        <> "/unauthorized"

updateDomainRegistrationImpl ::
  forall r.
  ( Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member TinyLog r,
    Member (Input EnterpriseLoginSubsystemConfig) r,
    Member EmailSending r
  ) =>
  Domain ->
  DomainRegistrationUpdate ->
  Sem r ()
updateDomainRegistrationImpl domain update = do
  validate update
  mOld <- (>>= fromStored) <$> lookup domain
  case mOld of
    Just dr -> do
      let new = dr {teamInvite = update.teamInvite, domainRedirect = update.domainRedirect} :: DomainRegistration
      audit mOld new *> upsert (toStored new)
    Nothing -> do
      let new = DomainRegistration domain update.domainRedirect update.teamInvite Nothing
      audit mOld new *> upsert (toStored new)
  where
    audit :: Maybe DomainRegistration -> DomainRegistration -> Sem r ()
    audit old new = sendAuditMail url "Domain registration updated" old (Just new)

    url :: Builder
    url =
      "PUT /i/domain-registration/"
        <> fromText (domainText domain)

lockDomainImpl ::
  forall r.
  ( Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member TinyLog r,
    Member (Input EnterpriseLoginSubsystemConfig) r,
    Member EmailSending r
  ) =>
  Domain ->
  Sem r ()
lockDomainImpl domain = do
  mOld <- tryGetDomainRegistrationImpl domain
  let new = DomainRegistration domain Locked Allowed Nothing
  audit mOld new *> upsert (toStored new)
  where
    url :: Builder
    url =
      "POST /i/domain-registration/"
        <> fromText (domainText domain)
        <> "/lock"

    audit :: Maybe DomainRegistration -> DomainRegistration -> Sem r ()
    audit old new = sendAuditMail url "Domain locked" old (Just new)

unlockDomainImpl ::
  forall r.
  ( Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member TinyLog r,
    Member (Input EnterpriseLoginSubsystemConfig) r,
    Member EmailSending r
  ) =>
  Domain ->
  Sem r ()
unlockDomainImpl domain = do
  old <- getDomainRegistrationImpl domain
  let new = old {domainRedirect = None} :: DomainRegistration
  case old.domainRedirect of
    Locked -> audit old new *> upsert (toStored new)
    _ -> throw EnterpriseLoginSubsystemUnlockError
  where
    url :: Builder
    url =
      "POST /i/domain-registration/"
        <> fromText (domainText domain)
        <> "/unlock"

    audit :: DomainRegistration -> DomainRegistration -> Sem r ()
    audit old new = sendAuditMail url "Domain locked" (Just old) (Just new)

preAuthorizeImpl ::
  forall r.
  ( Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member TinyLog r,
    Member (Input EnterpriseLoginSubsystemConfig) r,
    Member EmailSending r
  ) =>
  Domain ->
  Sem r ()
preAuthorizeImpl domain = do
  mOld <- tryGetDomainRegistrationImpl domain
  case mOld of
    Nothing -> do
      let new = DomainRegistration domain PreAuthorized Allowed Nothing
      audit mOld new *> upsert (toStored new)
    Just old | old.domainRedirect == None -> do
      let new = old {domainRedirect = PreAuthorized} :: DomainRegistration
      audit (Just old) new *> upsert (toStored new)
    Just old | old.domainRedirect == PreAuthorized -> pure ()
    _ -> throw $ EnterpriseLoginSubsystemPreAuthorizeError
  where
    url :: Builder
    url =
      "POST /i/domain-registration/"
        <> fromText (domainText domain)
        <> "/preauthorize"

    audit :: Maybe DomainRegistration -> DomainRegistration -> Sem r ()
    audit old new = sendAuditMail url "Domain locked" old (Just new)

getDomainRegistrationImpl ::
  ( Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member TinyLog r
  ) =>
  Domain ->
  Sem r DomainRegistration
getDomainRegistrationImpl domain = do
  mSdr <- tryGetDomainRegistrationImpl domain
  case mSdr of
    Just dr -> pure dr
    Nothing -> throw EnterpriseLoginSubsystemErrorNotFound

tryGetDomainRegistrationImpl ::
  forall r.
  ( Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member TinyLog r
  ) =>
  Domain ->
  Sem r (Maybe DomainRegistration)
tryGetDomainRegistrationImpl domain = do
  mSdr <- lookup domain
  maybe (pure Nothing) (fmap Just . fromStoredWithExcept) mSdr
  where
    fromStoredWithExcept :: StoredDomainRegistration -> Sem r DomainRegistration
    fromStoredWithExcept sdr = do
      case fromStored sdr of
        Nothing -> do
          Log.err $ Log.field "domain" (toByteString' domain) . Log.msg (Log.val "Invalid stored domain registration")
          throw $ EnterpriseLoginSubsystemInternalError "The stored domain registration is invalid. Please update or delete and recreate it with a valid configuration."
        Just dr -> pure dr

getDomainVerificationToken ::
  ( Member (Error ParseException) r,
    Member (Input Endpoint) r,
    Member Rpc r
  ) =>
  Domain ->
  Text ->
  Sem r DomainVerificationToken
getDomainVerificationToken domain authToken =
  decodeBodyOrThrow
    =<< enterpriseRequest
      ( method POST
          . paths
            [ "i",
              "create-verification-token",
              toByteString' domain,
              toByteString' authToken
            ]
          . expect2xx
      )

verifyDNSRecord ::
  ( Member (Error ParseException) r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member (Input Endpoint) r,
    Member Rpc r
  ) =>
  Domain ->
  Text ->
  Sem r ()
verifyDNSRecord domain authToken = do
  verified <-
    decodeBodyOrThrow
      =<< enterpriseRequest
        ( method POST
            . paths
              [ "i",
                "verify-domain-token",
                toByteString' domain,
                toByteString' authToken
              ]
            . expect2xx
        )
  unless verified $
    throw EnterpriseLoginSubsystemDomainVerificationFailed

enterpriseRequest :: (Member Rpc r, Member (Input Endpoint) r) => (Request -> Request) -> Sem r (Response (Maybe LByteString))
enterpriseRequest req = do
  ep <- input
  rpcWithRetries "wireServerEnterprise" ep req

decodeBodyOrThrow :: forall a r. (Typeable a, Aeson.FromJSON a, Member (Error ParseException) r) => Response (Maybe BL.ByteString) -> Sem r a
decodeBodyOrThrow r = either (throw . ParseException "wireServerEnterprise") pure (responseJsonEither r)

fromStored :: StoredDomainRegistration -> Maybe DomainRegistration
fromStored sdr =
  DomainRegistration sdr.domain
    <$> getDomainRedirect sdr
    <*> getTeamInvite sdr
    <*> pure sdr.dnsVerificationToken
  where
    getTeamInvite :: StoredDomainRegistration -> Maybe TeamInvite
    getTeamInvite = \case
      StoredDomainRegistration _ _ ti _ _ tid _ _ -> case (ti, tid) of
        (Just AllowedTag, Nothing) -> Just Allowed
        (Just NotAllowedTag, Nothing) -> Just NotAllowed
        (Just TeamTag, Just teamId) -> Just $ Team teamId
        (Nothing, Nothing) -> Just Allowed
        _ -> Nothing

    getDomainRedirect :: StoredDomainRegistration -> Maybe DomainRedirect
    getDomainRedirect = \case
      StoredDomainRegistration _ dr _ ssoId url _ _ _ -> case (dr, ssoId, url) of
        (Just NoneTag, Nothing, Nothing) -> Just None
        (Just LockedTag, Nothing, Nothing) -> Just Locked
        (Just PreAuthorizedTag, Nothing, Nothing) -> Just PreAuthorized
        (Just SSOTag, Just idpId, Nothing) -> Just $ SSO idpId
        (Just BackendTag, Nothing, Just beUrl) -> Just $ Backend beUrl
        (Just NoRegistrationTag, Nothing, Nothing) -> Just NoRegistration
        (Nothing, Nothing, Nothing) -> Just None
        _ -> Nothing

toStored :: DomainRegistration -> StoredDomainRegistration
toStored dr =
  let (domainRedirect, idpId, backendUrl) = fromDomainRedirect dr.domainRedirect
      (teamInvite, team) = fromTeamInvite dr.teamInvite
   in StoredDomainRegistration dr.domain (Just domainRedirect) (Just teamInvite) idpId backendUrl team dr.dnsVerificationToken Nothing
  where
    fromTeamInvite :: TeamInvite -> (TeamInviteTag, Maybe TeamId)
    fromTeamInvite Allowed = (AllowedTag, Nothing)
    fromTeamInvite NotAllowed = (NotAllowedTag, Nothing)
    fromTeamInvite (Team teamId) = (TeamTag, Just teamId)

    fromDomainRedirect :: DomainRedirect -> (DomainRedirectTag, Maybe SAML.IdPId, Maybe HttpsUrl)
    fromDomainRedirect None = (NoneTag, Nothing, Nothing)
    fromDomainRedirect Locked = (LockedTag, Nothing, Nothing)
    fromDomainRedirect (SSO idpId) = (SSOTag, Just idpId, Nothing)
    fromDomainRedirect (Backend url) = (BackendTag, Nothing, Just url)
    fromDomainRedirect NoRegistration = (NoRegistrationTag, Nothing, Nothing)
    fromDomainRedirect PreAuthorized = (PreAuthorizedTag, Nothing, Nothing)

validate :: (Member (Error EnterpriseLoginSubsystemError) r) => DomainRegistrationUpdate -> Sem r ()
validate dr = do
  case dr.domainRedirect of
    Locked -> when (dr.teamInvite /= Allowed) $ throw (EnterpriseLoginSubsystemErrorUpdateFailure "Team invite must be allowed for a locked domain")
    Backend _ -> when (dr.teamInvite /= NotAllowed) $ throw (EnterpriseLoginSubsystemErrorUpdateFailure "Team invite must not be allowed for a backend domain")
    _ -> pure ()

mkAuditMail :: EmailAddress -> EmailAddress -> Text -> LText -> Mail
mkAuditMail from to subject bdy =
  (emptyMail (Address Nothing (fromEmail from)))
    { mailTo = [Address Nothing (fromEmail to)],
      mailHeaders =
        [ ("Subject", subject),
          ("X-Zeta-Purpose", "audit")
        ],
      mailParts = [[plainPart bdy]]
    }

sendAuditMail ::
  ( Member (Input EnterpriseLoginSubsystemConfig) r,
    Member TinyLog r,
    Member EmailSending r
  ) =>
  Builder ->
  Text ->
  Maybe DomainRegistration ->
  Maybe DomainRegistration ->
  Sem r ()
sendAuditMail url subject mBefore mAfter = do
  let auditLog :: LText =
        toLazyText $
          url
            <> " called;\nOld value:\n"
            <> fromLazyText (LT.decodeUtf8 (maybe "null" Aeson.encodePretty mBefore))
            <> "\nNew value:\n"
            <> fromLazyText (LT.decodeUtf8 (maybe "null" Aeson.encodePretty mAfter))
  Log.info $
    Log.msg (Log.val "Domain registration audit log")
      . Log.field "url" (LT.encodeUtf8 $ toLazyText url)
      . Log.field "old_value" (maybe "null" Aeson.encode mBefore)
      . Log.field "new_value" (maybe "null" Aeson.encode mAfter)
  mConfig <- inputs emailConfig
  for_ mConfig $ \config -> do
    let mail = mkAuditMail (config.auditEmailSender) (config.auditEmailRecipient) subject auditLog
    sendMail mail

-- More info on the behavioral implications of domain registration records:
-- https://wearezeta.atlassian.net/wiki/spaces/ENGINEERIN/pages/1570832467/Email+domain+registration+and+configuration#Configuration-values
emailToDomainRegistration ::
  forall r.
  ( Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member TinyLog r
  ) =>
  EmailAddress ->
  Sem r (Maybe DomainRegistration)
emailToDomainRegistration email = case mkDomain $ T.decodeUtf8 $ Email.domainPart email of
  Right dom -> tryGetDomainRegistrationImpl dom
  Left msg ->
    -- The EmailAddress parser and servant *should* make this impossible, but they use
    -- different parsers, one of us is ours and may change any time, so who knows?
    throw . EnterpriseLoginSubsystemGuardFailed $ InvalidDomain msg

guardEmailDomainRegistrationTeamInvitationImpl ::
  forall r.
  ( Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member TinyLog r
  ) =>
  InvitationFlow ->
  TeamId ->
  EmailAddress ->
  Sem r ()
guardEmailDomainRegistrationTeamInvitationImpl invitationFlow tid email = do
  mReg <- emailToDomainRegistration email
  for_ mReg $ \reg -> do
    -- fail if domain-redirect is set to no-registration, or
    case reg.domainRedirect of
      None -> ok
      Locked -> ok
      SSO _ -> ok
      Backend _ -> ok
      NoRegistration -> case invitationFlow of
        ExistingUser -> nope DomRedirSetToNoRegistration
        NewUser -> ok -- https://wearezeta.atlassian.net/wiki/spaces/ENGINEERIN/pages/1587118249/Use+case+initiate+invitation+flow?focusedCommentId=1672839248
      PreAuthorized -> ok
    -- team-invitation is set to not-allowed or team:{team id} for any team ID that is not
    -- the team of the inviter
    case reg.teamInvite of
      Allowed -> ok
      NotAllowed -> nope TeamInviteSetToNotAllowed
      Team allowedTid ->
        if allowedTid == tid
          then ok
          else nope TeamInviteRestrictedToOtherTeam
  where
    ok = pure ()
    nope = throw . EnterpriseLoginSubsystemGuardFailed

guardEmailDomainRegistrationRegisterImpl ::
  forall r.
  ( Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member TinyLog r
  ) =>
  EmailAddress ->
  Sem r ()
guardEmailDomainRegistrationRegisterImpl email = do
  mReg <- emailToDomainRegistration email
  for_ mReg $ \reg -> do
    case reg.domainRedirect of
      None -> ok
      Locked -> ok
      SSO _ -> nope DomRedirSetToSSO
      Backend _ -> nope DomRedirSetToBackend
      NoRegistration -> nope DomRedirSetToNoRegistration
      PreAuthorized -> ok
  where
    ok = pure ()
    nope = throw . EnterpriseLoginSubsystemGuardFailed

requestDomainVerificationTokenImpl ::
  ( Member Random r,
    Member (Input Endpoint) r,
    Member (Error ParseException) r,
    Member Rpc r
  ) =>
  Maybe DomainVerificationAuthToken ->
  Domain ->
  Sem r DomainVerificationTokenResponse
requestDomainVerificationTokenImpl mAuthToken domain = do
  authToken <- maybe generateAuthToken pure mAuthToken
  dnsToken <- getDomainVerificationToken domain (serializeDomainVerificationAuthToken authToken)
  pure
    DomainVerificationTokenResponse
      { authToken = Just authToken,
        dnsToken = dnsToken
      }

requestDomainVerificationTeamTokenImpl ::
  forall r.
  ( Member (Input Endpoint) r,
    Member TinyLog r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member (Error ParseException) r,
    Member UserSubsystem r,
    Member GalleyAPIAccess r,
    Member DomainRegistrationStore r,
    Member Rpc r
  ) =>
  Local UserId ->
  Domain ->
  Sem r DomainVerificationTokenResponse
requestDomainVerificationTeamTokenImpl lusr domain = do
  (tid, _mDomReg) <- guardTeamAdminAccess lusr domain
  let authToken = idToText tid
  dnsToken <- getDomainVerificationToken domain authToken
  pure
    DomainVerificationTokenResponse
      { authToken = Nothing,
        dnsToken = dnsToken
      }

updateDomainRedirectImpl ::
  ( Member (Error EnterpriseLoginSubsystemError) r,
    Member (Error ParseException) r,
    Member TinyLog r,
    Member DomainRegistrationStore r,
    Member (Input Endpoint) r,
    Member (Input EnterpriseLoginSubsystemConfig) r,
    Member EmailSending r,
    Member Rpc r
  ) =>
  DomainVerificationAuthToken ->
  Domain ->
  DomainRedirectConfig ->
  Sem r ()
updateDomainRedirectImpl authToken domain config = do
  mbDomainReg <- tryGetDomainRegistrationImpl domain
  update <-
    maybe
      (throw EnterpriseLoginSubsystemOperationForbidden)
      pure
      $ mbDomainReg >>= computeUpdate
  verifyDNSRecord domain (serializeDomainVerificationAuthToken authToken)
  updateDomainRegistrationImpl domain update
  where
    computeUpdate reg = case (config, reg.domainRedirect) of
      (DomainRedirectConfigRemove, NoRegistration) ->
        Just $ DomainRegistrationUpdate PreAuthorized reg.teamInvite
      (DomainRedirectConfigRemove, Backend _) ->
        Just $ DomainRegistrationUpdate PreAuthorized reg.teamInvite
      (DomainRedirectConfigBackend url, PreAuthorized) ->
        Just $ DomainRegistrationUpdate (Backend url) NotAllowed
      (DomainRedirectConfigNoRegistration, PreAuthorized) ->
        Just $ DomainRegistrationUpdate NoRegistration reg.teamInvite
      _ -> Nothing

updateTeamInviteImpl ::
  forall r.
  ( Member (Error EnterpriseLoginSubsystemError) r,
    Member (Error ParseException) r,
    Member (Input Endpoint) r,
    Member (Input EnterpriseLoginSubsystemConfig) r,
    Member DomainRegistrationStore r,
    Member EmailSending r,
    Member GalleyAPIAccess r,
    Member SparAPIAccess r,
    Member Rpc r,
    Member TinyLog r,
    Member UserSubsystem r
  ) =>
  Local UserId ->
  Domain ->
  TeamInviteConfig ->
  Sem r ()
updateTeamInviteImpl luid domain config = do
  (tid, mbDomainReg) <- guardTeamAdminAccess luid domain
  verifyDNSRecord domain (idToText tid)
  update <- validateUpdate tid mbDomainReg config
  updateDomainRegistrationImpl domain update
  where
    validateUpdate :: TeamId -> Maybe DomainRegistration -> TeamInviteConfig -> Sem r DomainRegistrationUpdate
    validateUpdate tid mDomReg conf = do
      let domReg = fromMaybe defDomReg mDomReg
      when (domReg.domainRedirect == Locked) $
        throw EnterpriseLoginSubsystemOperationForbidden
      when (isJust $ domReg.domainRedirect ^? _Backend) $
        throw EnterpriseLoginSubsystemOperationForbidden
      case conf.teamInvite of
        Team tidConfig | tidConfig /= tid -> throw EnterpriseLoginSubsystemAuthFailure
        validTeamInvite -> case conf.code of
          Just idpId -> do
            validateIdPId tid idpId
            pure $ DomainRegistrationUpdate (SSO idpId) validTeamInvite
          Nothing -> pure $ DomainRegistrationUpdate domReg.domainRedirect validTeamInvite

    validateIdPId ::
      TeamId ->
      SAML.IdPId ->
      Sem r ()
    validateIdPId tid idp = do
      idps <- getIdentityProviders tid
      unless (idp `elem` idps.providers ^.. traverse . SAML.idpId) $
        throw EnterpriseLoginSubsystemOperationForbidden

    defDomReg :: DomainRegistration
    defDomReg = DomainRegistration domain def def Nothing

guardTeamAdminAccess ::
  forall r.
  ( Member TinyLog r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member UserSubsystem r,
    Member GalleyAPIAccess r,
    Member DomainRegistrationStore r
  ) =>
  Local UserId ->
  Domain ->
  Sem r (TeamId, Maybe DomainRegistration)
guardTeamAdminAccess luid domain = do
  profile <- getSelfProfile luid >>= note EnterpriseLoginSubsystemAuthFailure
  tid <- note EnterpriseLoginSubsystemAuthFailure profile.selfUser.userTeam
  teamMember <-
    getTeamMember (tUnqualified luid) tid
      >>= note EnterpriseLoginSubsystemAuthFailure
  validatePaymentStatus tid
  unless (isAdminOrOwner (teamMember ^. permissions)) $
    throw EnterpriseLoginSubsystemAuthFailure
  mbDomainReg <- tryGetDomainRegistrationImpl domain
  pure (tid, mbDomainReg)
  where
    validatePaymentStatus :: TeamId -> Sem r ()
    validatePaymentStatus tid = do
      -- FUTUREWORK: we need a dedicated feature flag for domain registration that is managed by ibis
      -- If the team is paying, conference calling will always be enabled
      feature <- getFeatureConfigForTeam @_ @ConferenceCallingConfig tid
      when (feature.status /= FeatureStatusEnabled) $
        throw EnterpriseLoginSubsystemPaymentRequired

getDomainRegistrationPublicImpl ::
  ( Member UserKeyStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member DomainRegistrationStore r,
    Member TinyLog r
  ) =>
  GetDomainRegistrationRequest ->
  Sem r DomainRedirect
getDomainRegistrationPublicImpl (GetDomainRegistrationRequest email) = do
  -- check if the email belongs to a registered user
  mUser <- lookupKey (mkEmailKey email)
  case mUser of
    Nothing -> do
      domain <-
        either
          (const (throw EnterpriseLoginSubsystemInvalidDomain))
          pure
          $ mkDomain (Text.decodeUtf8 (domainPart email))
      mReg <- tryGetDomainRegistrationImpl domain
      pure $ maybe None (.domainRedirect) mReg
    Just _ -> pure None

generateAuthToken :: (Member Random r) => Sem r DomainVerificationAuthToken
generateAuthToken = DomainVerificationAuthToken <$> Wire.Sem.Random.bytes 32
