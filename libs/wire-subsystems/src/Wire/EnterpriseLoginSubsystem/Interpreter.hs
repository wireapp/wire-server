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
import Data.Domain
import Data.Id
import Data.Qualified
import Data.Text.Encoding qualified as Text
import Data.Text.Internal.Builder (fromLazyText, fromText, toLazyText)
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Encoding as LT
import Imports hiding (lookup)
import Network.HTTP.Types.Method
import Network.Mail.Mime (Address (Address), Mail (mailHeaders, mailParts, mailTo), emptyMail, plainPart)
import Polysemy
import Polysemy.Error (Error, note, throw)
import Polysemy.Error qualified as Error
import Polysemy.Input (Input, input, inputs, runInputConst, runInputSem)
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
import SAML2.WebSSO qualified as SAML
import System.Logger.Message qualified as Log
import Util.Options
import Wire.API.EnterpriseLogin
import Wire.API.Routes.Public.Brig.DomainVerification
import Wire.API.Team.Feature
import Wire.API.Team.Member
import Wire.API.User hiding (NewUser)
import Wire.API.User.IdentityProvider (providers)
import Wire.DomainRegistrationStore
import Wire.DomainVerificationChallengeStore
  ( DomainVerificationChallengeStore,
    mkStoredDomainVerificationChallenge,
  )
import Wire.DomainVerificationChallengeStore qualified as Challenge
import Wire.EmailSending (EmailSending, sendMail)
import Wire.EnterpriseLoginSubsystem
import Wire.EnterpriseLoginSubsystem.Error
import Wire.GalleyAPIAccess
import Wire.ParseException
import Wire.Rpc
import Wire.Sem.Random as Random
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
    Member DomainVerificationChallengeStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member (Error ParseException) r,
    Member GalleyAPIAccess r,
    Member SparAPIAccess r,
    Member TinyLog r,
    Member EmailSending r,
    Member Random r,
    Member Rpc r,
    Member UserKeyStore r,
    Member UserSubsystem r,
    Member (Input (Local ())) r
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
    Member DomainVerificationChallengeStore r,
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
    Member UserSubsystem r,
    Member (Input (Local ())) r
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
    UpdateDomainRedirect mAuthToken domain config ->
      runInputSem (wireServerEnterpriseEndpoint <$> input) $
        updateDomainRedirectImpl mAuthToken domain config
    UpdateTeamInvite lusr domain config ->
      runInputSem (wireServerEnterpriseEndpoint <$> input) $
        updateTeamInviteImpl lusr domain config
    GetDomainRegistrationPublic req ->
      getDomainRegistrationPublicImpl req
    CreateDomainVerificationChallenge domain ->
      runInputSem (wireServerEnterpriseEndpoint <$> input) $
        createDomainVerificationChallengeImpl domain
    VerifyChallenge mLusr domain challengeId challengeToken ->
      runInputSem (wireServerEnterpriseEndpoint <$> input) $
        verifyChallengeImpl mLusr domain challengeId challengeToken
    AuthorizeTeam lusr domain ownershipToken ->
      runInputSem (wireServerEnterpriseEndpoint <$> input) $
        authorizeTeamImpl lusr domain ownershipToken
    GetRegisteredDomains lusr tid ->
      runInputSem (wireServerEnterpriseEndpoint <$> input) $
        getRegisteredDomainsImpl lusr tid
    DeleteTeamDomain lusr tid domain -> deleteTeamDomainImpl lusr tid domain

deleteTeamDomainImpl ::
  ( Member (Error EnterpriseLoginSubsystemError) r,
    Member TinyLog r,
    Member UserSubsystem r,
    Member GalleyAPIAccess r,
    Member DomainRegistrationStore r
  ) =>
  Local UserId ->
  TeamId ->
  Domain ->
  Sem r ()
deleteTeamDomainImpl lusr tid domain = do
  void $ guardTeamAdminAccessWithTeamIdCheck (Just tid) lusr
  domainReg <- lookup domain >>= note EnterpriseLoginSubsystemErrorNotFound
  unless (domainReg.authorizedTeam == Just tid) $
    throw EnterpriseLoginSubsystemOperationForbidden
  delete domain

getRegisteredDomainsImpl ::
  ( Member (Error EnterpriseLoginSubsystemError) r,
    Member UserSubsystem r,
    Member GalleyAPIAccess r,
    Member (Log.Logger (Log.Msg -> Log.Msg)) r,
    Member DomainRegistrationStore r
  ) =>
  Local UserId ->
  TeamId ->
  Sem r RegisteredDomains
getRegisteredDomainsImpl lusr tid = do
  void $ guardTeamAdminAccessWithTeamIdCheck (Just tid) lusr
  domains <- mkDomainRegistrationResponse <$$> lookupByTeam tid
  pure $ RegisteredDomains domains

authorizeTeamImpl ::
  ( Member TinyLog r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member UserSubsystem r,
    Member GalleyAPIAccess r,
    Member DomainRegistrationStore r
  ) =>
  Local UserId ->
  Domain ->
  DomainOwnershipToken ->
  Sem r ()
authorizeTeamImpl lusr domain (DomainOwnershipToken token) = do
  tid <- guardTeamAdminAccess lusr
  mDomainReg <- lookup domain
  domainReg <- checkDomainOwnership mDomainReg token
  when (domainReg.domainRedirect == Locked) $
    throw EnterpriseLoginSubsystemOperationForbidden
  upsert domainReg {authorizedTeam = Just tid}

checkDomainOwnership :: (Member (Error EnterpriseLoginSubsystemError) r) => Maybe DomainRegistration -> Token -> Sem r DomainRegistration
checkDomainOwnership mDomainReg ownershipToken = do
  domainReg <- note EnterpriseLoginSubsystemAuthFailure mDomainReg
  authTokenHash <- note EnterpriseLoginSubsystemAuthFailure domainReg.authTokenHash
  unless (hashToken ownershipToken == authTokenHash) $
    throw EnterpriseLoginSubsystemAuthFailure
  pure domainReg

createDomainVerificationChallengeImpl ::
  ( Member Random r,
    Member DomainVerificationChallengeStore r,
    Member (Error ParseException) r,
    Member (Input Endpoint) r,
    Member Rpc r,
    Member TinyLog r,
    Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r
  ) =>
  Domain ->
  Sem r DomainVerificationChallenge
createDomainVerificationChallengeImpl domain = do
  dr <- lookup domain
  when (((.domainRedirect) <$> dr) == Just Locked) $
    throw EnterpriseLoginSubsystemOperationForbidden
  challengeId <- Id <$> Random.uuid
  token <- Token <$> Random.bytes 32
  dnsVerificationToken <- newDnsVerificationToken
  let challenge =
        DomainVerificationChallenge
          { challengeId,
            token,
            dnsVerificationToken
          }
  Challenge.insert (mkStoredDomainVerificationChallenge domain challenge)
  pure challenge

verifyChallengeImpl ::
  ( Member DomainVerificationChallengeStore r,
    Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member (Error ParseException) r,
    Member (Input Endpoint) r,
    Member Random r,
    Member Rpc r,
    Member TinyLog r,
    Member UserSubsystem r,
    Member GalleyAPIAccess r
  ) =>
  Maybe (Local UserId) ->
  Domain ->
  ChallengeId ->
  Token ->
  Sem r Token
verifyChallengeImpl mLusr domain challengeId challengeToken = do
  mCurrentEntry <- lookup domain
  when (((.domainRedirect) <$> mCurrentEntry) == Just Locked) $
    throw EnterpriseLoginSubsystemOperationForbidden
  case mLusr of
    Just lusr -> void $ guardTeamAdminAccess lusr
    Nothing -> unless (domainRegistrationForAnonymousAllowed mCurrentEntry) $ throw EnterpriseLoginSubsystemOperationForbidden
  challenge <- Challenge.lookup challengeId >>= note EnterpriseLoginSubsystemChallengeNotFound
  unless
    ( challenge.challengeTokenHash == hashToken challengeToken
        && challenge.domain == domain
    )
    $ do
      throw EnterpriseLoginSubsystemAuthFailure
  verifyDNSRecord domain challenge.dnsVerificationToken
  authToken <- Token <$> Random.bytes 32
  let domReg = fromMaybe (mkDomainRegistration domain) mCurrentEntry
  upsert $
    domReg
      { authTokenHash = Just $ hashToken authToken,
        dnsVerificationToken = Just challenge.dnsVerificationToken
      }
  Challenge.delete challengeId
  pure authToken
  where
    domainRegistrationForAnonymousAllowed :: Maybe DomainRegistration -> Bool
    domainRegistrationForAnonymousAllowed mDr = case (.domainRedirect) <$> mDr of
      Nothing -> False
      Just Locked -> False
      Just PreAuthorized -> True
      -- once the domain was registered before (meaning an entry exists other than Locked or Preauthorized)
      -- there is no need to preauthorize it again before verifying the DNS record
      Just _ -> True

deleteDomainImpl ::
  ( Member DomainRegistrationStore r,
    Member TinyLog r,
    Member (Input EnterpriseLoginSubsystemConfig) r,
    Member EmailSending r
  ) =>
  Domain ->
  Sem r ()
deleteDomainImpl domain = do
  mOld <- lookup domain
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
  old <- lookupOrThrow domain
  let new = old {domainRedirect = None} :: DomainRegistration
  case old.domainRedirect of
    PreAuthorized -> audit old new *> upsert new
    Backend _ _ -> audit old new *> upsert new
    NoRegistration -> audit old new *> upsert new
    None -> pure ()
    Locked -> throw EnterpriseLoginSubsystemOperationForbidden
    SSO _ -> throw EnterpriseLoginSubsystemOperationForbidden
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
  mOld <- lookup domain
  let old = fromMaybe (mkDomainRegistration domain) mOld
      new =
        old {teamInvite = update.teamInvite, domainRedirect = update.domainRedirect} :: DomainRegistration
  audit mOld new *> upsert new
  where
    audit :: Maybe DomainRegistration -> DomainRegistration -> Sem r ()
    audit mOld new = sendAuditMail url "Domain registration updated" mOld (Just new)

    url :: Builder
    url =
      "PUT /i/domain-registration/"
        <> fromText (domainText domain)

lockDomainImpl ::
  forall r.
  ( Member DomainRegistrationStore r,
    Member TinyLog r,
    Member (Input EnterpriseLoginSubsystemConfig) r,
    Member EmailSending r
  ) =>
  Domain ->
  Sem r ()
lockDomainImpl domain = do
  mOld <- lookup domain
  let new = (mkDomainRegistration domain) {domainRedirect = Locked} :: DomainRegistration
  audit mOld new *> upsert new
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
  old <- lookupOrThrow domain
  let new = old {domainRedirect = None} :: DomainRegistration
  case old.domainRedirect of
    Locked -> audit old new *> upsert new
    _ -> throw EnterpriseLoginSubsystemOperationForbidden
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
  mOld <- lookup domain
  let old = fromMaybe (mkDomainRegistration domain) mOld
      new = old {domainRedirect = PreAuthorized} :: DomainRegistration
  case old.domainRedirect of
    PreAuthorized -> pure ()
    None -> audit mOld new *> upsert new
    _ -> throw $ EnterpriseLoginSubsystemOperationForbidden
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
    Member TinyLog r
  ) =>
  Domain ->
  Sem r (Maybe DomainRegistrationResponse)
getDomainRegistrationImpl domain = mkDomainRegistrationResponse <$$> lookup domain

lookupOrThrow ::
  ( Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member TinyLog r
  ) =>
  Domain ->
  Sem r DomainRegistration
lookupOrThrow = lookup >=> Error.note EnterpriseLoginSubsystemErrorNotFound

newDnsVerificationToken ::
  ( Member (Error ParseException) r,
    Member (Input Endpoint) r,
    Member Rpc r
  ) =>
  Sem r DnsVerificationToken
newDnsVerificationToken =
  decodeBodyOrThrow
    =<< enterpriseRequest
      (method POST . paths ["i", "create-verification-token"] . expect2xx)

verifyDNSRecord ::
  ( Member (Error ParseException) r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member (Input Endpoint) r,
    Member Rpc r
  ) =>
  Domain ->
  DnsVerificationToken ->
  Sem r ()
verifyDNSRecord domain dnsToken = do
  verified <-
    decodeBodyOrThrow
      =<< enterpriseRequest
        ( method POST
            . paths
              [ "i",
                "verify-domain-token",
                toByteString' domain,
                toByteString' dnsToken
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

validate :: (Member (Error EnterpriseLoginSubsystemError) r) => DomainRegistrationUpdate -> Sem r ()
validate dr = do
  case dr.domainRedirect of
    Locked -> when (dr.teamInvite /= Allowed) $ throw EnterpriseLoginSubsystemOperationForbidden
    Backend _ _ -> when (dr.teamInvite /= NotAllowed) $ throw EnterpriseLoginSubsystemOperationForbidden
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
  let encodeDomainRegistrationPretty =
        maybe
          "null"
          (Aeson.encodePretty . mkDomainRegistrationResponse)
  let encodeDomainRegistration =
        maybe
          "null"
          (Aeson.encode . mkDomainRegistrationResponse)
  let auditLog :: LText =
        toLazyText $
          url
            <> " called;\nOld value:\n"
            <> fromLazyText
              ( LT.decodeUtf8 (encodeDomainRegistrationPretty mBefore)
              )
            <> "\nNew value:\n"
            <> fromLazyText
              ( LT.decodeUtf8
                  ( encodeDomainRegistrationPretty
                      mAfter
                  )
              )
  Log.info $
    Log.msg (Log.val "Domain registration audit log")
      . Log.field "url" (LT.encodeUtf8 $ toLazyText url)
      . Log.field "old_value" (encodeDomainRegistration mBefore)
      . Log.field "new_value" (encodeDomainRegistration mAfter)
  mConfig <- inputs emailConfig
  for_ mConfig $ \config -> do
    let mail = mkAuditMail (config.auditEmailSender) (config.auditEmailRecipient) subject auditLog
    sendMail mail

updateDomainRedirectImpl ::
  ( Member (Error EnterpriseLoginSubsystemError) r,
    Member TinyLog r,
    Member DomainRegistrationStore r,
    Member (Input EnterpriseLoginSubsystemConfig) r,
    Member EmailSending r
  ) =>
  Token ->
  Domain ->
  DomainRedirectConfig a ->
  Sem r ()
updateDomainRedirectImpl token domain config = do
  mDomainReg <- lookup domain
  domainReg <- checkDomainOwnership mDomainReg token
  unless (isAllowed domainReg.domainRedirect) $
    throw EnterpriseLoginSubsystemOperationForbidden
  updateDomainRegistrationImpl domain $ computeUpdate domainReg
  where
    computeUpdate reg = case config of
      DomainRedirectConfigRemove ->
        DomainRegistrationUpdate PreAuthorized reg.teamInvite
      DomainRedirectConfigBackend url webappUrl ->
        DomainRegistrationUpdate (Backend url webappUrl) NotAllowed
      DomainRedirectConfigNoRegistration ->
        DomainRegistrationUpdate NoRegistration reg.teamInvite

    isAllowed = \case
      PreAuthorized -> True
      Backend _ _ -> True
      NoRegistration -> True
      _ -> False

updateTeamInviteImpl ::
  forall r.
  ( Member (Error EnterpriseLoginSubsystemError) r,
    Member (Input EnterpriseLoginSubsystemConfig) r,
    Member DomainRegistrationStore r,
    Member EmailSending r,
    Member GalleyAPIAccess r,
    Member SparAPIAccess r,
    Member TinyLog r,
    Member UserSubsystem r
  ) =>
  Local UserId ->
  Domain ->
  TeamInviteConfig ->
  Sem r ()
updateTeamInviteImpl luid domain config = do
  tid <- guardTeamAdminAccess luid
  mbDomainReg <- lookup domain
  domainReg <- note EnterpriseLoginSubsystemOperationForbidden mbDomainReg
  unless (domainReg.authorizedTeam == Just tid) $
    throw EnterpriseLoginSubsystemOperationForbidden
  update <- validateUpdate tid domainReg config
  updateDomainRegistrationImpl domain update
  where
    validateUpdate :: TeamId -> DomainRegistration -> TeamInviteConfig -> Sem r DomainRegistrationUpdate
    validateUpdate tid domReg conf = do
      when (domReg.domainRedirect == Locked) $
        throw EnterpriseLoginSubsystemOperationForbidden
      when (isJust $ domReg.domainRedirect ^? _Backend) $
        throw EnterpriseLoginSubsystemOperationForbidden
      case conf.teamInvite of
        Team tidConfig | tidConfig /= tid -> throw EnterpriseLoginSubsystemOperationForbidden
        validTeamInvite -> case conf.domainRedirect of
          Just (TeamSso idpId) -> do
            validateIdPId tid idpId
            pure $ DomainRegistrationUpdate (SSO idpId) validTeamInvite
          Just TeamNone -> pure $ DomainRegistrationUpdate None validTeamInvite
          Just TeamNoRegistration -> pure $ DomainRegistrationUpdate NoRegistration validTeamInvite
          Nothing -> pure $ DomainRegistrationUpdate domReg.domainRedirect validTeamInvite

    validateIdPId ::
      TeamId ->
      SAML.IdPId ->
      Sem r ()
    validateIdPId tid idp = do
      idps <- getIdentityProviders tid
      unless (idp `elem` idps.providers ^.. traverse . SAML.idpId) $
        throw EnterpriseLoginSubsystemOperationForbidden

guardTeamAdminAccess ::
  forall r.
  ( Member (Error EnterpriseLoginSubsystemError) r,
    Member UserSubsystem r,
    Member GalleyAPIAccess r
  ) =>
  Local UserId ->
  Sem r TeamId
guardTeamAdminAccess = guardTeamAdminAccessWithTeamIdCheck Nothing

guardTeamAdminAccessWithTeamIdCheck ::
  forall r.
  ( Member (Error EnterpriseLoginSubsystemError) r,
    Member UserSubsystem r,
    Member GalleyAPIAccess r
  ) =>
  Maybe TeamId ->
  Local UserId ->
  Sem r TeamId
guardTeamAdminAccessWithTeamIdCheck mExpectedTeam luid = do
  profile <- getSelfProfile luid >>= note EnterpriseLoginSubsystemOperationForbidden
  tid <- note EnterpriseLoginSubsystemOperationForbidden profile.selfUser.userTeam
  when (any (/= tid) mExpectedTeam) $
    throw EnterpriseLoginSubsystemOperationForbidden
  teamMember <-
    getTeamMember (tUnqualified luid) tid
      >>= note EnterpriseLoginSubsystemOperationForbidden
  validatePaymentStatus tid
  unless (isAdminOrOwner (teamMember ^. permissions)) $
    throw EnterpriseLoginSubsystemOperationForbidden
  pure tid
  where
    validatePaymentStatus :: TeamId -> Sem r ()
    validatePaymentStatus tid = do
      feature <- getFeatureConfigForTeam @_ @DomainRegistrationConfig tid
      when (feature.status /= FeatureStatusEnabled) $
        throw EnterpriseLoginSubsystemPaymentRequired

getDomainRegistrationPublicImpl ::
  ( Member UserKeyStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member DomainRegistrationStore r,
    Member TinyLog r,
    Member UserSubsystem r,
    Member (Input (Local ())) r
  ) =>
  GetDomainRegistrationRequest ->
  Sem r DomainRedirectResponse
getDomainRegistrationPublicImpl (GetDomainRegistrationRequest email) = do
  -- check if the email belongs to a registered user
  mUser <- do
    mUid <- lookupKey (mkEmailKey email)
    mLuid <- for mUid (\uid -> flip qualifyAs uid <$> input)
    maybe (pure Nothing) (fmap (fmap selfUser) . getSelfProfile) mLuid

  domain <-
    either
      (const (throw EnterpriseLoginSubsystemInvalidDomain))
      pure
      $ mkDomain (Text.decodeUtf8 (domainPart email))
  mReg <- getDomainRegistrationImpl domain

  pure $ case (mUser, maybe None (.domainRedirect) mReg) of
    (Just _, Backend _ _) -> DomainRedirectResponse True NoRegistration
    (Just user, SSO _)
      | not (isSSOAccountFromTeam (mReg >>= (.authorizedTeam)) user) ->
          DomainRedirectResponse False NoRegistration
    (_, dr) -> DomainRedirectResponse False dr
  where
    isSSOAccountFromTeam :: Maybe TeamId -> User -> Bool
    isSSOAccountFromTeam Nothing _ = False
    isSSOAccountFromTeam (Just tid) user = any isSSOIdentity user.userIdentity && user.userTeam == Just tid
