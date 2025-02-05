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
    VerifyChallenge domain challengeId challengeToken ->
      runInputSem (wireServerEnterpriseEndpoint <$> input) $
        verifyChallengeImpl domain challengeId challengeToken
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
  case domainReg.settings of
    Just (DomainForLocalTeam tid' _) | tid' == tid -> pure ()
    _ -> throw EnterpriseLoginSubsystemOperationForbidden
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
  newSettings <- case domainReg.settings of
    Just DomainLocked -> throw EnterpriseLoginSubsystemOperationForbidden
    Just (DomainForLocalTeam _oldTid (Just idpid)) -> pure (DomainForLocalTeam tid (Just idpid)) -- TODO: really?  https://wearezeta.atlassian.net/wiki/spaces/ENGINEERIN/pages/1690501212/Use+case+Set+or+delete+the+domain+configuration+for+a+team?focusedCommentId=1709047823
    _ -> pure (DomainForLocalTeam tid Nothing)
  upsert domainReg {settings = Just newSettings}

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
    Member Rpc r
  ) =>
  Domain ->
  Sem r DomainVerificationChallenge
createDomainVerificationChallengeImpl domain = do
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
    Member TinyLog r
  ) =>
  Domain ->
  ChallengeId ->
  Token ->
  Sem r Token
verifyChallengeImpl domain challengeId challengeToken = do
  challenge <- Challenge.lookup challengeId >>= note EnterpriseLoginSubsystemChallengeNotFound
  unless
    ( challenge.challengeTokenHash == hashToken challengeToken
        && challenge.domain == domain
    )
    $ do
      throw EnterpriseLoginSubsystemAuthFailure
  verifyDNSRecord domain challenge.dnsVerificationToken
  authToken <- Token <$> Random.bytes 32
  mOld <- lookup domain
  let old = fromMaybe (def domain) mOld
  upsert $
    old
      { authTokenHash = Just $ hashToken authToken,
        dnsVerificationToken = Just challenge.dnsVerificationToken
      }
  Challenge.delete challengeId
  pure authToken

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
  let new = old {settings = Nothing} :: DomainRegistration
  case old.settings of
    Nothing -> pure ()
    Just DomainLocked -> throw EnterpriseLoginSubsystemOperationForbidden
    Just DomainPreAuthorized -> audit old new *> upsert new
    Just DomainNoRegistration -> audit old new *> upsert new
    Just (DomainForBackend _) -> audit old new *> upsert new
    Just (DomainForLocalTeam _ _) -> throw EnterpriseLoginSubsystemOperationForbidden
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
  let old = fromMaybe (def domain) mOld
  new <-
    let err _ = throw $ EnterpriseLoginSubsystemInvalidDomainUpdate (show (domain, update))
     in either err pure (domainRegistrationFromUpdate old update)
  audit mOld new *> upsert new
  where
    audit :: Maybe DomainRegistration -> DomainRegistration -> Sem r ()
    audit mOld new = sendAuditMail url "Domain registration updated" mOld (Just new)

    url :: Builder
    url =
      "PUT /i/domain-registration/"
        <> fromText (domainText domain)

-- TODO: this changes behavior in this PR: Locked overwrites all other data stored in
-- DomainRegistrationSettings, like authorized team or sso code.  is this what we want?  (i
-- think it is and we do!)
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
  let new = (def domain) {settings = Just DomainLocked} :: DomainRegistration
  audit mOld new *> upsert new
  where
    url :: Builder
    url =
      "POST /i/domain-registration/"
        <> fromText (domainText domain)
        <> "/lock"

    audit :: Maybe DomainRegistration -> DomainRegistration -> Sem r ()
    audit old new = sendAuditMail url "Domain locked" old (Just new)

-- TODO: overwrites all previous settings
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
  let new = old {settings = Nothing} :: DomainRegistration
  case old.settings of
    Just DomainLocked -> audit old new *> upsert new
    _ -> throw EnterpriseLoginSubsystemOperationForbidden -- TODO: why not do nothing?
  where
    url :: Builder
    url =
      "POST /i/domain-registration/"
        <> fromText (domainText domain)
        <> "/unlock"

    audit :: DomainRegistration -> DomainRegistration -> Sem r ()
    audit old new = sendAuditMail url "Domain locked" (Just old) (Just new)

-- TODO: overwrites all previous settings
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
  let old = fromMaybe (def domain) mOld
      new = old {settings = Just DomainPreAuthorized} :: DomainRegistration
  case old.settings of
    Just DomainPreAuthorized -> pure ()
    Nothing -> audit mOld new *> upsert new
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
    Backend _ -> when (dr.teamInvite /= NotAllowed) $ throw EnterpriseLoginSubsystemOperationForbidden
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
  DomainRedirectConfig ->
  Sem r ()
updateDomainRedirectImpl token domain config = do
  mDomainReg <- lookup domain
  domainReg <- checkDomainOwnership mDomainReg token
  update <-
    note EnterpriseLoginSubsystemOperationForbidden $
      computeUpdate domainReg
  updateDomainRegistrationImpl domain update
  where
    -- TODO: it is more straight-forward for 'computeUpdate' to use the DomainRegistrationRow to
    -- compute the update value.  maybe the update type should also be adjusted to fit better
    -- into 'DomainRegistration'?
    computeUpdate (domainRegistrationToRow -> reg) = case (config, reg.domainRedirect) of
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
  case domainReg.settings of
    Just (DomainForLocalTeam tid' _) | tid' == tid -> throw EnterpriseLoginSubsystemOperationForbidden
    _ -> pure ()
  update <- validateUpdate tid domainReg config
  updateDomainRegistrationImpl domain update
  where
    -- TODO: it is more straight-forward for 'computeUpdate' to use the DomainRegistrationRow to
    -- compute the update value.  maybe the update type should also be adjusted to fit better
    -- into 'DomainRegistration'?
    validateUpdate :: TeamId -> DomainRegistration -> TeamInviteConfig -> Sem r DomainRegistrationUpdate
    validateUpdate tid (domainRegistrationToRow -> domReg) conf = do
      when (domReg.domainRedirect == Locked) $
        throw EnterpriseLoginSubsystemOperationForbidden
      when (isJust $ domReg.domainRedirect ^? _Backend) $
        throw EnterpriseLoginSubsystemOperationForbidden
      case conf.teamInvite of
        Team tidConfig | tidConfig /= tid -> throw EnterpriseLoginSubsystemOperationForbidden
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
    Member TinyLog r
  ) =>
  GetDomainRegistrationRequest ->
  Sem r DomainRedirectResponse
getDomainRegistrationPublicImpl (GetDomainRegistrationRequest email) = do
  -- check if the email belongs to a registered user
  mUser <- lookupKey (mkEmailKey email)

  domain <-
    either
      (const (throw EnterpriseLoginSubsystemInvalidDomain))
      pure
      $ mkDomain (Text.decodeUtf8 (domainPart email))
  mReg <- getDomainRegistrationImpl domain

  pure $ case mUser of
    Nothing -> DomainRedirectResponse False (maybe None (.domainRedirect) mReg)
    Just _ ->
      DomainRedirectResponse (fmap (domainRedirectTag . (.domainRedirect)) mReg == Just BackendTag) None
