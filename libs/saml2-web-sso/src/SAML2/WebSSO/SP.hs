{-# LANGUAGE OverloadedStrings #-}

module SAML2.WebSSO.SP where

import Control.Lens hiding (Level)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Extra (ifM)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.List (nub, partition)
import Data.List.NonEmpty (NonEmpty)
import Data.String.Conversions
import Data.Time
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import GHC.Stack
import SAML2.Util
import SAML2.WebSSO.Config
import SAML2.WebSSO.Servant.CPP
import SAML2.WebSSO.Types
import Servant hiding (MkLink, URI (..))
import URI.ByteString

----------------------------------------------------------------------
-- class

-- | Application logic of the service provider.
type SP m = (HasConfig m, HasLogger m, HasCreateUUID m, HasNow m)

class HasLogger m where
  logger :: Level -> String -> m ()
  default logger :: (HasConfig m, MonadIO m) => Level -> String -> m ()
  logger = loggerConfIO

class HasCreateUUID m where
  createUUID :: m UUID
  default createUUID :: (MonadIO m) => m UUID
  createUUID = createUUIDIO

class HasNow m where
  getNow :: m Time
  default getNow :: (MonadIO m) => m Time
  getNow = getNowIO

type SPStore m = (SP m, SPStoreRequest AuthnRequest m, SPStoreAssertion Assertion m)

class SPStoreAssertion i m where
  storeAssertionInternal :: ID i -> Time -> m ()
  unStoreAssertion :: ID i -> m ()
  isAliveAssertion ::
    ID i ->
    -- | stored and not timed out.
    m Bool

class SPStoreRequest i m where
  storeRequest :: ID i -> Issuer -> Time -> m ()
  unStoreRequest :: ID i -> m ()
  isAliveRequest ::
    ID i ->
    -- | stored and not timed out.
    m Bool

  -- TODO: Rename this function to `getIssuer` and only return that
  getRequest :: ID i -> m (Maybe (Issuer, Time))

class (MonadError err m) => SPStoreIdP err m where
  type IdPConfigExtra m :: Type
  type IdPConfigSPId m :: Type
  storeIdPConfig :: IdPConfig (IdPConfigExtra m) -> m ()
  getIdPConfig :: IdPId -> m (IdPConfig (IdPConfigExtra m))
  getIdPConfigByIssuer :: Issuer -> IdPConfigSPId m -> m (IdPConfig (IdPConfigExtra m))
  getIdPConfigByIssuer issuer spid = getIdPConfigByIssuerOptionalSPId issuer (Just spid)
  getIdPConfigByIssuerOptionalSPId :: Issuer -> Maybe (IdPConfigSPId m) -> m (IdPConfig (IdPConfigExtra m))

-- | HTTP handling of the service provider.
class (SP m, SPStore m, SPStoreIdP err m, MonadError err m) => SPHandler err m where
  type NTCTX m :: Type
  nt :: forall x. NTCTX m -> m x -> Handler x

----------------------------------------------------------------------
-- combinators

-- | Store 'Assertion's to prevent replay attack.  'Time' argument is end of life (IDs may be
-- garbage collected after that time).  Iff assertion has already been stored and is still alive,
-- return 'False'.
storeAssertion :: (Monad m, SPStore m) => ID Assertion -> Time -> m Bool
storeAssertion item endOfLife =
  ifM
    (isAliveAssertion item)
    (pure False)
    (True <$ storeAssertionInternal item endOfLife)

loggerConfIO :: (HasConfig m, MonadIO m) => Level -> String -> m ()
loggerConfIO level msg = do
  cfgsays <- (^. cfgLogLevel) <$> getConfig
  loggerIO cfgsays level msg

loggerIO :: (MonadIO m) => Level -> Level -> String -> m ()
loggerIO cfgsays level msg =
  when (level >= cfgsays) $ liftIO $ putStrLn msg

createUUIDIO :: (MonadIO m) => m UUID
createUUIDIO = liftIO UUID.nextRandom

getNowIO :: (MonadIO m) => m Time
getNowIO = Time <$> liftIO getCurrentTime

-- | (Microsoft Active Directory likes IDs to be of the form @id<32 hex digits>@: @ID . cs . ("id"
-- <>) . filter (/= '-') . cs . UUID.toText <$> createUUID@.  Hopefully the more common form
-- produced by this function is also ok.)
createID :: (Functor m, SP m) => m (ID a)
createID = mkID . ("_" <>) . UUID.toText <$> createUUID

-- | Generate an 'AuthnRequest' value for the initiate-login response.  The 'NameIdPolicy' is
-- 'NameIDFUnspecified'.
--
-- We do not use XML encryption (which appears to have been dead for years).  We do not sign
-- 'AuthnRequest' values.  Both encryption and signatures are optional, and rarely found in the
-- wild.  Security: (1) the 'AuthnReq' 'ID' is stored, and 'Assertion's need to refer to a valid one
-- in order to get a positive 'AccessVerdict' by 'judge'.  'AuthnResponse's answering requests not
-- originating from us will be ignored.  (2) the request 'Issuer' is there to help the IdP construct
-- an 'AuthnResponse' that we will accept.  If it is changed by an attacker (either in the browser
-- or on the wire, which is weakly procted by TLS) and passed on to the legitimate IdP, there will
-- either be no access-granting 'AuthnResponse', or it will contain the wrong audience and be
-- rejected by us.  (3) The nameID policy is expected to be configured on the IdP side to not
-- support any set of name spaces that overlap (e.g. because user A has an email that is the account
-- name of user B).
createAuthnRequest :: (Monad m, SP m, SPStore m) => NominalDiffTime -> m Issuer -> m AuthnRequest
createAuthnRequest lifeExpectancySecs getIssuer = do
  _rqID <- createID
  _rqIssueInstant <- getNow
  _rqIssuer <- getIssuer
  let _rqNameIDPolicy = Just $ NameIdPolicy NameIDFUnspecified Nothing True
  storeRequest _rqID _rqIssuer (addTime lifeExpectancySecs _rqIssueInstant)
  pure AuthnRequest {..}

-- | The clock drift between IdP and us that we allow for.
--
-- FUTUREWORK: make this configurable
tolerance :: NominalDiffTime
tolerance = 60 -- seconds; must be positive

-- | First arg must be comfortably earlier than the latter: if 'tolerance' is one minute, then
-- @("4:32:14" `earlier` "4:31:15") == True@.
--
-- If this returns 'False' you should be *more* inclined to throw an error, so a good calling
-- pattern is @unless (a `earlier` b) throwSomething@, which is very different from @when (b
-- `earlier` a) throwSomething@.
earlier :: Time -> Time -> Bool
earlier early late = early < addTime tolerance late

-- | Even though this makes little sense, the standard requires to distinguish between "less"
-- and "less or equal".  For all practical purposes, you may consider @noLater == earlier@.
noLater :: Time -> Time -> Bool
noLater early late = early <= addTime tolerance late

----------------------------------------------------------------------
-- paths

getSsoURI ::
  forall m endpoint api.
  ( HasCallStack,
    Functor m,
    HasConfig m,
    IsElem endpoint api,
    HasLink endpoint,
    ToHttpApiData (MkLink endpoint)
  ) =>
  Proxy api ->
  Proxy endpoint ->
  m URI
getSsoURI proxyAPI proxyAPIAuthResp = extpath . (^. cfgSPSsoURI) <$> getConfig
  where
    extpath :: URI -> URI
    extpath = (=/ (cs . toUrlPiece $ safeLink proxyAPI proxyAPIAuthResp))

-- | 'getSsoURI' for links that have one variable path segment.
--
-- FUTUREWORK: this is only sometimes what we need.  it would be nice to have a type class with a
-- method 'getSsoURI' for arbitrary path arities.
getSsoURI' ::
  forall endpoint api a (f :: Type -> Type) t.
  ( Functor f,
    HasConfig f,
    MkLink endpoint ~ (t -> a),
    HasLink endpoint,
    ToHttpApiData a,
    IsElem endpoint api
  ) =>
  Proxy api ->
  Proxy endpoint ->
  t ->
  f URI
getSsoURI' proxyAPI proxyAPIAuthResp idpid = extpath . (^. cfgSPSsoURI) <$> getConfig
  where
    extpath :: URI -> URI
    extpath = (=/ (cs . toUrlPiece $ safeLink proxyAPI proxyAPIAuthResp idpid))

----------------------------------------------------------------------
-- compute access verdict(s)

-- | This monad collects errors in a writer, so that the reasons for access denial are as helpful as
-- possible.  It is a little like an exception monad, except you can throw several exceptions
-- without interrupting the flow, and will get a list of them at the end.
--
-- NOTE: @-XGeneralizedNewtypeDeriving@ does not help with the boilerplate instances below, since
-- this is a transformer stack and not a concrete 'Monad'.
newtype JudgeT m a = JudgeT
  {fromJudgeT :: ExceptT DeniedReason (WriterT [DeniedReason] (ReaderT JudgeCtx m)) a}

-- | Note on security: we assume that the SP has only one audience, which is defined here.  If you
-- have different sub-services running on your SP, associate a dedicated IdP with each sub-service.
-- (To be more specific, construct 'AuthnReq's to different IdPs for each sub-service.)  Secure
-- association with the service can then be guaranteed via the 'Issuer' in the signed 'Assertion'.
data JudgeCtx = JudgeCtx
  { _judgeCtxAudience :: Issuer,
    _judgeCtxResponseURI :: URI
  }
  deriving (Eq, Show)

makeLenses ''JudgeCtx

runJudgeT :: forall m. (Monad m, SP m) => JudgeCtx -> JudgeT m AccessVerdict -> m AccessVerdict
runJudgeT ctx (JudgeT em) = fmap collectErrors . (`runReaderT` ctx) . runWriterT $ runExceptT em
  where
    collectErrors :: (Either DeniedReason AccessVerdict, [DeniedReason]) -> AccessVerdict
    collectErrors (Left err, errs') = AccessDenied $ err : errs'
    collectErrors (Right _, errs@(_ : _)) = AccessDenied errs
    collectErrors (Right v, []) = v

-- the parts of the MonadError, MonadWriter interfaces we want here.
class (Functor m, Applicative m, Monad m) => MonadJudge m where
  getJudgeCtx :: m JudgeCtx
  deny :: DeniedReason -> m ()
  giveup :: DeniedReason -> m a

instance (Functor m, Applicative m, Monad m) => MonadJudge (JudgeT m) where
  getJudgeCtx = JudgeT . lift . lift $ ask
  deny = JudgeT . tell . (: [])
  giveup = JudgeT . throwError

instance (Functor m, Applicative m, Monad m) => Functor (JudgeT m) where
  fmap f = JudgeT . fmap f . fromJudgeT

instance (Functor m, Applicative m, Monad m) => Applicative (JudgeT m) where
  pure = JudgeT . pure
  (JudgeT f) <*> (JudgeT x) = JudgeT (f <*> x)

instance (Functor m, Applicative m, Monad m) => Monad (JudgeT m) where
  (JudgeT x) >>= f = JudgeT (x >>= fromJudgeT . f)

instance (Monad m, HasConfig m) => HasConfig (JudgeT m) where
  getConfig = JudgeT . lift . lift . lift $ getConfig

instance (Monad m, HasLogger m) => HasLogger (JudgeT m) where
  logger level = JudgeT . lift . lift . lift . logger level

instance (Monad m, HasCreateUUID m) => HasCreateUUID (JudgeT m) where
  createUUID = JudgeT . lift . lift . lift $ createUUID

instance (Monad m, HasNow m) => HasNow (JudgeT m) where
  getNow = JudgeT . lift . lift . lift $ getNow

instance (Monad m, SPStoreAssertion i m) => SPStoreAssertion i (JudgeT m) where
  storeAssertionInternal item = JudgeT . lift . lift . lift . storeAssertionInternal item
  unStoreAssertion = JudgeT . lift . lift . lift . unStoreAssertion
  isAliveAssertion = JudgeT . lift . lift . lift . isAliveAssertion

instance (Monad m, SPStoreRequest i m) => SPStoreRequest i (JudgeT m) where
  storeRequest item issuer = JudgeT . lift . lift . lift . storeRequest item issuer
  unStoreRequest = JudgeT . lift . lift . lift . unStoreRequest
  isAliveRequest = JudgeT . lift . lift . lift . isAliveRequest
  getRequest = JudgeT . lift . lift . lift . getRequest

-- | [3/4.1.4.2], [3/4.1.4.3]; specific to active-directory:
-- <https://docs.microsoft.com/en-us/azure/active-directory/develop/active-directory-single-sign-on-protocol-reference>
--
-- 'judge' does not consider the following parts of the 'AuthnResponse'.
--  * 'subjectID'
--  * 'scdAddress' ("If any bearer <SubjectConfirmationData> includes an Address attribute, the
--    service provider MAY check the user agent's client address against it."  [3/4.1.4.3])
--  * 'astSessionIndex'
--  * 'astSubjectLocality' ("This element is entirely advisory, since both of these fields are
--    quite easily “spoofed,” but may be useful information in some applications." [1/2.7.2.1])
--
-- 'judge' does *not* check any of the *unsigned* parts of the AuthnResponse body because...
-- those are not signed!  the standard doesn't seem to worry about that, but wire does.  this
-- affects `rspStatus`, `inRespTo`, `rspIssueInstant`, `rspDestination`.  those are inferred
-- from the *signed* information available.
judge :: (Monad m, SP m, SPStore m) => NonEmpty Assertion -> JudgeCtx -> m AccessVerdict
judge assertions ctx = runJudgeT ctx (foldJudge assertions)

foldJudge :: (HasCallStack, MonadJudge m, SP m, SPStore m) => NonEmpty Assertion -> m AccessVerdict
foldJudge (toList -> assertions) = do
  verdicts <- judge1 `mapM` assertions
  let (granteds, denieds) =
        verdicts
          & partition
            ( \case
                AccessDenied _ -> False
                AccessGranted _ -> True
            )

  pure $ case (granteds, denieds) of
    (nub -> [result@(AccessGranted _)], []) -> result
    (_, denied : _) -> denied
    (bad, _) -> AccessDenied (DeniedBadUserRefs (show bad) : (view avReasons =<< denieds))

judge1 :: (HasCallStack, MonadJudge m, SP m, SPStore m) => Assertion -> m AccessVerdict
judge1 assertion = do
  inRespTo <- either (giveup . DeniedBadInResponseTos) pure $ assertionToInResponseTo assertion
  checkInResponseTo "response" inRespTo
  verdict <- checkAssertion assertion
  unStoreRequest inRespTo
  pure verdict

-- | If this fails, we could continue ('deny'), but we stop processing ('giveup') to make DOS
-- attacks harder.
checkInResponseTo :: (SPStore m, MonadJudge m) => String -> ID AuthnRequest -> m ()
checkInResponseTo loc req = do
  ok <- isAliveRequest req
  unless ok . giveup . DeniedBadInResponseTos $ loc <> ": " <> show req

checkIsInPast :: (SP m, MonadJudge m) => (Time -> Time -> DeniedReason) -> Time -> m ()
checkIsInPast err tim = do
  now <- getNow
  unless (tim `earlier` now) . deny $ err tim now

-- | Check that the response is intended for us (based on config's finalize-login uri stored in
-- 'JudgeCtx').
checkDestination :: (HasConfig m, MonadJudge m) => (String -> String -> DeniedReason) -> URI -> m ()
checkDestination err (renderURI -> expectedByIdp) = do
  (renderURI -> expectedByUs) <- (^. judgeCtxResponseURI) <$> getJudgeCtx
  unless (expectedByUs == expectedByIdp) . deny $ err (cs expectedByUs) (cs expectedByIdp)

checkAssertion :: (SP m, SPStore m, MonadJudge m) => Assertion -> m AccessVerdict
checkAssertion ass = do
  checkIsInPast DeniedAssertionIssueInstantNotInPast (ass ^. assIssueInstant)
  storeAssertion (ass ^. assID) (ass ^. assEndOfLife) >>= \case
    True -> pure ()
    False -> deny DeniedStatusFailure
  checkConditions `mapM_` (ass ^. assConditions)
  checkSubjectConfirmations ass
  let statements = ass ^. assContents . sasStatements
  unless (any isAuthnStatement statements) (deny DeniedNoAuthnStatement)
  checkStatement `mapM_` toList statements
  pure $ AccessGranted (assertionToUserRef ass)

checkStatement :: (SP m, MonadJudge m) => Statement -> m ()
checkStatement stm =
  do
    let issued = stm ^. astAuthnInstant
        mtimeout = stm ^. astSessionNotOnOrAfter
    checkIsInPast DeniedAuthnStatementIssueInstantNotInPast issued
    forM_ mtimeout $ \endoflife -> do
      now <- getNow
      unless (now `earlier` endoflife) . deny $ DeniedAuthnStatmentExpiredAt endoflife

-- | Check all 'SubjectConfirmation's and 'Subject's in all 'Assertion'.  Deny if not at least one
-- confirmation has method "bearer".
checkSubjectConfirmations :: (SP m, SPStore m, MonadJudge m) => Assertion -> m ()
checkSubjectConfirmations assertion = do
  bearerFlags :: [HasBearerConfirmation] <- case assertion ^. assContents . sasSubject of
    Subject _ confs -> checkSubjectConfirmation assertion `mapM` confs
  unless (nub bearerFlags == [HasBearerConfirmation]) $
    deny DeniedNoBearerConfSubj

data HasBearerConfirmation = HasBearerConfirmation | NoBearerConfirmation
  deriving (Eq, Ord, Bounded, Enum)

-- | Locally check one 'SubjectConfirmation' and deny if there is a problem.  If this is a
-- confirmation of method "bearer", return 'HasBearerConfirmation'.
checkSubjectConfirmation :: (SPStore m, MonadJudge m) => Assertion -> SubjectConfirmation -> m HasBearerConfirmation
checkSubjectConfirmation ass conf = do
  let bearer =
        if (conf ^. scMethod) == SubjectConfirmationMethodBearer
          then HasBearerConfirmation
          else NoBearerConfirmation
  when (bearer == HasBearerConfirmation) $ do
    when (all (null . (^. condAudienceRestriction)) (ass ^. assConditions)) $
      deny DeniedBearerConfAssertionsWithoutAudienceRestriction
  -- (the actual validation of the audience restrictions happens in 'checkConditions'.)

  checkSubjectConfirmationData `mapM_` (conf ^. scData)
  pure bearer

checkSubjectConfirmationData ::
  (HasConfig m, SP m, SPStore m, MonadJudge m) =>
  SubjectConfirmationData ->
  m ()
checkSubjectConfirmationData confdat = do
  checkDestination DeniedBadRecipient $ confdat ^. scdRecipient
  now <- getNow
  unless (now `earlier` (confdat ^. scdNotOnOrAfter)) . deny $
    DeniedNotOnOrAfterSubjectConfirmation (confdat ^. scdNotOnOrAfter)
  case confdat ^. scdNotBefore of
    Just notbef -> unless (notbef `noLater` now) . deny $ DeniedNotBeforeSubjectConfirmation notbef
    _ -> pure ()
  -- double-check the result of the call to 'rspInResponseTo' in 'judge'' above.
  checkInResponseTo "assertion" `mapM_` (confdat ^. scdInResponseTo)

checkConditions :: forall m. (HasCallStack, MonadJudge m, SP m) => Conditions -> m ()
checkConditions (Conditions lowlimit uplimit _onetimeuse {- TODO: what checks are we missing for onetimeuse? -} audiences) = do
  now <- getNow
  case lowlimit of
    Just lim -> unless (lim `noLater` now) . deny $ DeniedNotBeforeCondition lim
    _ -> pure ()
  case uplimit of
    Just lim -> unless (now `earlier` lim) . deny $ DeniedNotOnOrAfterCondition lim
    _ -> pure ()
  Issuer us <- (^. judgeCtxAudience) <$> getJudgeCtx
  let checkAudience :: NonEmpty URI -> m ()
      checkAudience aus =
        unless (us `elem` aus) . deny $
          DeniedAudienceMismatch us aus
  checkAudience `mapM_` audiences
