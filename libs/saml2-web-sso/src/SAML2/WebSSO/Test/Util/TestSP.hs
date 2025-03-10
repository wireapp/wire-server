{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module SAML2.WebSSO.Test.Util.TestSP where

import Control.Concurrent.MVar
import Control.Exception (ErrorCall (..), throwIO)
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Crypto.Random.Types (MonadRandom (..))
import Data.EitherR
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map as Map
import Data.Maybe
import Data.Time
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Data.Void (Void)
import SAML2.WebSSO as SAML
import SAML2.WebSSO.API.Example (GetAllIdPs (..), simpleGetIdPConfigBy, simpleIsAliveID', simpleStoreID', simpleUnStoreID')
import SAML2.WebSSO.Test.Util.Types
import Servant
import System.IO
import System.IO.Silently (hCapture)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.Internal (runWaiSession)
import Text.XML.DSig as SAML
import URI.ByteString (pathL)
import URI.ByteString.QQ (uri)
import Prelude hiding (head)

-- | FUTUREWORK: we already have 'SimpleSP'; is there a good reason why we need both types?
newtype TestSP a = TestSP {runTestSP :: ReaderT CtxV (ExceptT SimpleError IO) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader CtxV, MonadError SimpleError)

instance HasConfig TestSP where
  getConfig = (^. ctxConfig) <$> (liftIO . readMVar =<< ask)

instance HasLogger TestSP

instance HasCreateUUID TestSP

instance HasNow TestSP where
  -- Make TestSP to move forward in time after each look at the clock.
  getNow = modifyCtx (\ctx -> (ctx & ctxNow %~ (1 `addTime`), ctx ^. ctxNow))

-- | These helpers are very similar to the ones in "SAML2.WebSSO.API.Example".  Exercise to the
-- reader: implement only once, use twice.  (Some hints: None of the "lens through IORef/MVar/etc"
-- libraries took off. There's http://hackage.haskell.org/package/monad-var but I haven't looked at
-- it. You might also want to read ekmett's comments at
-- https://www.reddit.com/r/haskell/comments/8gc8p0/extensible_monadic_lenses/. Don't ask me about
-- monadic lenses though, I'm really clueless.)
simpleStoreID ::
  (MonadIO m, MonadReader (MVar ctx) m) =>
  Lens' ctx (Map.Map (ID a) Time) ->
  ID a ->
  Time ->
  m ()
simpleStoreID sel item endOfLife = do
  store <- ask
  liftIO $ modifyMVar_ store (pure . (sel %~ simpleStoreID' item endOfLife))

simpleUnStoreID ::
  (MonadIO m, MonadReader (MVar ctx) m) =>
  Lens' ctx (Map.Map (ID a) Time) ->
  ID a ->
  m ()
simpleUnStoreID sel item = do
  store <- ask
  liftIO $ modifyMVar_ store (pure . (sel %~ simpleUnStoreID' item))

simpleIsAliveID ::
  (MonadIO m, MonadReader (MVar ctx) m, SP m) =>
  Lens' ctx (Map.Map (ID a) Time) ->
  ID a ->
  m Bool
simpleIsAliveID sel item = do
  now <- getNow
  store <- ask
  items <- liftIO $ readMVar store
  pure $ simpleIsAliveID' now item (items ^. sel)

instance SPStoreID AuthnRequest TestSP where
  storeID = simpleStoreID ctxRequestStore
  unStoreID = simpleUnStoreID ctxRequestStore
  isAliveID = simpleIsAliveID ctxRequestStore

instance SPStoreID Assertion TestSP where
  storeID = simpleStoreID ctxAssertionStore
  unStoreID = simpleUnStoreID ctxAssertionStore
  isAliveID = simpleIsAliveID ctxAssertionStore

instance SPStoreIdP SimpleError TestSP where
  type IdPConfigExtra TestSP = ()
  type IdPConfigSPId TestSP = Void -- FUTUREWORK: can we do better than this?
  storeIdPConfig _ = pure ()
  getIdPConfig = simpleGetIdPConfigBy readIdPs (^. idpId)
  getIdPConfigByIssuerOptionalSPId issuer _ = simpleGetIdPConfigBy readIdPs (^. idpMetadata . edIssuer) issuer

instance GetAllIdPs SimpleError TestSP where
  getAllIdPs = fmap fst . (^. ctxIdPs) <$> (ask >>= liftIO . readMVar)

instance SPHandler SimpleError TestSP where
  type NTCTX TestSP = CtxV

  nt :: forall x. CtxV -> TestSP x -> Handler x
  nt = handlerFromTestSP

readIdPs :: TestSP [IdPConfig_]
readIdPs = (fmap fst . (^. ctxIdPs) <$> (ask >>= liftIO . readMVar))

handlerFromTestSP :: CtxV -> TestSP a -> Handler a
handlerFromTestSP ctx (TestSP m) = Handler . ExceptT . fmap (fmapL toServerError) . runExceptT $ m `runReaderT` ctx

ioFromTestSP :: CtxV -> TestSP a -> IO a
ioFromTestSP ctx m = either (throwIO . ErrorCall . show) pure =<< (runExceptT . runHandler' $ handlerFromTestSP ctx m)

withapp ::
  forall (api :: Type).
  (HasServer api '[]) =>
  Proxy api ->
  ServerT api TestSP ->
  IO CtxV ->
  SpecWith (CtxV, Application) ->
  Spec
withapp proxy handler mkctx = withState (mkctx <&> \ctx -> (ctx, app ctx))
  where
    app ctx = serve proxy (hoistServer (Proxy @api) (nt @SimpleError @TestSP ctx) handler :: Server api)

capture' :: HasCallStack => IO a -> IO a
capture' action =
  hCapture [stdout, stderr] action >>= \case
    ("", out) -> pure out
    (noise, _) -> error $ show noise

captureApplication :: HasCallStack => Application -> Application
captureApplication app req cont = capture' (app req cont)

runtest :: (CtxV -> WaiSession () a) -> (CtxV, Application) -> IO a
runtest test (ctx, app) = runWaiSession (test ctx) app


runtest' :: WaiSession () a -> ((CtxV, Application) -> IO a)
runtest' action = runtest (\_ctx -> action)

mkTestCtxSimple :: MonadIO m => m CtxV
mkTestCtxSimple = liftIO $ do
  let _ctxNow = timeNow -- constant time value, see below
      _ctxConfig = fallbackConfig & cfgLogLevel .~ Fatal
      _ctxIdPs = mempty
      _ctxAssertionStore = mempty
      _ctxRequestStore = mempty
  newMVar Ctx {..}

mkTestCtxWithIdP :: MonadIO m => m CtxV
mkTestCtxWithIdP = liftIO $ do
  ctxmv <- mkTestCtxSimple
  testcfg <- makeTestIdPConfig
  liftIO $ modifyMVar_ ctxmv (pure . (ctxIdPs .~ [testcfg]))
  pure ctxmv

-- | construct a hypothetical idp configuration from sample metadata and credentials, without
-- actually registering it.
makeTestIdPConfig :: (MonadIO m, MonadRandom m) => m (IdPConfig (), SampleIdP)
makeTestIdPConfig = do
  _idpId <- IdPId <$> liftIO UUID.nextRandom
  sampleIdP@(SampleIdP _idpMetadata _ _ _) <- makeSampleIdPMetadata
  let _edIssuer = _idpMetadata ^. edIssuer
      _edRequestURI = _idpMetadata ^. edRequestURI
      _edCertAuthnResponse = _idpMetadata ^. edCertAuthnResponse
      _idpExtraInfo = ()
  pure (IdPConfig {..}, sampleIdP)

makeSampleIdPMetadata :: HasCallStack => (MonadIO m, MonadRandom m) => m SampleIdP
makeSampleIdPMetadata = do
  issuer <- makeIssuer
  requri <- do
    uuid <- UUID.toASCIIBytes <$> liftIO UUID.nextRandom
    pure $ [uri|https://requri.net/|] & pathL .~ ("/" <> uuid)
  (privcreds, creds, cert) <- SAML.mkSignCredsWithCert Nothing 96
  pure $ SampleIdP (IdPMetadata issuer requri (cert :| [])) privcreds creds cert

makeIssuer :: MonadIO m => m Issuer
makeIssuer = do
  uuid <- liftIO UUID.nextRandom
  either
    (liftIO . throwIO . ErrorCall . show)
    (pure . Issuer)
    (SAML.parseURI' ("https://issuer.net/_" <> UUID.toText uuid))

mkTestSPMetadata :: (Monad m, HasConfig m) => m SPMetadata
mkTestSPMetadata = do
  let _spID = mkID "_4b7e1488-c0c6-11e8-aef0-9fe604f9513a"
      _spValidUntil = fromTime $ addTime (60 * 60 * 24 * 365) timeNow
      _spCacheDuration = 2592000
      _spOrgName = mkXmlText "drnick"
      _spOrgDisplayName = mkXmlText "drnick"
      _spContacts = [fallbackContact]
  _spOrgURL <- (^. fromIssuer) <$> defSPIssuer
  _spResponseURL <- defResponseURI
  pure SPMetadata {..}

-- | Use this to see more output on a per-test basis.
verbose :: Ctx -> Ctx
verbose = ctxConfig . cfgLogLevel .~ Debug

timeLongAgo :: Time
timeLongAgo = unsafeReadTime "1918-04-14T09:58:58.457Z"

timeInALongTime :: Time
timeInALongTime = unsafeReadTime "2045-04-14T09:58:58.457Z"

timeNow :: Time
timeNow = unsafeReadTime "2018-03-11T17:13:13Z"

timeIn5seconds :: Time
timeIn5seconds = unsafeReadTime "2018-03-11T17:13:18Z"

timeIn10seconds :: Time
timeIn10seconds = unsafeReadTime "2018-03-11T17:13:23Z"

timeIn10minutes :: Time
timeIn10minutes = unsafeReadTime "2018-03-11T17:23:00.01Z"

timeIn20minutes :: Time
timeIn20minutes = unsafeReadTime "2018-03-11T17:33:00Z"

modifyCtx :: (HasCallStack, MonadIO m, MonadReader CtxV m) => (Ctx -> (Ctx, a)) -> m a
modifyCtx f = do
  ctx <- ask
  liftIO $ modifyMVar ctx (pure . f)

modifyCtx_ :: (HasCallStack, MonadIO m, MonadReader CtxV m) => (Ctx -> Ctx) -> m ()
modifyCtx_ = modifyCtx . ((,()) .)

-- | Run an action at a time specified relative to now.  This does NOT support hspec's 'parallel'.
timeTravel :: (HasCallStack, MonadIO m, MonadReader CtxV m) => NominalDiffTime -> m a -> m a
timeTravel distance action = do
  let mv dist_ = modifyCtx_ (ctxNow %~ (dist_ `addTime`))
  mv distance
  result <- action
  mv (- distance)
  pure result
