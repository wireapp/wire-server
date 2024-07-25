{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- Disabling for HasCallStack
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module API.Teams.LegalHold.Util where

import API.SQS
import API.Util
import Bilge hiding (accept, head, timeout, trace)
import Bilge.Assert
import Brig.Types.Test.Arbitrary ()
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.Chan
import Control.Concurrent.Timeout hiding (threadDelay)
import Control.Exception (asyncExceptionFromException)
import Control.Lens hiding ((#))
import Control.Monad.Catch
import Control.Retry
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (FromJSON, withObject, (.:))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Conversion
import Data.CallStack
import Data.Id
import Data.List.NonEmpty qualified as NonEmpty
import Data.List1 qualified as List1
import Data.Misc (PlainTextPassword6)
import Data.PEM
import Data.Streaming.Network (bindRandomPortTCP)
import Data.String.Conversions
import Data.Tagged
import Data.Text.Encoding (encodeUtf8)
import Galley.Options
import Galley.Types.Teams
import Imports
import Network.HTTP.Types.Status (status200, status400, status404)
import Network.Socket (Socket)
import Network.Socket qualified as Socket
import Network.Wai as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.Warp.Internal qualified as Warp
import Network.Wai.Handler.WarpTLS qualified as Warp
import Network.Wai.Utilities.Response qualified as Wai
import Test.QuickCheck.Instances ()
import Test.Tasty.Cannon qualified as WS
import Test.Tasty.HUnit
import Test.Tasty.Options
import Test.Tasty.Providers
import Test.Tasty.Providers.ConsoleFormat
import Test.Tasty.Runners
import TestSetup
import Wire.API.Internal.Notification (ntfPayload)
import Wire.API.Provider.Service
import Wire.API.Team.Feature qualified as Public
import Wire.API.Team.LegalHold
import Wire.API.Team.LegalHold.External
import Wire.API.User.Client
import Wire.API.UserEvent qualified as Ev

--------------------------------------------------------------------
-- setup helpers

-- | Create a new legal hold service creation request with the URL from the integration test
-- config.
newLegalHoldService :: (HasCallStack) => Warp.Port -> TestM NewLegalHoldService
newLegalHoldService lhPort = do
  config <- view (tsIConf . to provider)
  key' <- liftIO $ readServiceKey (publicKey config)
  let Just url =
        fromByteString $
          encodeUtf8 (botHost config) <> ":" <> cs (show lhPort) <> "/legalhold"
  pure
    NewLegalHoldService
      { newLegalHoldServiceUrl = url,
        newLegalHoldServiceKey = key',
        newLegalHoldServiceToken = ServiceToken "tok"
      }

-- | FUTUREWORK: reduce duplication (copied from brig/Provider.hs)
readServiceKey :: (HasCallStack, MonadIO m) => FilePath -> m ServiceKeyPEM
readServiceKey fp = liftIO $ do
  bs <- BS.readFile fp
  let Right [k] = pemParseBS bs
  pure (ServiceKeyPEM k)

withDummyTestServiceForTeam ::
  forall a.
  (HasCallStack) =>
  UserId ->
  TeamId ->
  -- | the test
  (Chan (Wai.Request, LByteString) -> TestM a) ->
  TestM a
withDummyTestServiceForTeam owner tid go =
  withDummyTestServiceForTeamNoService $ \lhPort chan -> do
    newService <- newLegalHoldService lhPort
    postSettings owner tid newService !!! testResponse 201 Nothing
    go chan

-- FUTUREWORK: run this test suite against an actual LH service (by changing URL and key in
-- the config file), and see if it works as well as with our mock service.
withDummyTestServiceForTeamNoService ::
  forall a.
  (HasCallStack) =>
  -- | the test
  (Warp.Port -> Chan (Wai.Request, LByteString) -> TestM a) ->
  TestM a
withDummyTestServiceForTeamNoService go = do
  withTestService dummyService go
  where
    dummyService :: Chan (Wai.Request, LByteString) -> Wai.Application
    dummyService ch req cont = do
      reqBody <- Wai.strictRequestBody req
      writeChan ch (req, reqBody)
      case (pathInfo req, requestMethod req, getRequestHeader "Authorization" req) of
        (["legalhold", "status"], "GET", _) -> cont respondOk
        (_, _, Nothing) -> cont missingAuth
        (["legalhold", "initiate"], "POST", Just _) -> cont initiateResp
        (["legalhold", "confirm"], "POST", Just _) ->
          cont respondOk
        (["legalhold", "remove"], "POST", Just _) -> cont respondOk
        _ -> cont respondBad

    initiateResp :: Wai.Response
    initiateResp =
      Wai.json $
        -- FUTUREWORK: use another key to prevent collisions with keys used by tests
        NewLegalHoldClient somePrekeys (head $ someLastPrekeys)

    respondOk :: Wai.Response
    respondOk = responseLBS status200 mempty mempty

    respondBad :: Wai.Response
    respondBad = responseLBS status404 mempty mempty

    missingAuth :: Wai.Response
    missingAuth = responseLBS status400 mempty "no authorization header"

    getRequestHeader :: String -> Wai.Request -> Maybe ByteString
    getRequestHeader name req = lookup (fromString name) $ requestHeaders req

-- | FUTUREWORK: this function calls an internal end-point to whitelist a team.  It only
-- appears to bracket this state change and undo it in a finalizer.
--
-- We should probably not have this function, just do the call inline, and use the 'TestM'
-- actions again rather than the polymorphic ones that we have here.
--
-- it's here for historical reason because we did this in galley.yaml
-- at some point in the past rather than in an internal end-point, and that required spawning
-- another galley 'Application' with 'withSettingsOverrides'.
withLHWhitelist :: forall a. (HasCallStack) => TeamId -> TestM a -> TestM a
withLHWhitelist tid action = do
  void $ putLHWhitelistTeam tid
  opts <- view tsGConf
  withSettingsOverrides (const opts) action

-- | If you play with whitelists, you should use this one.  Every whitelisted team that does
-- not get fully deleted will blow up the whitelist that is cached in every warp handler.
withTeam :: forall a. (HasCallStack) => ((HasCallStack) => UserId -> TeamId -> TestM a) -> TestM a
withTeam action =
  bracket
    createBindingTeam
    (\(owner, tid) -> deleteTeam owner tid >> waitForDeleteEvent tid)
    (uncurry action)
  where
    waitForDeleteEvent :: TeamId -> TestM ()
    waitForDeleteEvent tid =
      assertTeamDelete 15 "waitForDeleteEvent" tid

withFreePortAnyAddr :: (MonadMask m, MonadIO m) => ((Warp.Port, Socket) -> m a) -> m a
withFreePortAnyAddr = bracket openFreePortAnyAddr (liftIO . Socket.close . snd)

openFreePortAnyAddr :: (MonadIO m) => m (Warp.Port, Socket)
openFreePortAnyAddr = liftIO $ bindRandomPortTCP "*"

-- | Run a test with an mock legal hold service application.  The mock service is also binding
-- to a TCP socket for the backend to connect to.  The mock service can expose internal
-- details to the test (for both read and write) via a 'Chan'.
--
-- WARNINGS: (1) This is not concurrency-proof!  (2) tests need to be written in a way that
-- they can be run several times if they fail the first time.  this is the allow for the ssl
-- service to have some time to propagate through the test system (needed on k8s).
withTestService ::
  (HasCallStack) =>
  -- | the mock service
  (Chan e -> Application) ->
  -- | the test
  (Warp.Port -> Chan e -> TestM a) ->
  TestM a
withTestService mkApp go = withFreePortAnyAddr $ \(sPort, sock) -> do
  config <- view (tsIConf . to provider)
  serverStarted <- newEmptyMVar
  let tlss = Warp.tlsSettings (cert config) (privateKey config)
  let defs = Warp.defaultSettings {Warp.settingsPort = sPort, Warp.settingsBeforeMainLoop = putMVar serverStarted ()}
  buf <- liftIO newChan
  srv <-
    liftIO . Async.async $
      Warp.runTLSSocket tlss defs sock $
        mkApp buf
  takeMVar serverStarted
  go sPort buf `finally` liftIO (Async.cancel srv)

publicKeyNotMatchingService :: PEM
publicKeyNotMatchingService =
  let Right [k] =
        pemParseBS . BS.unlines $
          [ "-----BEGIN PUBLIC KEY-----",
            "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAu+Kg/PHHU3atXrUbKnw0",
            "G06FliXcNt3lMwl2os5twEDcPPFw/feGiAKymxp+7JqZDrseS5D9THGrW+OQRIPH",
            "WvUBdiLfGrZqJO223DB6D8K2Su/odmnjZJ2z23rhXoEArTplu+Dg9K+c2LVeXTKV",
            "VPOaOzgtAB21XKRiQ4ermqgi3/njr03rXyq/qNkuNd6tNcg+HAfGxfGvvCSYBfiS",
            "bUKr/BeArYRcjzr/h5m1In6fG/if9GEI6m8dxHT9JbY53wiksowy6ajCuqskIFg8",
            "7X883H+LA/d6X5CTiPv1VMxXdBUiGPuC9IT/6CNQ1/LFt0P37ax58+LGYlaFo7la",
            "nQIDAQAZ",
            "-----END PUBLIC KEY-----"
          ]
   in k

----------------------------------------------------------------------
-- API helpers

getEnabled :: (HasCallStack) => TeamId -> TestM ResponseLBS
getEnabled tid = do
  g <- viewGalley
  get $
    g
      . paths ["i", "teams", toByteString' tid, "features", "legalhold"]

renewToken :: (HasCallStack) => Text -> TestM ()
renewToken tok = do
  b <- viewBrig
  void . post $
    b
      . paths ["access"]
      . cookieRaw "zuid" (toByteString' tok)
      . expect2xx

putEnabled :: (HasCallStack) => TeamId -> Public.FeatureStatus -> TestM ()
putEnabled tid enabled = do
  g <- viewGalley
  putEnabledM g tid enabled

putEnabledM :: (HasCallStack, MonadHttp m, MonadIO m) => GalleyR -> TeamId -> Public.FeatureStatus -> m ()
putEnabledM g tid enabled = void $ putEnabledM' g expect2xx tid enabled

putEnabled' :: (HasCallStack) => (Bilge.Request -> Bilge.Request) -> TeamId -> Public.FeatureStatus -> TestM ResponseLBS
putEnabled' extra tid enabled = do
  g <- viewGalley
  putEnabledM' g extra tid enabled

putEnabledM' :: (HasCallStack, MonadHttp m) => GalleyR -> (Bilge.Request -> Bilge.Request) -> TeamId -> Public.FeatureStatus -> m ResponseLBS
putEnabledM' g extra tid enabled = do
  put $
    g
      . paths ["i", "teams", toByteString' tid, "features", "legalhold"]
      . json (Public.WithStatusNoLock enabled Public.LegalholdConfig Public.FeatureTTLUnlimited)
      . extra

postSettings :: (HasCallStack) => UserId -> TeamId -> NewLegalHoldService -> TestM ResponseLBS
postSettings uid tid new =
  -- Retry calls to this endpoint, on k8s it sometimes takes a while to establish a working
  -- connection.
  retrying policy only412 $ \_ -> do
    g <- viewGalley
    post $
      g
        . paths ["teams", toByteString' tid, "legalhold", "settings"]
        . zUser uid
        . zConn "conn"
        . zType "access"
        . json new
  where
    policy :: RetryPolicy
    policy = limitRetriesByCumulativeDelay 5_000_000 $ exponentialBackoff 50
    only412 :: RetryStatus -> ResponseLBS -> TestM Bool
    only412 _ resp = pure $ statusCode resp == 412

getSettingsTyped :: (HasCallStack) => UserId -> TeamId -> TestM ViewLegalHoldService
getSettingsTyped uid tid = responseJsonUnsafe <$> (getSettings uid tid <!! testResponse 200 Nothing)

getSettings :: (HasCallStack) => UserId -> TeamId -> TestM ResponseLBS
getSettings uid tid = do
  g <- viewGalley
  get $
    g
      . paths ["teams", toByteString' tid, "legalhold", "settings"]
      . zUser uid
      . zConn "conn"
      . zType "access"

deleteSettings :: (HasCallStack) => Maybe PlainTextPassword6 -> UserId -> TeamId -> TestM ResponseLBS
deleteSettings mPassword uid tid = do
  g <- viewGalley
  delete $
    g
      . paths ["teams", toByteString' tid, "legalhold", "settings"]
      . zUser uid
      . zConn "conn"
      . zType "access"
      . json (RemoveLegalHoldSettingsRequest mPassword)

getUserStatusTyped :: (HasCallStack) => UserId -> TeamId -> TestM UserLegalHoldStatusResponse
getUserStatusTyped uid tid = do
  g <- viewGalley
  getUserStatusTyped' g uid tid

getUserStatusTyped' :: (HasCallStack, MonadHttp m, MonadIO m, MonadCatch m) => GalleyR -> UserId -> TeamId -> m UserLegalHoldStatusResponse
getUserStatusTyped' g uid tid = do
  resp <- getUserStatus' g uid tid <!! testResponse 200 Nothing
  pure $ responseJsonUnsafe resp

getUserStatus' :: (HasCallStack, MonadHttp m) => GalleyR -> UserId -> TeamId -> m ResponseLBS
getUserStatus' g uid tid = do
  get $
    g
      . paths ["teams", toByteString' tid, "legalhold", toByteString' uid]
      . zUser uid
      . zConn "conn"
      . zType "access"

approveLegalHoldDevice :: (HasCallStack) => Maybe PlainTextPassword6 -> UserId -> UserId -> TeamId -> TestM ResponseLBS
approveLegalHoldDevice mPassword zusr uid tid = do
  g <- viewGalley
  approveLegalHoldDevice' g mPassword zusr uid tid

approveLegalHoldDevice' ::
  (HasCallStack, MonadHttp m) =>
  GalleyR ->
  Maybe PlainTextPassword6 ->
  UserId ->
  UserId ->
  TeamId ->
  m ResponseLBS
approveLegalHoldDevice' g mPassword zusr uid tid = do
  put $
    g
      . paths ["teams", toByteString' tid, "legalhold", toByteString' uid, "approve"]
      . zUser zusr
      . zConn "conn"
      . zType "access"
      . json (ApproveLegalHoldForUserRequest mPassword)

disableLegalHoldForUser ::
  (HasCallStack) =>
  Maybe PlainTextPassword6 ->
  TeamId ->
  UserId ->
  UserId ->
  TestM ResponseLBS
disableLegalHoldForUser mPassword tid zusr uid = do
  g <- viewGalley
  disableLegalHoldForUser' g mPassword tid zusr uid

disableLegalHoldForUser' ::
  (HasCallStack, MonadHttp m) =>
  GalleyR ->
  Maybe PlainTextPassword6 ->
  TeamId ->
  UserId ->
  UserId ->
  m ResponseLBS
disableLegalHoldForUser' g mPassword tid zusr uid = do
  delete $
    g
      . paths ["teams", toByteString' tid, "legalhold", toByteString' uid]
      . zUser zusr
      . zType "access"
      . json (DisableLegalHoldForUserRequest mPassword)

assertExactlyOneLegalHoldDevice :: (HasCallStack) => UserId -> TestM ()
assertExactlyOneLegalHoldDevice uid = do
  clients :: [Client] <-
    getClients uid >>= responseJsonError
  liftIO $ do
    let numdevs = length $ clientType <$> clients
    assertEqual ("expected exactly one legal hold device for user: " <> show uid) numdevs 1

assertZeroLegalHoldDevices :: (HasCallStack) => UserId -> TestM ()
assertZeroLegalHoldDevices uid = do
  clients :: [Client] <-
    getClients uid >>= responseJsonError
  liftIO $ do
    let numdevs = length $ clientType <$> clients
    assertBool
      ( "a legal hold device was found when none was expected for user"
          <> show uid
      )
      (numdevs == 0)

---------------------------------------------------------------------
--- Device helpers

----------------------------------------------------------------------
---- Device helpers

grantConsent :: (HasCallStack) => TeamId -> UserId -> TestM ()
grantConsent tid zusr = do
  g <- viewGalley
  grantConsent' g tid zusr

grantConsent' :: (HasCallStack, MonadHttp m, MonadIO m) => GalleyR -> TeamId -> UserId -> m ()
grantConsent' = grantConsent'' expect2xx

grantConsent'' :: (HasCallStack, MonadHttp m, MonadIO m) => (Bilge.Request -> Bilge.Request) -> GalleyR -> TeamId -> UserId -> m ()
grantConsent'' expectation g tid zusr = do
  void . post $
    g
      . paths ["teams", toByteString' tid, "legalhold", "consent"]
      . zUser zusr
      . zConn "conn"
      . zType "access"
      . expectation

requestLegalHoldDevice :: (HasCallStack) => UserId -> UserId -> TeamId -> TestM ResponseLBS
requestLegalHoldDevice zusr uid tid = do
  g <- viewGalley
  requestLegalHoldDevice' g zusr uid tid

requestLegalHoldDevice' :: (HasCallStack, MonadHttp m) => GalleyR -> UserId -> UserId -> TeamId -> m ResponseLBS
requestLegalHoldDevice' g zusr uid tid = do
  post $
    g
      . paths ["teams", toByteString' tid, "legalhold", toByteString' uid]
      . zUser zusr
      . zConn "conn"
      . zType "access"

----------------------------------------------------------------------
-- test helpers

-- (partial implementation, just good enough to make the tests work)
instance FromJSON Ev.UserEvent where
  parseJSON = withObject "Ev.UserEvent" $ \o -> do
    tag :: Text <- o .: "type"
    case tag of
      "user.legalhold-enable" -> Ev.UserLegalHoldEnabled <$> o .: "id"
      "user.legalhold-disable" -> Ev.UserLegalHoldDisabled <$> o .: "id"
      "user.legalhold-request" ->
        Ev.LegalHoldClientRequested
          <$> ( Ev.LegalHoldClientRequestedData
                  <$> o .: "id" -- this is the target user
                  <*> o .: "last_prekey"
                  <*> (o .: "client" >>= withObject "id" (.: "id"))
              )
      x -> fail $ "Ev.UserEvent: unsupported event type: " ++ show x

-- (partial implementation, just good enough to make the tests work)
instance FromJSON Ev.ClientEvent where
  parseJSON = withObject "Ev.ClientEvent" $ \o -> do
    tag :: Text <- o .: "type"
    case tag of
      "user.client-add" -> Ev.ClientAdded <$> o .: "client"
      "user.client-remove" -> Ev.ClientRemoved <$> (o .: "client" >>= withObject "id" (.: "id"))
      x -> fail $ "Ev.ClientEvent: unsupported event type: " ++ show x

instance FromJSON Ev.ConnectionEvent where
  parseJSON = Aeson.withObject "ConnectionEvent" $ \o -> do
    tag :: Text <- o .: "type"
    case tag of
      "user.connection" ->
        Ev.ConnectionUpdated
          <$> o .: "connection"
          <*> pure Nothing
      x -> fail $ "unspported event type: " ++ show x

assertNotification :: (HasCallStack, FromJSON a, MonadIO m) => WS.WebSocket -> (a -> Assertion) -> m ()
assertNotification ws predicate =
  void . liftIO . WS.assertMatch (5 WS.# WS.Second) ws $ \notif -> do
    unless ((NonEmpty.length . List1.toNonEmpty $ ntfPayload $ notif) == 1) $
      error $
        "not suppored by test helper: event with more than one object in the payload: " <> cs (Aeson.encode notif)
    let j = Aeson.Object $ List1.head (ntfPayload notif)
    case Aeson.fromJSON j of
      Aeson.Success x -> predicate x
      Aeson.Error s -> error $ s ++ " in " ++ cs (Aeson.encode j)

assertNoNotification :: (HasCallStack, MonadIO m) => WS.WebSocket -> m ()
assertNoNotification ws = void . liftIO $ WS.assertNoEvent (5 WS.# WS.Second) [ws]

assertMatchJSON :: (HasCallStack, FromJSON a, MonadCatch m, MonadIO m) => Chan (Wai.Request, LByteString) -> (a -> m ()) -> m ()
assertMatchJSON c match = do
  assertMatchChan c $ \(_, reqBody) -> do
    case Aeson.eitherDecode reqBody of
      Right x -> match x
      Left s -> error $ s ++ " in " ++ cs reqBody

assertMatchChan :: (HasCallStack, MonadCatch m, MonadIO m) => Chan a -> (a -> m ()) -> m ()
assertMatchChan c match = go []
  where
    refill = mapM_ (liftIO <$> writeChan c)
    go buf = do
      m <- liftIO . timeout (5 WS.# WS.Second) . readChan $ c
      case m of
        Just n ->
          do
            match n
            refill buf
            `catchAll` \e -> case asyncExceptionFromException e of
              Just x -> error $ show (x :: SomeAsyncException)
              Nothing -> go (n : buf)
        Nothing -> do
          refill buf
          error "Timeout"

getLHWhitelistedTeam :: (HasCallStack) => TeamId -> TestM ResponseLBS
getLHWhitelistedTeam tid = do
  galleyCall <- viewGalley
  getLHWhitelistedTeam' galleyCall tid

getLHWhitelistedTeam' :: (HasCallStack, MonadHttp m) => GalleyR -> TeamId -> m ResponseLBS
getLHWhitelistedTeam' g tid = do
  get
    ( g
        . paths ["i", "legalhold", "whitelisted-teams", toByteString' tid]
    )

putLHWhitelistTeam :: (HasCallStack) => TeamId -> TestM ResponseLBS
putLHWhitelistTeam tid = do
  galleyCall <- viewGalley
  putLHWhitelistTeam' galleyCall tid

putLHWhitelistTeam' :: (HasCallStack, MonadHttp m) => GalleyR -> TeamId -> m ResponseLBS
putLHWhitelistTeam' g tid = do
  put
    ( g
        . paths ["i", "legalhold", "whitelisted-teams", toByteString' tid]
    )

errWith :: (HasCallStack, Typeable a, FromJSON a) => Int -> (a -> Bool) -> ResponseLBS -> TestM ()
errWith wantStatus wantBody rsp = liftIO $ do
  assertEqual "" wantStatus (statusCode rsp)
  assertBool
    (show $ responseBody rsp)
    ( maybe False wantBody (responseJsonMaybe rsp)
    )

------------------------------------

testOnlyIfLhEnabled :: IO TestSetup -> TestName -> TestM () -> TestTree
testOnlyIfLhEnabled setupAction name testAction = do
  singleTest name $ LHTest FeatureLegalHoldDisabledByDefault setupAction testAction

testOnlyIfLhWhitelisted :: IO TestSetup -> TestName -> TestM () -> TestTree
testOnlyIfLhWhitelisted setupAction name testAction = do
  singleTest name $ LHTest FeatureLegalHoldWhitelistTeamsAndImplicitConsent setupAction testAction

data LHTest = LHTest FeatureLegalHold (IO TestSetup) (TestM ())

instance IsTest LHTest where
  run :: OptionSet -> LHTest -> (Progress -> IO ()) -> IO Result
  run _ (LHTest expectedFlag setupAction testAction) _ = do
    setup <- setupAction
    let featureLegalHold = setup ^. tsGConf . settings . featureFlags . flagLegalHold
    if featureLegalHold == expectedFlag
      then do
        hunitResult <- try $ void . flip runReaderT setup . runTestM $ testAction
        pure $
          case hunitResult of
            Right _ -> testPassed ""
            Left (HUnitFailure stack message) -> testFailed $ prependCallStack stack message
      else pure . skipTest $ "test skipped because: \n  " <> show expectedFlag <> " /= " <> show featureLegalHold

  testOptions :: Tagged LHTest [OptionDescription]
  testOptions = pure []

-- | Skipped tests are to be marked as failed in tasty. See this comment for
-- details:
-- https://github.com/UnkindPartition/tasty/blob/0debac85701560e8c96cd3705988c50197cb214e/core/Test/Tasty/Core.hs#L99-L119
skipTest :: String -> Result
skipTest reason =
  Result
    { resultOutcome = Success,
      resultDescription = reason,
      resultShortDescription = "SKIP",
      resultTime = 0,
      resultDetailsPrinter = noResultDetails
    }

prependCallStack :: CallStack -> String -> String
prependCallStack stack s =
  "Error message: " <> s <> "\n\n" <> prettyCallStack stack

prettyCallStack :: CallStack -> String
prettyCallStack = intercalate "\n" . prettyCallStackLines

prettyCallStackLines :: CallStack -> [String]
prettyCallStackLines stack = case stack of
  [] -> []
  stk ->
    "CallStack (from HasCallStack):"
      : map (("  " ++) . prettyCallSite) stk
  where
    prettyCallSite (f, loc) = f ++ ", called at " ++ prettySrcLoc loc

prettySrcLoc :: SrcLoc -> String
prettySrcLoc SrcLoc {..} =
  concat
    [ srcLocFile,
      ":",
      show srcLocStartLine,
      ":",
      show srcLocStartCol,
      " in ",
      srcLocPackage,
      ":",
      srcLocModule
    ]
