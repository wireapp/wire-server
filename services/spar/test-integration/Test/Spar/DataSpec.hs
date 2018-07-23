{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Test.Spar.DataSpec where

import Bilge
import Cassandra as Cas
import Control.Concurrent
import Control.Exception
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Either (isRight)
import Data.Id
import Data.Maybe (isJust)
import Data.String.Conversions
import Data.Time
import Data.UUID as UUID
import Data.UUID.V4 as UUID
import Lens.Micro
import Spar.API
import Spar.API.Instances ()
import Spar.API.Test (IntegrationTests)
import Spar.Data as Data
import Spar.Options as Options
import Spar.Types
import URI.ByteString as URI
import URI.ByteString.QQ (uri)
import Util
import Util.Options
import Web.Cookie

import qualified Data.List as List
import qualified SAML2.WebSSO as SAML
import qualified Servant
import qualified Servant.Client as Servant
import qualified Text.XML as XML
import qualified Text.XML.Util as SAML


spec :: SpecWith TestEnv
spec = do
  describe "TTL" $ do
    it "works in seconds" $ do
      env <- ask
      (_, _, idpid) <- createTestIdP
      (_, req) <- call $ callAuthnReq (env ^. teSpar) idpid

      let probe :: IO Bool
          probe = do
            denv :: Data.Env <- Data.mkEnv (env ^. teOpts) <$> getCurrentTime
            runClient (env ^. teCql) (checkAgainstRequest (req ^. SAML.rqID) `runReaderT` denv)

          maxttl :: Int  -- musec
          maxttl = (fromIntegral . fromTTL $ env ^. teOpts . to maxttlAuthreq) * 1000 * 1000

      liftIO $ do
        maxttl `shouldSatisfy` (< 60 * 1000 * 1000)  -- otherwise the test will be really slow.
        probe `shouldReturn` True
        threadDelay ((maxttl `div` 10) * 8)
        probe `shouldReturn` True
        threadDelay  ((maxttl `div` 10) * 4)
        probe `shouldReturn` False


  describe "cql binding" $ do

    -- requests

    let runStoreReq :: TestEnv -> SAML.ID SAML.AuthnRequest -> Http ()
        runStoreReq env authreqid = do
          endoflife <- getTime (fromIntegral ttlReqs)
          runServantClient env $ clientPostRequest authreqid endoflife

        runCheckReq :: TestEnv -> SAML.ID SAML.AuthnRequest -> Http Bool
        runCheckReq env authreqid = do
          runServantClient env $ clientGetRequest authreqid

        getID :: MonadIO m => m (SAML.ID a)
        getID = SAML.ID . UUID.toText <$> liftIO UUID.nextRandom

        getTime :: MonadIO m => NominalDiffTime -> m SAML.Time
        getTime secs = (SAML.addTime secs) . SAML.Time <$> liftIO getCurrentTime

        ttlReqs :: Int
        ttlReqs = 3

    describe "storeRequest" $ do
      it "responds with 2xx" $ do
        env <- ask
        reqid <- getID
        runStoreReq env reqid `shouldRespondWith` (== ())

    describe "checkAgainstRequest" $ do
      context "request exists and is alive" $ do
        it "returns True" $ do
          env <- ask
          reqid <- getID
          runStoreReq env reqid `shouldRespondWith` (== ())
          runCheckReq env reqid `shouldRespondWith` (== True)

      context "request does not exist" $ do
        it "returns False" $ do
          env <- ask
          reqid <- getID
          runCheckReq env reqid `shouldRespondWith` (== False)

      context "request exists, but is outdated" $ do
        it "returns False" $ do
          env <- ask
          reqid <- getID
          runStoreReq env reqid `shouldRespondWith` (== ())
          liftIO $ threadDelay (ttlReqs * 1000 * 1000)
          runCheckReq env reqid `shouldRespondWith` (== False)


    -- assertions

    let runStoreAssertion :: TestEnv -> SAML.ID SAML.Assertion -> NominalDiffTime -> Http Bool
        runStoreAssertion env assid ttl = do
          endoflife <- getTime ttl
          runServantClient env $ clientPostAssertion assid endoflife

    describe "storeAssertion" $ do
      context "assertion does not exist" $ do
        it "returns True" $ do
          env  <- ask
          uuid <- SAML.ID . UUID.toText <$> liftIO UUID.nextRandom
          runStoreAssertion env uuid 1 `shouldRespondWith` (== True)

      context "assertion exists, but is outdated" $ do
        it "returns True" $ do
          env  <- ask
          uuid <- SAML.ID . UUID.toText <$> liftIO UUID.nextRandom
          runStoreAssertion env uuid 1 `shouldRespondWith` (== True)
          liftIO $ threadDelay (2 * 1000 * 1000)
          runStoreAssertion env uuid 1 `shouldRespondWith` (== True)

      context "assertion exists and is alive" $ do
        it "returns False" $ do
          env  <- ask
          uuid <- SAML.ID . UUID.toText <$> liftIO UUID.nextRandom
          runStoreAssertion env uuid 2 `shouldRespondWith` (== True)
          runStoreAssertion env uuid 2 `shouldRespondWith` (== False)


    -- users

    let runInsertUser :: TestEnv -> SAML.UserRef -> UserId -> Http ()
        runInsertUser env uref = runServantClient env . clientPostUser uref

        runGetUser :: TestEnv -> SAML.UserRef -> Http (Maybe UserId)
        runGetUser env = runServantClient env . clientGetUser

        nextUserRef :: MonadIO m => m SAML.UserRef
        nextUserRef = liftIO $ do
          (UUID.toText -> tenant) <- UUID.nextRandom
          (UUID.toText -> subject) <- UUID.nextRandom
          pure $ SAML.UserRef
            (SAML.Issuer $ SAML.unsafeParseURI ("http://" <> tenant))
            (SAML.opaqueNameID subject)

    describe "insertUser, getUser" $ do
      context "user is new" $ do
        it "getUser returns Nothing" $ do
          env  <- ask
          uref <- nextUserRef
          runGetUser env uref `shouldRespondWith` (== Nothing)

        it "inserts new user and responds with 201 / returns new user" $ do
          env  <- ask
          uref <- nextUserRef
          uid  <- Id <$> liftIO UUID.nextRandom
          runInsertUser env uref uid `shouldRespondWith` (== ())  -- no exception
          runGetUser env uref `shouldRespondWith` (== Just uid)

      context "user already exists (idempotency)" $ do
        it "inserts new user and responds with 201 / returns new user" $ do
          env  <- ask
          uref <- nextUserRef
          uid  <- Id <$> liftIO UUID.nextRandom
          uid' <- Id <$> liftIO UUID.nextRandom
          runInsertUser env uref uid `shouldRespondWith` (== ())
          runInsertUser env uref uid' `shouldRespondWith` (== ())
          runGetUser env uref `shouldRespondWith` (== Just uid')


    -- access verdict

    let runPostVerdict :: HasCallStack => TestEnv -> (SAML.AuthnResponse, SAML.AccessVerdict) -> Http SAML.ResponseVerdict
        runPostVerdict env = runServantClient env . clientPostVerdict

    describe "accessVerdict" $ do
      context "web" $ do
        context "invalid idp" $ do
          it "responds with status 200 and a valid html page with constant expected title." $ do
            pending

        context "denied" $ do
          it "responds with status 200 and a valid html page with constant expected title." $ do
            env <- ask
            outcome <- call $ runPostVerdict env (emptyAuthnResponse, SAML.AccessDenied ["we don't like you", "seriously"])
            liftIO $ do
              Servant.errHTTPCode outcome `shouldBe` 200
              Servant.errReasonPhrase outcome `shouldBe` "forbidden"
              ('1', cs @LBS @String (Servant.errBody outcome))
                `shouldSatisfy` (("<title>wire:sso:error:forbidden</title>" `List.isInfixOf`) . snd)
              ('2', XML.parseLBS XML.def $ Servant.errBody outcome)
                `shouldSatisfy` (isRight . snd)

        context "granted" $ do
          it "responds with status 200 and a valid html page with constant expected title." $ do
            (_, _, _) <- createTestIdP
            env <- ask
            let tenant  = sampleIdP ^. nidpIssuer
                subject = SAML.opaqueNameID "blee"
            outcome <- call $ runPostVerdict env (emptyAuthnResponse, SAML.AccessGranted (SAML.UserRef tenant subject))
            liftIO $ do
              Servant.errHTTPCode outcome `shouldBe` 200
              Servant.errReasonPhrase outcome `shouldBe` "success"
              ('1', cs @LBS @String (Servant.errBody outcome))
                `shouldSatisfy` (("<title>wire:sso:success</title>" `List.isInfixOf`) . snd)
              ('2', XML.parseLBS XML.def (Servant.errBody outcome))
                `shouldSatisfy` (isRight . snd)
              ('3', List.lookup "Set-Cookie" . Servant.errHeaders $ outcome)
                `shouldSatisfy` (isJust . snd)

      context "mobile" $ do
        let prepare :: HasCallStack => Bool -> ReaderT TestEnv IO (UserId, SAML.ResponseVerdict, URI, [(SBS, SBS)])
            prepare isGranted = do
              (uid, _, idpid) <- createTestIdP
              env <- ask
              let tenant  = sampleIdP ^. nidpIssuer
                  subject = SAML.opaqueNameID "blee"
                  uref    = SAML.UserRef tenant subject
              call $ runInsertUser env uref uid
              authnreq <- call . runServantClient env $ clientGetAuthnRequest
                (Just [uri|wire://login-granted/?cookie=$cookie&userid=$userid|])
                (Just [uri|wire://login-denied/?label=$label|])
                idpid
              let authnresp = fleshOutResponse emptyAuthnResponse authnreq
                  verdict = if isGranted
                    then SAML.AccessGranted uref
                    else SAML.AccessDenied ["we don't like you", "seriously"]
              outcome <- call $ runPostVerdict env (authnresp, verdict)
              let loc :: URI.URI
                  loc = maybe (error "no location") (either error id . SAML.parseURI' . cs)
                      . List.lookup "Location" . Servant.errHeaders
                      $ outcome
                  qry :: [(SBS, SBS)]
                  qry = queryPairs $ uriQuery loc
              pure (uid, outcome, loc, qry)

        context "invalid idp" $ do
          it "responds with status 302 to the error redirect." $ do
            pending

        context "denied" $ do
          it "responds with status 302 to the error redirect." $ do
            (_uid, outcome, loc, qry) <- prepare False
            liftIO $ do
              Servant.errHTTPCode outcome `shouldBe` 302
              Servant.errReasonPhrase outcome `shouldBe` "forbidden"
              Servant.errBody outcome `shouldBe` mempty
              uriScheme loc `shouldBe` (URI.Scheme "wire")
              List.lookup "userid" qry `shouldNotBe` Nothing
              List.lookup "cookie" qry `shouldNotBe` Nothing
              List.lookup "label"  qry `shouldBe`    (Just "forbidden")

        context "granted" $ do
          it "responds with status 200 and a valid html page with constant expected title." $ do
            (uid, outcome, loc, qry) <- prepare True
            liftIO $ do
              Servant.errHTTPCode outcome `shouldBe` 302
              Servant.errReasonPhrase outcome `shouldBe` "success"
              Servant.errBody outcome `shouldBe` mempty
              uriScheme loc `shouldBe` (URI.Scheme "wire")
              List.lookup "label"  qry `shouldNotBe` Nothing
              List.lookup "userid" qry `shouldBe` (Just . cs . show $ uid)
              List.lookup "cookie" qry `shouldNotBe` Nothing
              List.lookup "cookie" qry `shouldNotBe` Just "$cookie"
              let Just (ckies :: SBS) = List.lookup "cookie" qry
                  cky :: SetCookie = parseSetCookie . (error . show) . parseCookies $ ckies
              setCookieName cky `shouldBe` "wire"
              setCookieSecure cky `shouldBe` True
              setCookieHttpOnly cky `shouldBe` True


runServantClient :: HasCallStack => TestEnv -> Servant.ClientM a -> Http a
runServantClient tenv = hoist . (`Servant.runClientM` cenv)
  where
    hoist :: IO (Either Servant.ServantError a) -> Http a
    hoist = HttpT . ReaderT . const . (>>= either (throwM . ErrorCall . show) pure)

    cenv :: Servant.ClientEnv
    cenv = Servant.ClientEnv (tenv ^. teMgr) (tenv ^. teTstOpts . to cfgSpar . to endpointToUrl)
      where
        endpointToUrl :: Endpoint -> Servant.BaseUrl
        endpointToUrl (Endpoint sparhost sparport) =
          Servant.BaseUrl Servant.Http (cs sparhost) (fromIntegral sparport) "/i/integration-tests"

clientPostRequest   :: SAML.ID SAML.AuthnRequest -> SAML.Time -> Servant.ClientM ()
clientGetRequest    :: SAML.ID SAML.AuthnRequest -> Servant.ClientM Bool
clientPostAssertion :: SAML.ID SAML.Assertion -> SAML.Time -> Servant.ClientM Bool
clientPostUser      :: SAML.UserRef -> UserId -> Servant.ClientM ()
clientGetUser       :: SAML.UserRef -> Servant.ClientM (Maybe UserId)
clientPostVerdict   :: (SAML.AuthnResponse, SAML.AccessVerdict) -> Servant.ClientM Servant.ServantErr

clientPostRequest     Servant.:<|>
  clientGetRequest    Servant.:<|>
  clientPostAssertion Servant.:<|>
  clientPostUser      Servant.:<|>
  clientGetUser       Servant.:<|>
  clientPostVerdict
  = Servant.client (Servant.Proxy @IntegrationTests)

clientGetAuthnRequest :: Maybe URI -> Maybe URI -> SAML.IdPId -> Servant.ClientM (SAML.FormRedirect SAML.AuthnRequest)
clientGetAuthnRequest = Servant.client (Servant.Proxy @APIAuthReq)


emptyAuthnResponse :: SAML.AuthnResponse
emptyAuthnResponse = SAML.Response
  { SAML._rspID           = SAML.ID "bleep"
  , SAML._rspInRespTo     = Nothing
  , SAML._rspVersion      = SAML.Version_2_0
  , SAML._rspIssueInstant = SAML.unsafeReadTime "2018-04-13T06:33:02.772Z"
  , SAML._rspDestination  = Nothing
  , SAML._rspIssuer       = Nothing
  , SAML._rspStatus       = SAML.StatusSuccess
  , SAML._rspPayload      = []
  }

-- | See 'verdictHandler' on the question of why we don't need an 'Assertion' in the payload here.
fleshOutResponse :: SAML.AuthnResponse -> SAML.FormRedirect SAML.AuthnRequest -> SAML.AuthnResponse
fleshOutResponse resp (SAML.FormRedirect _ req) = resp & SAML.rspInRespTo .~ Just (req ^. SAML.rqID)
