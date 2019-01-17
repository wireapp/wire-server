{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Two (weak) reasons why I implemented the clients without the help of servant-client: (1) I
-- wanted smooth integration in 'HttpMonad'; (2) I wanted the choice of receiving the unparsed
-- 'ResponseLBS' rather than the parsed result (or a hard-to examine error).  this is important for
-- testing for expected failures.  See also: https://github.com/haskell-servant/servant/issues/1004
--
-- FUTUREWORK: this is all copied from /services/galley/test/integration/API/Util.hs and some other
-- places; should we make this a new library?  (@tiago-loureiro says no that's fine.)
module Util.Core
  (
  -- * Test environment
    mkEnvFromOptions, mkEnv, destroyEnv
  -- * Test helpers
  , passes, it, pending, pendingWith
  , shouldRespondWith
  , module Test.Hspec
  -- * HTTP
  , call
  , endpointToReq
  , endpointToSettings
  , endpointToURL
  , responseJSON
  , decodeBody
  , decodeBody'
  -- * Other
  , createUserWithTeam
  , createTeamMember
  , deleteUser
  , getTeams
  , getSelfProfile
  , nextWireId
  , nextSAMLID
  , nextUserRef
  , createRandomPhoneUser
  , zUser
  , ping
  , makeIssuer
  , makeTestIdPMetadata
  , getTestSPMetadata
  , registerTestIdP
  , registerTestIdPFrom
  , negotiateAuthnRequest
  , negotiateAuthnRequest'
  , getCookie
  , hasPersistentCookieHeader
  , hasDeleteBindCookieHeader
  , hasSetBindCookieHeader
  , submitAuthnResponse
  , submitAuthnResponse'
  , loginSsoUserFirstTime
  , callAuthnReqPrecheck'
  , callAuthnReq, callAuthnReq'
  , callIdpGet, callIdpGet'
  , callIdpGetAll, callIdpGetAll'
  , callIdpCreate, callIdpCreate'
  , callIdpDelete, callIdpDelete'
  , initCassandra
  , ssoToUidSpar
  , runSparCass, runSparCassWithEnv
  , runSimpleSP
  , runSpar
  , getSsoidViaSelf, getSsoidViaSelf'
  , getUserIdViaRef, getUserIdViaRef'
  ) where

import Imports hiding (head)
import Bilge hiding (getCookie)  -- we use Web.Cookie instead of the http-client type
import Bilge.Assert ((!!!), (===), (<!!))
import Brig.Types.Common (UserSSOId(..), UserIdentity(..))
import Brig.Types.User (userIdentity, selfUser)
import Cassandra as Cas
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.Except
import Control.Retry
import Data.Aeson as Aeson hiding (json)
import Data.Aeson.Lens as Aeson
import Data.ByteString.Conversion
import Data.EitherR (fmapL)
import Data.Id
import Data.Misc (PlainTextPassword(..))
import Data.Proxy
import Data.Range
import Data.String.Conversions
import Data.Time
import Data.UUID as UUID hiding (null, fromByteString)
import Data.UUID.V4 as UUID (nextRandom)
import GHC.TypeLits
import Network.HTTP.Client.MultipartFormData
import SAML2.WebSSO as SAML
import SAML2.WebSSO.Test.Credentials
import SAML2.WebSSO.Test.MockResponse
import Spar.API.Types
import Spar.Run
import Spar.Types
import System.Random (randomRIO)
import Test.Hspec hiding (it, xit, pending, pendingWith)
import URI.ByteString
import URI.ByteString.QQ (uri)
import Util.Options
import Util.Types

import qualified Brig.Types.Activation as Brig
import qualified Brig.Types.User as Brig
import qualified Brig.Types.User.Auth as Brig
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Base64.Lazy as EL
import qualified Data.Text.Ascii as Ascii
import qualified Data.Yaml as Yaml
import qualified Galley.Types.Teams as Galley
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.Warp.Internal as Warp
import qualified Options.Applicative as OPA
import qualified SAML2.WebSSO.API.Example as SAML
import qualified Spar.App as Spar
import qualified Spar.Data as Data
import qualified Spar.Intra.Brig as Intra
import qualified Spar.Options
import qualified Test.Hspec
import qualified Text.XML as XML
import qualified Text.XML.Cursor as XML
import qualified Text.XML.DSig as SAML
import qualified Web.Cookie as Web


-- | Call 'mkEnv' with options from config files.
mkEnvFromOptions :: IO TestEnv
mkEnvFromOptions = do
  let desc = "Spar - SSO Service Integration Test Suite"
  (integrationCfgFilePath, cfgFilePath) <- OPA.execParser (OPA.info (OPA.helper <*> cliOptsParser) (OPA.header desc <> OPA.fullDesc))
  integrationOpts :: IntegrationConfig <- Yaml.decodeFileEither integrationCfgFilePath >>= either (error . show) pure
  serviceOpts :: Opts <- Yaml.decodeFileEither cfgFilePath >>= either (throwIO . ErrorCall . show) Spar.Options.deriveOpts
  mkEnv integrationOpts serviceOpts

-- | Accept config file locations as cli options.
cliOptsParser :: OPA.Parser (String, String)
cliOptsParser = (,) <$>
  (OPA.strOption $
    OPA.long "integration-config"
    <> OPA.short 'i'
    <> OPA.help "Integration config to load"
    <> OPA.showDefault
    <> OPA.value defaultIntPath)
  <*>
  (OPA.strOption $
    OPA.long "service-config"
    <> OPA.short 's'
    <> OPA.help "Spar application config to load"
    <> OPA.showDefault
    <> OPA.value defaultSparPath)
  where
    defaultIntPath = "/etc/wire/integration/integration.yaml"
    defaultSparPath = "/etc/wire/spar/conf/spar.yaml"

-- | Create an environment for integration tests from integration and spar config files.
--
-- NB: We used to have a mock IdP server here that allowed spar to resolve metadata URLs and pull
-- metadata.  (It *could* have been used by the test suite to get 'AuthnRequest' values as well, but
-- that's no more interesting than simulating the idp end-point from inside the spar-integration
-- executable as a monadic function, only more complicated.)  Since spar does not accept metadata
-- URLs any more <https://github.com/wireapp/wire-server/pull/466#issuecomment-419396359>, we
-- removed the mock idp functionality.  if you want to re-introduce it,
-- <https://github.com/wireapp/wire-server/pull/466/commits/9c93f1e278500522a0565639140ac55dc21ee2d2>
-- would be a good place to look for code to steal.
mkEnv :: HasCallStack => IntegrationConfig -> Opts -> IO TestEnv
mkEnv _teTstOpts _teOpts = do
  _teMgr :: Manager <- newManager defaultManagerSettings
  sparCtxLogger <- mkLogger _teOpts
  _teCql :: ClientState <- initCassandra _teOpts sparCtxLogger
  let _teBrig            = endpointToReq (cfgBrig   _teTstOpts)
      _teGalley          = endpointToReq (cfgGalley _teTstOpts)
      _teSpar            = endpointToReq (cfgSpar   _teTstOpts)
      _teSparEnv         = Spar.Env {..}
      sparCtxOpts        = _teOpts
      sparCtxCas         = _teCql
      sparCtxHttpManager = _teMgr
      sparCtxHttpBrig    = _teBrig empty
      sparCtxHttpGalley  = _teGalley empty
      sparCtxRequestId   = RequestId "<fake request id>"
  pure TestEnv {..}

destroyEnv :: HasCallStack => TestEnv -> IO ()
destroyEnv _ = pure ()


passes :: MonadIO m => m ()
passes = liftIO $ True `shouldBe` True

it :: HasCallStack
       -- or, more generally:
       -- MonadIO m, Example (TestEnv -> m ()), Arg (TestEnv -> m ()) ~ TestEnv
   => String -> TestSpar () -> SpecWith TestEnv
it msg bdy = Test.Hspec.it msg $ runReaderT bdy

pending :: (HasCallStack, MonadIO m) => m ()
pending = liftIO Test.Hspec.pending

pendingWith :: (HasCallStack, MonadIO m) => String -> m ()
pendingWith = liftIO . Test.Hspec.pendingWith


createUserWithTeam :: (HasCallStack, MonadHttp m, MonadIO m) => BrigReq -> GalleyReq -> m (UserId, TeamId)
createUserWithTeam brg gly = do
    e <- randomEmail
    n <- UUID.toString <$> liftIO UUID.nextRandom
    let p = RequestBodyLBS . Aeson.encode $ object
            [ "name"            .= n
            , "email"           .= Brig.fromEmail e
            , "password"        .= defPassword
            , "team"            .= newTeam
            ]
    bdy <- decodeBody' <$> post (brg . path "/i/users" . contentJson . body p)
    let (uid, Just tid) = (Brig.userId bdy, Brig.userTeam bdy)
    (team:_) <- (^. Galley.teamListTeams) <$> getTeams uid gly
    () <- Control.Exception.assert {- "Team ID in registration and team table do not match" -} (tid ==  team ^. Galley.teamId)
          $ pure ()
    selfTeam <- Brig.userTeam . Brig.selfUser <$> getSelfProfile brg uid
    () <- Control.Exception.assert {- "Team ID in self profile and team table do not match" -} (selfTeam == Just tid)
          $ pure ()
    return (uid, tid)

-- | NB: this does create an SSO UserRef on brig, but not on spar.  this is inconsistent, but the
-- inconsistency does not affect the tests we're running with this.  to resolve it, we could add an
-- internal end-point to spar that allows us to create users without idp response verification.
createTeamMember :: (HasCallStack, MonadCatch m, MonadIO m, MonadHttp m)
                 => BrigReq -> GalleyReq -> TeamId -> Galley.Permissions -> m UserId
createTeamMember brigreq galleyreq teamid perms = do
  let randomtxt = liftIO $ UUID.toText <$> UUID.nextRandom
      randomssoid = Brig.UserSSOId <$> randomtxt <*> randomtxt
  name  <- randomtxt
  ssoid <- randomssoid
  resp :: ResponseLBS
    <- postUser name False (Just ssoid) (Just teamid) brigreq
       <!! const 201 === statusCode
  let nobody :: UserId            = Brig.userId (decodeBody' @Brig.User resp)
      tmem   :: Galley.TeamMember = Galley.newTeamMember nobody perms Nothing
  addTeamMember galleyreq teamid (Galley.newNewTeamMember tmem)
  pure nobody

addTeamMember :: (HasCallStack, MonadCatch m, MonadIO m, MonadHttp m)
              => GalleyReq -> TeamId -> Galley.NewTeamMember -> m ()
addTeamMember galleyreq tid mem =
    void $ post ( galleyreq
                . paths ["i", "teams", toByteString' tid, "members"]
                . contentJson
                . expect2xx
                . lbytes (Aeson.encode mem)
                )

-- | Delete a user from Brig and wait until it's gone.
deleteUser :: (HasCallStack, MonadMask m, MonadCatch m, MonadIO m, MonadHttp m)
           => BrigReq -> UserId -> m ()
deleteUser brigreq uid = do
    deleteUserNoWait brigreq uid
    recoverAll (exponentialBackoff 30000 <> limitRetries 5) $ \_ -> do
        profile <- getSelfProfile brigreq uid
        liftIO $ selfUser profile `shouldSatisfy` Brig.userDeleted

-- | Delete a user from Brig but don't wait.
deleteUserNoWait :: (HasCallStack, MonadCatch m, MonadIO m, MonadHttp m)
                 => BrigReq -> UserId -> m ()
deleteUserNoWait brigreq uid =
    void $ delete ( brigreq
                  . paths ["i", "users", toByteString' uid]
                  . expect2xx
                  )

-- | See also: 'nextSAMLID', 'nextUserRef'.  The names are chosed to be consistent with
-- 'UUID.nextRandom'.
nextWireId :: MonadIO m => m (Id a)
nextWireId = Id <$> liftIO UUID.nextRandom

nextSAMLID :: MonadIO m => m (ID a)
nextSAMLID = ID . UUID.toText <$> liftIO UUID.nextRandom

nextUserRef :: MonadIO m => m SAML.UserRef
nextUserRef = liftIO $ do
  (UUID.toText -> tenant) <- UUID.nextRandom
  (UUID.toText -> subject) <- UUID.nextRandom
  pure $ SAML.UserRef
    (SAML.Issuer $ SAML.unsafeParseURI ("http://" <> tenant))
    (SAML.opaqueNameID subject)

createRandomPhoneUser :: (HasCallStack, MonadCatch m, MonadIO m, MonadHttp m) => BrigReq -> m (UserId, Brig.Phone)
createRandomPhoneUser brig_ = do
    usr <- randomUser brig_
    let uid = Brig.userId usr
    phn <- liftIO randomPhone
    -- update phone
    let phoneUpdate = RequestBodyLBS . Aeson.encode $ Brig.PhoneUpdate phn
    put (brig_ . path "/self/phone" . contentJson . zUser uid . zConn "c" . body phoneUpdate) !!!
        (const 202 === statusCode)
    -- activate
    act <- getActivationCode brig_ (Right phn)
    case act of
        Nothing -> liftIO . throwIO $ ErrorCall "missing activation key/code"
        Just kc -> activate brig_ kc !!! const 200 === statusCode
    -- check new phone
    get (brig_ . path "/self" . zUser uid) !!! do
        const 200 === statusCode
        const (Right (Just phn)) === (fmap Brig.userPhone . decodeBody)

    return (uid, phn)

decodeBody :: (HasCallStack, FromJSON a) => ResponseLBS -> Either String a
decodeBody = maybe (Left "no body") (\s -> (<> (": " <> cs (show s))) `fmapL` eitherDecode' s) . responseBody

decodeBody' :: (HasCallStack, FromJSON a) => ResponseLBS -> a
decodeBody' = either (error . show) id . decodeBody

getTeams :: (HasCallStack, MonadHttp m, MonadIO m) => UserId -> GalleyReq -> m Galley.TeamList
getTeams u gly = do
    r <- get ( gly
             . paths ["teams"]
             . zAuthAccess u "conn"
             . expect2xx
             )
    return $ decodeBody' r

getSelfProfile :: (HasCallStack, MonadHttp m, MonadIO m) => BrigReq -> UserId -> m Brig.SelfProfile
getSelfProfile brg usr = do
    rsp <- get $ brg . path "/self" . zUser usr
    return $ decodeBody' rsp

zAuthAccess :: UserId -> SBS -> Request -> Request
zAuthAccess u c = header "Z-Type" "access" . zUser u . zConn c

newTeam :: Galley.BindingNewTeam
newTeam = Galley.BindingNewTeam $ Galley.newNewTeam (unsafeRange "teamName") (unsafeRange "defaultIcon")

randomEmail :: MonadIO m => m Brig.Email
randomEmail = do
    uid <- liftIO nextRandom
    return $ Brig.Email ("success+" <> UUID.toText uid) "simulator.amazonses.com"

randomPhone :: MonadIO m => m Brig.Phone
randomPhone = liftIO $ do
    nrs <- map show <$> replicateM 14 (randomRIO (0,9) :: IO Int)
    let phone = Brig.parsePhone . cs $ "+0" ++ concat nrs
    return $ fromMaybe (error "Invalid random phone#") phone

randomUser :: (HasCallStack, MonadCatch m, MonadIO m, MonadHttp m) => BrigReq -> m Brig.User
randomUser brig_ = do
    n <- cs . UUID.toString <$> liftIO UUID.nextRandom
    createUser n brig_

createUser :: (HasCallStack, MonadCatch m, MonadIO m, MonadHttp m)
           => ST -> BrigReq -> m Brig.User
createUser name brig_ = do
    r <- postUser name True Nothing Nothing brig_ <!! const 201 === statusCode
    return $ decodeBody' r

-- more flexible variant of 'createUser' (see above).  (check the variant that brig has before you
-- clone this again!)
postUser :: (HasCallStack, MonadIO m, MonadHttp m)
         => ST -> Bool -> Maybe Brig.UserSSOId -> Maybe TeamId -> BrigReq -> m ResponseLBS
postUser name haveEmail ssoid teamid brig_ = do
    email <- if haveEmail then Just <$> randomEmail else pure Nothing
    let p = RequestBodyLBS . Aeson.encode $ object
            [ "name"            .= name
            , "email"           .= email
            , "password"        .= defPassword
            , "cookie"          .= defCookieLabel
            , "sso_id"          .= ssoid
            , "team_id"         .= teamid
            ]
    post (brig_ . path "/i/users" . contentJson . body p)

defPassword :: PlainTextPassword
defPassword = PlainTextPassword "secret"

defCookieLabel :: Brig.CookieLabel
defCookieLabel = Brig.CookieLabel "auth"

getActivationCode :: (HasCallStack, MonadIO m, MonadHttp m)
                  => BrigReq -> Either Brig.Email Brig.Phone -> m (Maybe (Brig.ActivationKey, Brig.ActivationCode))
getActivationCode brig_ ep = do
    let qry = either (queryItem "email" . toByteString') (queryItem "phone" . toByteString') ep
    r <- get $ brig_ . path "/i/users/activation-code" . qry
    let lbs   = fromMaybe "" $ responseBody r
    let akey  = Brig.ActivationKey  . Ascii.unsafeFromText <$> (lbs ^? Aeson.key "key"  . Aeson._String)
    let acode = Brig.ActivationCode . Ascii.unsafeFromText <$> (lbs ^? Aeson.key "code" . Aeson._String)
    return $ (,) <$> akey <*> acode

activate :: (HasCallStack, MonadIO m, MonadHttp m)
         => BrigReq -> Brig.ActivationPair -> m ResponseLBS
activate brig_ (k, c) = get $ brig_
    . path "activate"
    . queryItem "key" (toByteString' k)
    . queryItem "code" (toByteString' c)

zUser :: UserId -> Request -> Request
zUser = header "Z-User" . toByteString'

zConn :: SBS -> Request -> Request
zConn = header "Z-Connection"


endpointToReq :: Endpoint -> (Bilge.Request -> Bilge.Request)
endpointToReq ep = Bilge.host (ep ^. epHost . to cs) . Bilge.port (ep ^. epPort)

endpointToSettings :: Endpoint -> Warp.Settings
endpointToSettings endpoint = Warp.defaultSettings
  { Warp.settingsHost = Imports.fromString . cs $ endpoint ^. epHost
  , Warp.settingsPort = fromIntegral $ endpoint ^. epPort
  }

endpointToURL :: MonadIO m => Endpoint -> ST -> m URI
endpointToURL endpoint urlpath = either err pure url
  where
    url     = parseURI' ("http://" <> urlhost <> ":" <> urlport) <&> (=/ urlpath)
    urlhost = cs $ endpoint ^. epHost
    urlport = cs . show $ endpoint ^. epPort
    err     = liftIO . throwIO . ErrorCall . show . (, (endpoint, url))


-- spar specifics

shouldRespondWith :: forall a. (HasCallStack, Show a, Eq a)
                  => Http a -> (a -> Bool) -> TestSpar ()
shouldRespondWith action proper = do
  resp <- call action
  liftIO $ resp `shouldSatisfy` proper

-- I tried this, but i don't  think it's worth the learning effort.  Perhaps it'll be helpful as a comment here.  :-)
-- envit :: Example (r -> m a) => String -> ReaderT r m a -> SpecWith (Arg (r -> m a))
-- envit msg action = it msg $ \env -> action `runReaderT` env

call :: (MonadIO m, MonadReader TestEnv m) => Http a -> m a
call req = ask >>= \env -> liftIO $ runHttpT (env ^. teMgr) req

ping :: (Request -> Request) -> Http ()
ping req = void . get $ req . path "/i/status" . expect2xx


makeIssuer :: MonadIO m => m Issuer
makeIssuer = do
  uuid <- liftIO UUID.nextRandom
  either (liftIO . throwIO . ErrorCall . show)
         (pure . Issuer)
         (SAML.parseURI' ("https://issuer.net/_" <> UUID.toText uuid))

-- | Create a cloned new 'IdPMetadata' value from the sample value already registered, but with a
-- fresh random 'Issuer'.  This is the simplest way to get such a value for registration of a new
-- 'IdP' (registering the same 'Issuer' twice is an error).
makeTestIdPMetadata :: (HasCallStack, MonadIO m) => m IdPMetadata
makeTestIdPMetadata = do
  issuer <- makeIssuer
  pure $ sampleIdPMetadata issuer [uri|https://requri.net/|]


getTestSPMetadata :: (HasCallStack, MonadReader TestEnv m, MonadIO m) => m SPMetadata
getTestSPMetadata = do
  env  <- ask
  resp <- call . get $ (env ^. teSpar) . path "/sso/metadata" . expect2xx
  raw  <- maybe (crash_ "no body") (pure . cs) $ responseBody resp
  either (crash_ . show) pure (SAML.decode raw)
  where
    crash_ = liftIO . throwIO . ErrorCall


-- | Create a fresh 'IdPMetadata' suitable for testing.  Call 'createUserWithTeam' and create the
-- idp in the resulting team.  The user returned is the owner of the team.
registerTestIdP :: (HasCallStack, MonadIO m, MonadReader TestEnv m)
              => m (UserId, TeamId, IdP)
registerTestIdP = do
  idpmeta <- makeTestIdPMetadata
  env <- ask
  registerTestIdPFrom idpmeta (env ^. teMgr) (env ^. teBrig) (env ^. teGalley) (env ^. teSpar)

-- | Helper for 'registerTestIdP'.
registerTestIdPFrom :: (HasCallStack, MonadIO m)
                  => IdPMetadata -> Manager -> BrigReq -> GalleyReq -> SparReq -> m (UserId, TeamId, IdP)
registerTestIdPFrom metadata mgr brig galley spar = do
  liftIO . runHttpT mgr $ do
    (uid, tid) <- createUserWithTeam brig galley
    (uid, tid,) <$> callIdpCreate spar (Just uid) metadata


getCookie :: KnownSymbol name => proxy name -> ResponseLBS -> Either String (SAML.SimpleSetCookie name)
getCookie proxy rsp = do
  web :: Web.SetCookie
    <- Web.parseSetCookie <$> maybe (Left "no set-cookie header") Right
                                    (lookup "set-cookie" (responseHeaders rsp))
  if Web.setCookieName web == SAML.cookieName proxy
    then Right $ SimpleSetCookie web
    else Left $ "bad cookie name.  (found, expected) == " <> show (Web.setCookieName web, SAML.cookieName proxy)

-- | In 'setResponseCookie' we set an expiration date iff cookie is persistent.  So here we test for
-- expiration date.  Easier than parsing and inspecting the cookie value.
hasPersistentCookieHeader :: ResponseLBS -> Either String ()
hasPersistentCookieHeader rsp = do
  cky <- getCookie (Proxy @"zuid") rsp
  when (isNothing . Web.setCookieExpires $ fromSimpleSetCookie cky) $
    Left $ "expiration date should NOT empty: " <> show cky

-- | A bind cookie is always sent, but if we do not want to send one, it looks like this:
-- "wire.com=; Path=/sso/finalize-login; Expires=Thu, 01-Jan-1970 00:00:00 GMT; Max-Age=-1; Secure"
hasDeleteBindCookieHeader :: HasCallStack => ResponseLBS -> Either String ()
hasDeleteBindCookieHeader rsp = isDeleteBindCookie =<< getCookie (Proxy @"zbind") rsp

isDeleteBindCookie :: HasCallStack => SetBindCookie -> Either String ()
isDeleteBindCookie (SimpleSetCookie cky) =
  if (SAML.Time <$> Web.setCookieExpires cky) == Just (SAML.unsafeReadTime "1970-01-01T00:00:00Z")
  then Right ()
  else Left $ "expiration should be empty: " <> show cky

hasSetBindCookieHeader :: HasCallStack => ResponseLBS -> Either String ()
hasSetBindCookieHeader rsp = isSetBindCookie =<< getCookie (Proxy @"zbind") rsp

isSetBindCookie :: HasCallStack => SetBindCookie -> Either String ()
isSetBindCookie (SimpleSetCookie cky) = do
  unless (Web.setCookieName cky == "zbind") $ do
    Left $ "expected zbind cookie: " <> show cky

  unless (maybe False ("/sso/finalize-login" `SBS.isPrefixOf`) $ Web.setCookiePath cky) $ do
    Left $ "expected path prefix /sso/finalize-login: " <> show cky

  unless (Web.setCookieSecure cky) $ do
    Left $ "cookie must be secure: " <> show cky

  unless (Web.setCookieSameSite cky == Just Web.sameSiteStrict) $ do
    Left $ "cookie must be same-site: " <> show cky


-- | see also: 'callAuthnReq'
negotiateAuthnRequest :: (HasCallStack, MonadIO m, MonadReader TestEnv m)
                      => IdP -> m (SAML.SignPrivCreds, SAML.AuthnRequest)
negotiateAuthnRequest idp = negotiateAuthnRequest' DoInitiateLogin idp id >>= \case
  (creds, req, cky) -> case maybe (Left "missing") isDeleteBindCookie cky of
    Right () -> pure (creds, req)
    Left msg -> error $ "unexpected bind cookie: " <> show (cky, msg)

doInitiatePath :: DoInitiate -> [ST]
doInitiatePath DoInitiateLogin = ["sso", "initiate-login"]
doInitiatePath DoInitiateBind  = ["sso-initiate-bind"]

negotiateAuthnRequest'
  :: (HasCallStack, MonadIO m, MonadReader TestEnv m)
  => DoInitiate -> IdP -> (Request -> Request) -> m (SAML.SignPrivCreds, SAML.AuthnRequest, Maybe SetBindCookie)
negotiateAuthnRequest' (doInitiatePath -> doInit) idp modreq = do
  env <- ask
  resp :: ResponseLBS
    <- call $ get
           ( modreq
           . (env ^. teSpar)
           . paths (cs <$> (doInit <> [idPIdToST $ idp ^. SAML.idpId]))
           . expect2xx
           )
  (_, authnreq) <- either error pure . parseAuthnReqResp $ cs <$> responseBody resp
  let wireCookie = SAML.SimpleSetCookie . Web.parseSetCookie
                     <$> lookup "Set-Cookie" (responseHeaders resp)
  pure (sampleIdPPrivkey, authnreq, wireCookie)


submitAuthnResponse :: (HasCallStack, MonadIO m, MonadReader TestEnv m)
                    => SignedAuthnResponse -> m ResponseLBS
submitAuthnResponse = submitAuthnResponse' id

submitAuthnResponse' :: (HasCallStack, MonadIO m, MonadReader TestEnv m)
                    => (Request -> Request) -> SignedAuthnResponse -> m ResponseLBS
submitAuthnResponse' reqmod (SignedAuthnResponse authnresp) = do
  env <- ask
  req :: Request
    <- formDataBody [partLBS "SAMLResponse" . EL.encode . XML.renderLBS XML.def $ authnresp] empty
  call $ post' req (reqmod . (env ^. teSpar) . path "/sso/finalize-login/")


loginSsoUserFirstTime :: (HasCallStack, MonadIO m, MonadReader TestEnv m) => IdP -> m UserId
loginSsoUserFirstTime idp = do
  env <- ask
  (privCreds, authnReq) <- negotiateAuthnRequest idp
  spmeta <- getTestSPMetadata
  authnResp <- runSimpleSP $ mkAuthnResponse privCreds idp spmeta authnReq True
  sparAuthnResp <- submitAuthnResponse authnResp
  let wireCookie = maybe (error "no wire cookie") id . lookup "Set-Cookie" $ responseHeaders sparAuthnResp

  accessResp :: ResponseLBS <- call $
    post ((env ^. teBrig) . path "/access" . header "Cookie" wireCookie . expect2xx)

  let uid :: UserId
      uid = Id . fromMaybe (error "bad user field in /access response body") . UUID.fromText $ uidRaw

      uidRaw :: HasCallStack => ST
      uidRaw = accessToken ^?! Aeson.key "user" . _String

      accessToken :: HasCallStack => Aeson.Value
      accessToken = tok
        where
          tok = either (error . ("parse error in /access response body: " <>)) id $
            Aeson.eitherDecode raw
          raw = fromMaybe (error "no body in /access response") $
            responseBody accessResp

  pure uid


-- TODO: move this to /lib/bilge?
responseJSON :: FromJSON a => ResponseLBS -> Either String a
responseJSON = fmapL show . Aeson.eitherDecode <=< maybe (Left "no body") pure . responseBody

callAuthnReq :: forall m. (HasCallStack, MonadIO m, MonadHttp m)
             => SparReq -> SAML.IdPId -> m (URI, SAML.AuthnRequest)
callAuthnReq sparreq_ idpid = assert test_parseAuthnReqResp $ do
  resp <- callAuthnReq' (sparreq_ . expect2xx) idpid
  either (err resp) pure $ parseAuthnReqResp (cs <$> responseBody resp)
  where
    err :: forall n a. MonadIO n => ResponseLBS -> String -> n a
    err resp = liftIO . throwIO . ErrorCall . (<> ("; " <> show (responseBody resp)))

test_parseAuthnReqResp :: Bool
test_parseAuthnReqResp = isRight tst1
  where
    tst1 = parseAuthnReqResp @(Either String) (Just raw)
    _tst2 = XML.parseText XML.def raw
    raw = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\"><html xml:lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\"><body onload=\"document.forms[0].submit()\"><noscript><p><strong>Note:</strong>Since your browser does not support JavaScript, you must press the Continue button once to proceed.</p></noscript><form action=\"http://idp.net/sso/request\" method=\"post\"><input name=\"SAMLRequest\" type=\"hidden\" value=\"PHNhbWxwOkF1dGhuUmVxdWVzdCB4bWxuczpzYW1sYT0idXJuOm9hc2lzOm5hbWVzOnRjOlNBTUw6Mi4wOmFzc2VydGlvbiIgeG1sbnM6c2FtbG09InVybjpvYXNpczpuYW1lczp0YzpTQU1MOjIuMDptZXRhZGF0YSIgeG1sbnM6ZHM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvMDkveG1sZHNpZyMiIElEPSJpZGVhMDUwZmM0YzBkODQxNzJiODcwMjIzMmNlZmJiMGE3IiBJc3N1ZUluc3RhbnQ9IjIwMTgtMDctMDJUMTk6Mzk6MDYuNDQ3OTg3MVoiIFZlcnNpb249IjIuMCIgeG1sbnM6c2FtbHA9InVybjpvYXNpczpuYW1lczp0YzpTQU1MOjIuMDpwcm90b2NvbCI+PElzc3VlciB4bWxucz0idXJuOm9hc2lzOm5hbWVzOnRjOlNBTUw6Mi4wOmFzc2VydGlvbiI+aHR0cHM6Ly9hcHAud2lyZS5jb20vPC9Jc3N1ZXI+PC9zYW1scDpBdXRoblJlcXVlc3Q+\"/><noscript><input type=\"submit\" value=\"Continue\"/></noscript></form></body></html>"

parseAuthnReqResp :: forall n. MonadError String n
          => Maybe LT -> n (URI, SAML.AuthnRequest)
parseAuthnReqResp Nothing = throwError "no response body"
parseAuthnReqResp (Just raw) = do
  xml :: XML.Document
    <- either (throwError . ("malformed html in response body: " <>) . show) pure
     $ XML.parseText XML.def raw
  reqUri  :: URI
    <- safeHead "form" (XML.fromDocument xml XML.$// XML.element (XML.Name "form" (Just "http://www.w3.org/1999/xhtml") Nothing))
       >>= safeHead "action" . XML.attribute "action"
       >>= SAML.parseURI'
  reqBody :: SAML.AuthnRequest
    <- safeHead "input" (XML.fromDocument xml XML.$// XML.element (XML.Name "input" (Just "http://www.w3.org/1999/xhtml") Nothing))
       >>= safeHead "value" . XML.attribute "value"
       >>= either (throwError . show) pure . EL.decode . cs
       >>= either (throwError . show) pure . SAML.decodeElem . cs
  pure (reqUri, reqBody)

safeHead :: forall n a. (MonadError String n, Show a) => String -> [a] -> n a
safeHead _   (a:_) = pure a
safeHead msg []    = throwError $ msg <> ": []"

callAuthnReq' :: (MonadIO m, MonadHttp m) => SparReq -> SAML.IdPId -> m ResponseLBS
callAuthnReq' sparreq_ idpid = do
  get $ sparreq_ . path (cs $ "/sso/initiate-login/" -/ SAML.idPIdToST idpid)

callAuthnReqPrecheck' :: (MonadIO m, MonadHttp m) => SparReq -> SAML.IdPId -> m ResponseLBS
callAuthnReqPrecheck' sparreq_ idpid = do
  head $ sparreq_ . path (cs $ "/sso/initiate-login/" -/ SAML.idPIdToST idpid)

callIdpGet :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> SAML.IdPId -> m IdP
callIdpGet sparreq_ muid idpid = do
  resp <- callIdpGet' (sparreq_ . expect2xx) muid idpid
  either (liftIO . throwIO . ErrorCall . show) pure
    $ responseJSON @IdP resp

callIdpGet' :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> SAML.IdPId -> m ResponseLBS
callIdpGet' sparreq_ muid idpid = do
  get $ sparreq_ . maybe id zUser muid . path (cs $ "/identity-providers/" -/ SAML.idPIdToST idpid)

callIdpGetAll :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> m IdPList
callIdpGetAll sparreq_ muid = do
  resp <- callIdpGetAll' (sparreq_ . expect2xx) muid
  either (liftIO . throwIO . ErrorCall . show) pure
    $ responseJSON resp

callIdpGetAll' :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> m ResponseLBS
callIdpGetAll' sparreq_ muid = do
  get $ sparreq_ . maybe id zUser muid . path "/identity-providers"

callIdpCreate :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> SAML.IdPMetadata -> m IdP
callIdpCreate sparreq_ muid metadata = do
  resp <- callIdpCreate' (sparreq_ . expect2xx) muid metadata
  either (liftIO . throwIO . ErrorCall . show) pure
    $ responseJSON @IdP resp

callIdpCreate' :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> SAML.IdPMetadata -> m ResponseLBS
callIdpCreate' sparreq_ muid metadata = do
  post $ sparreq_
    . maybe id zUser muid
    . path "/identity-providers/"
    . body (RequestBodyLBS . cs $ SAML.encode metadata)
    . header "Content-Type" "application/xml"

callIdpDelete :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> SAML.IdPId -> m ()
callIdpDelete sparreq_ muid idpid = void $ callIdpDelete' (sparreq_ . expect2xx) muid idpid

callIdpDelete' :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> SAML.IdPId -> m ResponseLBS
callIdpDelete' sparreq_ muid idpid = do
  delete $ sparreq_ . maybe id zUser muid . path (cs $ "/identity-providers/" -/ SAML.idPIdToST idpid)


-- helpers talking to spar's cassandra directly

-- | Look up 'UserId' under 'UserSSOId' on spar's cassandra directly.
ssoToUidSpar :: (HasCallStack, MonadIO m, MonadReader TestEnv m) => Brig.UserSSOId -> m (Maybe UserId)
ssoToUidSpar ssoid = do
  ssoref <- either (error . ("could not parse UserRef: " <>)) pure $ Intra.fromUserSSOId ssoid
  runSparCass @Client $ Data.getUser ssoref

runSparCass
  :: (HasCallStack, m ~ Client, MonadIO m', MonadReader TestEnv m')
  => m a -> m' a
runSparCass action = do
  env <- ask
  liftIO $ runClient (env ^. teCql) action

runSparCassWithEnv
  :: ( HasCallStack
     , m ~ ReaderT Data.Env (ExceptT TTLError Cas.Client)
     , MonadIO m', MonadReader TestEnv m'
     )
  => m a -> m' a
runSparCassWithEnv action = do
  env  <- ask
  denv <- Data.mkEnv <$> (pure $ env ^. teOpts) <*> liftIO getCurrentTime
  val  <- runSparCass (runExceptT (action `runReaderT` denv))
  either (liftIO . throwIO . ErrorCall . show) pure val

runSimpleSP :: (MonadReader TestEnv m, MonadIO m) => SAML.SimpleSP a -> m a
runSimpleSP action = do
  env    <- ask
  liftIO $ do
    ctx    <- SAML.mkSimpleSPCtx (env ^. teOpts . to saml) []
    result <- SAML.runSimpleSP ctx action
    either (throwIO . ErrorCall . show) pure result

runSpar :: (MonadReader TestEnv m, MonadIO m) => Spar.Spar a -> m a
runSpar (Spar.Spar action) = do
  env    <- (^. teSparEnv) <$> ask
  liftIO $ do
    result <- runExceptT $ action `runReaderT` env
    either (throwIO . ErrorCall . show) pure result


getSsoidViaSelf :: HasCallStack => UserId -> TestSpar UserSSOId
getSsoidViaSelf uid = maybe (error "not found") pure =<< getSsoidViaSelf' uid

getSsoidViaSelf' :: HasCallStack => UserId -> TestSpar (Maybe UserSSOId)
getSsoidViaSelf' uid = do
  let probe :: HasCallStack => TestSpar (Maybe UserSSOId)
      probe = do
        env <- ask
        fmap getUserSSOId . call . get $
          ( (env ^. teBrig)
          . header "Z-User" (toByteString' uid)
          . path "/self"
          . expect2xx
          )

      getUserSSOId :: HasCallStack => ResponseLBS -> Maybe UserSSOId
      getUserSSOId (fmap Aeson.eitherDecode . responseBody -> Just (Right selfprof))
        = case userIdentity $ selfUser selfprof of
            Just (SSOIdentity ssoid _ _) -> Just ssoid
            Just (FullIdentity _ _)  -> Nothing
            Just (EmailIdentity _)   -> Nothing
            Just (PhoneIdentity _)   -> Nothing
            Nothing                  -> Nothing
      getUserSSOId _ = Nothing

  env <- ask
  liftIO $ retrying
    (exponentialBackoff 50 <> limitRetries 5)
    (\_ -> pure . isNothing)
    (\_ -> probe `runReaderT` env)

getUserIdViaRef :: HasCallStack => UserRef -> TestSpar UserId
getUserIdViaRef uref = maybe (error "not found") pure =<< getUserIdViaRef' uref

getUserIdViaRef' :: HasCallStack => UserRef -> TestSpar (Maybe UserId)
getUserIdViaRef' uref = do
  env <- ask
  liftIO $ retrying
    (exponentialBackoff 50 <> limitRetries 5)
    (\_ -> pure . isNothing)
    (\_ -> runClient (env ^. teCql) $ Data.getUser uref)
