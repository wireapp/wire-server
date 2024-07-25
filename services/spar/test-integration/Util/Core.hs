{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
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

-- | Two (weak) reasons why I implemented the clients without the help of servant-client: (1) I
-- wanted smooth integration in 'HttpMonad'; (2) I wanted the choice of receiving the unparsed
-- 'ResponseLBS' rather than the parsed result (or a hard-to examine error).  this is important for
-- testing for expected failures.  See also: https://github.com/haskell-servant/servant/issues/1004
--
-- FUTUREWORK: this is all copied from /services/galley/test/integration/API/Util.hs and some other
-- places; should we make this a new library?  (@tiago-loureiro says no that's fine.)
module Util.Core
  ( -- * Test environment
    mkEnvFromOptions,
    mkEnv,
    destroyEnv,

    -- * Test helpers
    it,
    pending,
    pendingWith,
    shouldRespondWith,
    module Test.Hspec,
    aFewTimes,
    aFewTimesAssert,
    aFewTimesRecover,

    -- * HTTP
    call,
    endpointToReq,

    -- * Other
    randomEmail,
    defPassword,
    getUserBrig,
    changeHandleBrig,
    setRandomHandleBrig,
    updateProfileBrig,
    createUserWithTeam,
    createUserWithTeamDisableSSO,
    putSSOEnabledInternal,
    inviteAndRegisterUser,
    createTeamMember,
    deleteUserOnBrig,
    getTeams,
    getTeamMemberIds,
    getTeamMembers,
    promoteTeamMember,
    getSelfProfile,
    nextWireId,
    nextWireIdP,
    nextHandle,
    nextSAMLID,
    nextSubject,
    nextUserRef,
    zUser,
    zConn,
    ping,
    makeTestIdP,
    getTestSPMetadata,
    registerTestIdP,
    registerTestIdPWithMeta,
    registerTestIdPFrom,
    tryLogin,
    tryLoginFail,
    negotiateAuthnRequest,
    negotiateAuthnRequest',
    getCookie,
    hasPersistentCookieHeader,
    submitAuthnResponse,
    submitAuthnResponse',
    loginSsoUserFirstTime,
    loginSsoUserFirstTime',
    loginCreatedSsoUser,
    callAuthnReq,
    callAuthnReq',
    callIdpGet,
    callIdpGet',
    callIdpGetRaw,
    callIdpGetRaw',
    callIdpGetAll,
    callIdpGetAll',
    callIdpCreate,
    callIdpCreate',
    callIdpCreateRaw,
    callIdpCreateRaw',
    callIdpCreateReplace,
    callIdpCreateReplace',
    callIdpCreateWithHandle,
    callIdpUpdate',
    callIdpUpdate,
    callIdpUpdateWithHandle,
    callIdpDelete,
    callIdpDelete',
    callIdpDeletePurge',
    initCassandra,
    ssoToUidSpar,
    runSimpleSP,
    runSpar,
    runSparE,
    type CanonicalEffs,
    getSsoidViaSelf,
    getSsoidViaSelf',
    getUserIdViaRef,
    getUserIdViaRef',
    callGetDefaultSsoCode,
    callSetDefaultSsoCode,
    callDeleteDefaultSsoCode,
    checkErr,
    checkErrHspec,
    updateTeamMemberRole,
    checkChangeRoleOfTeamMember,
    eventually,
    getIdPByIssuer,
    retryNUntil,
    randomUser,
  )
where

import Bilge hiding (getCookie, host, port) -- we use Web.Cookie instead of the http-client type
import qualified Bilge
import Bilge.Assert (Assertions, (!!!), (<!!), (===))
import Cassandra as Cas
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.Except
import Control.Retry
import Crypto.Random.Types (MonadRandom)
import Data.Aeson as Aeson hiding (json)
import Data.Aeson.Lens as Aeson
import qualified Data.ByteString.Base64.Lazy as EL
import Data.ByteString.Conversion
import Data.Handle (Handle, parseHandle)
import Data.Id
import Data.Misc (PlainTextPassword6, plainTextPassword6Unsafe)
import Data.Proxy
import Data.Range
import Data.String.Conversions
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy.Encoding as LT
import Data.UUID as UUID hiding (fromByteString, null)
import Data.UUID.V4 as UUID (nextRandom)
import qualified Data.Yaml as Yaml
import GHC.TypeLits
import Imports hiding (head)
import Network.HTTP.Client.MultipartFormData
import qualified Options.Applicative as OPA
import Polysemy (Sem)
import SAML2.WebSSO as SAML hiding ((<$$>))
import qualified SAML2.WebSSO.API.Example as SAML
import SAML2.WebSSO.Test.Lenses (userRefL)
import SAML2.WebSSO.Test.MockResponse
import SAML2.WebSSO.Test.Util (SampleIdP (..), makeSampleIdPMetadata)
import qualified Spar.App as IdpConfigStire
import qualified Spar.App as Spar
import Spar.CanonicalInterpreter
import Spar.Error (SparError)
import qualified Spar.Intra.BrigApp as Intra
import Spar.Options
import Spar.Run
import qualified Spar.Sem.IdPConfigStore as IdPConfigStore
import qualified Spar.Sem.SAMLUserStore as SAMLUserStore
import qualified Spar.Sem.ScimExternalIdStore as ScimExternalIdStore
import qualified System.Logger.Extended as Log
import System.Random (randomRIO)
import Test.Hspec hiding (it, pending, pendingWith, xit)
import qualified Test.Hspec
import qualified Text.XML as XML
import qualified Text.XML.Cursor as XML
import Text.XML.DSig (SignPrivCreds)
import qualified Text.XML.DSig as SAML
import URI.ByteString as URI
import Util.Options
import Util.Types
import qualified Web.Cookie as Web
import Wire.API.Team (Icon (..))
import qualified Wire.API.Team as Galley
import Wire.API.Team.Feature (FeatureStatus (..), FeatureTTL' (..), FeatureTrivialConfig (trivialConfig), SSOConfig, WithStatusNoLock (WithStatusNoLock))
import qualified Wire.API.Team.Invitation as TeamInvitation
import Wire.API.Team.Member (NewTeamMember, TeamMemberList, rolePermissions)
import qualified Wire.API.Team.Member as Member
import qualified Wire.API.Team.Member as Team
import Wire.API.Team.Permission
import Wire.API.Team.Role
import qualified Wire.API.Team.Role as Role
import Wire.API.User
import qualified Wire.API.User as User
import Wire.API.User.Auth hiding (Cookie)
import Wire.API.User.IdentityProvider
import Wire.API.User.Scim (runValidExternalIdEither)
import Wire.Sem.Logger.TinyLog

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
cliOptsParser =
  (,)
    <$> OPA.strOption
      ( OPA.long "integration-config"
          <> OPA.short 'i'
          <> OPA.help "Integration config to load"
          <> OPA.showDefault
          <> OPA.value defaultIntPath
      )
    <*> OPA.strOption
      ( OPA.long "service-config"
          <> OPA.short 's'
          <> OPA.help "Spar application config to load"
          <> OPA.showDefault
          <> OPA.value defaultSparPath
      )
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
mkEnv :: (HasCallStack) => IntegrationConfig -> Opts -> IO TestEnv
mkEnv tstOpts opts = do
  mgr :: Manager <- newManager defaultManagerSettings
  sparCtxLogger <- Log.mkLogger (samlToLevel $ saml opts ^. SAML.cfgLogLevel) (logNetStrings opts) (logFormat opts)
  cql :: ClientState <- initCassandra opts sparCtxLogger
  let brig = endpointToReq tstOpts.brig
      galley = endpointToReq tstOpts.galley
      spar = endpointToReq tstOpts.spar
      sparEnv = Spar.Env {..}
      wireIdPAPIVersion = WireIdPAPIV2
      sparCtxOpts = opts
      sparCtxCas = cql
      sparCtxHttpManager = mgr
      sparCtxHttpBrig = brig empty
      sparCtxHttpGalley = galley empty
      sparCtxRequestId = RequestId "<fake request id>"
  pure $
    TestEnv
      mgr
      cql
      brig
      galley
      spar
      sparEnv
      opts
      tstOpts
      wireIdPAPIVersion

destroyEnv :: (HasCallStack) => TestEnv -> IO ()
destroyEnv _ = pure ()

it ::
  (HasCallStack) =>
  -- or, more generally:
  -- MonadIO m, Example (TestEnv -> m ()), Arg (TestEnv -> m ()) ~ TestEnv
  String ->
  TestSpar () ->
  SpecWith TestEnv
it msg bdy = Test.Hspec.it msg $ runReaderT bdy

pending :: (HasCallStack, MonadIO m) => m ()
pending = liftIO Test.Hspec.pending

pendingWith :: (HasCallStack, MonadIO m) => String -> m ()
pendingWith = liftIO . Test.Hspec.pendingWith

-- | Run a probe several times, until a "good" value materializes or until patience runs out
-- (after ~2secs).
-- If all retries were unsuccessful, 'aFewTimes' will return the last obtained value, even
-- if it does not satisfy the predicate.
aFewTimes :: TestSpar a -> (a -> Bool) -> TestSpar a
aFewTimes action good = do
  env <- ask
  liftIO $
    retrying
      (exponentialBackoff 1000 <> limitRetries 11)
      (\_ -> pure . not . good)
      (\_ -> action `runReaderT` env)

retryNUntil :: (MonadIO m) => Int -> (a -> Bool) -> m a -> m a
retryNUntil n good m =
  retrying
    (constantDelay 1000000 <> limitRetries n)
    (const (pure . not . good))
    (const m)

aFewTimesAssert :: (HasCallStack) => TestSpar a -> (a -> Bool) -> TestSpar ()
aFewTimesAssert action good = do
  result <- aFewTimes action good
  good result `assert` pure ()

aFewTimesRecover :: TestSpar a -> TestSpar a
aFewTimesRecover action = do
  env <- ask
  liftIO $
    recoverAll
      (exponentialBackoff 1000 <> limitRetries 10)
      (\_ -> action `runReaderT` env)

-- | Duplicate of 'Spar.Intra.getBrigUser'.
getUserBrig :: (HasCallStack) => UserId -> TestSpar (Maybe User)
getUserBrig uid = do
  env <- ask
  let req =
        (env ^. teBrig)
          . path "/self"
          . header "Z-User" (toByteString' uid)
  resp <- call $ get req
  case statusCode resp of
    200 -> do
      let user = selfUser $ responseJsonUnsafe resp
      pure $
        if userDeleted user
          then Nothing
          else Just user
    404 -> pure Nothing
    bad -> error $ show bad

createUserWithTeam :: (HasCallStack, MonadHttp m, MonadIO m, MonadFail m) => BrigReq -> GalleyReq -> m (UserId, TeamId)
createUserWithTeam brg gly = do
  (uid, tid) <- createUserWithTeamDisableSSO brg gly
  putSSOEnabledInternal gly tid FeatureStatusEnabled
  pure (uid, tid)

createUserWithTeamDisableSSO :: (HasCallStack, MonadHttp m, MonadIO m, MonadFail m) => BrigReq -> GalleyReq -> m (UserId, TeamId)
createUserWithTeamDisableSSO brg gly = do
  e <- randomEmail
  n <- UUID.toString <$> liftIO UUID.nextRandom
  let p =
        RequestBodyLBS . Aeson.encode $
          object
            [ "name" .= n,
              "email" .= fromEmail e,
              "password" .= defPassword,
              "team" .= newTeam
            ]
  bdy <- selfUser . responseJsonUnsafe <$> post (brg . path "/i/users" . contentJson . body p)
  let (uid, Just tid) = (userId bdy, userTeam bdy)
  (team' : _) <- (^. Galley.teamListTeams) <$> getTeams uid gly
  () <-
    Control.Exception.assert {- "Team ID in registration and team table do not match" -} (tid == team' ^. Galley.teamId) $
      pure ()
  selfTeam <- userTeam . selfUser <$> getSelfProfile brg uid
  () <-
    Control.Exception.assert {- "Team ID in self profile and team table do not match" -} (selfTeam == Just tid) $
      pure ()
  pure (uid, tid)

putSSOEnabledInternal :: (HasCallStack, MonadHttp m, MonadIO m) => GalleyReq -> TeamId -> FeatureStatus -> m ()
putSSOEnabledInternal gly tid enabled = do
  void . put $
    gly
      . paths ["i", "teams", toByteString' tid, "features", "sso"]
      . json (WithStatusNoLock @SSOConfig enabled trivialConfig FeatureTTLUnlimited)
      . expect2xx

-- | cloned from `/services/brig/test/integration/API/Team/Util.hs`.
inviteAndRegisterUser ::
  (MonadIO m, MonadCatch m, MonadFail m, MonadHttp m, HasCallStack) =>
  BrigReq ->
  UserId ->
  TeamId ->
  Email ->
  m User
inviteAndRegisterUser brig u tid inviteeEmail = do
  let invite = stdInvitationRequest inviteeEmail
  inv <- responseJsonError =<< postInvitation tid u invite
  Just inviteeCode <- getInvitationCode tid (TeamInvitation.inInvitation inv)
  rspInvitee <-
    post
      ( brig
          . path "/register"
          . contentJson
          . body (accept' inviteeEmail inviteeCode)
      )
      <!! const 201
        === statusCode
  let Just invitee = responseJsonMaybe rspInvitee
  unless (Just tid == userTeam invitee) $ error "Team ID in registration and team table do not match"
  selfTeam <- userTeam . selfUser <$> getSelfProfile brig (userId invitee)
  unless (selfTeam == Just tid) $ error "Team ID in self profile and team table do not match"
  pure invitee
  where
    accept' :: User.Email -> User.InvitationCode -> RequestBody
    accept' email code = acceptWithName (User.Name "Bob") email code
    --
    acceptWithName :: User.Name -> User.Email -> User.InvitationCode -> RequestBody
    acceptWithName name email code =
      RequestBodyLBS . Aeson.encode $
        object
          [ "name" .= User.fromName name,
            "email" .= email,
            "password" .= defPassword,
            "team_code" .= code
          ]
    --
    postInvitation ::
      (MonadHttp m, HasCallStack) =>
      TeamId ->
      UserId ->
      TeamInvitation.InvitationRequest ->
      m ResponseLBS
    postInvitation t u' i =
      post $
        brig
          . paths ["teams", toByteString' t, "invitations"]
          . contentJson
          . body (RequestBodyLBS $ Aeson.encode i)
          . zAuthAccess u' "conn"
    --
    getInvitationCode ::
      (MonadIO m, MonadHttp m, HasCallStack) =>
      TeamId ->
      InvitationId ->
      m (Maybe User.InvitationCode)
    getInvitationCode t ref = do
      r <-
        get
          ( brig
              . path "/i/teams/invitation-code"
              . queryItem "team" (toByteString' t)
              . queryItem "invitation_id" (toByteString' ref)
          )
      let lbs = fromMaybe "" $ responseBody r
      pure $ fromByteString (maybe (error "No code?") encodeUtf8 (lbs ^? key "code" . _String))

-- | NB: this does create an SSO UserRef on brig, but not on spar.  this is inconsistent, but the
-- inconsistency does not affect the tests we're running with this.  to resolve it, we could add an
-- internal end-point to spar that allows us to create users without idp response verification.
--
-- FUTUREWORK: same as 'addTeamMember'.  drop this and always use 'loginSsoUserFirstTime' or
-- 'inviteAndRegisterUser' instead!
createTeamMember ::
  (HasCallStack, MonadCatch m, MonadIO m, MonadHttp m) =>
  BrigReq ->
  GalleyReq ->
  TeamId ->
  Permissions ->
  m UserId
createTeamMember brigreq galleyreq teamid perms = do
  let randomtxt = liftIO $ UUID.toText <$> UUID.nextRandom
      randomssoid = liftIO $ UserSSOId <$> (mkSampleUref <$> rnd <*> rnd)
      rnd = cs . show <$> randomRIO (0 :: Integer, 10000000)
  name <- randomtxt
  ssoid <- randomssoid
  resp :: ResponseLBS <-
    postUser name False (Just ssoid) (Just teamid) brigreq
      <!! const 201 === statusCode
  let nobody :: UserId = userId (responseJsonUnsafe @User resp)
  addTeamMember galleyreq teamid (Member.mkNewTeamMember nobody perms Nothing)
  pure nobody

-- | FUTUREWORK(fisx): use the specified & supported flows for scaffolding; this is a hack
-- that builds up the internal structure from scratch, without too much thought.  For
-- instance, the team field in the user in brig won't be updated.
addTeamMember ::
  (HasCallStack, MonadCatch m, MonadHttp m) =>
  GalleyReq ->
  TeamId ->
  NewTeamMember ->
  m ()
addTeamMember galleyreq tid mem =
  void $
    post
      ( galleyreq
          . paths ["i", "teams", toByteString' tid, "members"]
          . contentJson
          . expect2xx
          . lbytes (Aeson.encode mem)
      )

-- | Delete a user from Brig and wait until it's gone.
deleteUserOnBrig ::
  (HasCallStack, MonadMask m, MonadIO m, MonadHttp m) =>
  BrigReq ->
  UserId ->
  m ()
deleteUserOnBrig brigreq uid = do
  deleteUserNoWait brigreq uid
  recoverAll (exponentialBackoff 500000 <> limitRetries 5) $ \_ -> do
    profile <- getSelfProfile brigreq uid
    liftIO $ selfUser profile `shouldSatisfy` userDeleted

-- | Delete a user from Brig but don't wait.
deleteUserNoWait ::
  (HasCallStack, MonadCatch m, MonadHttp m) =>
  BrigReq ->
  UserId ->
  m ()
deleteUserNoWait brigreq uid =
  void $
    delete
      ( brigreq
          . paths ["i", "users", toByteString' uid]
          . expect2xx
      )

-- | See also: 'nextSAMLID', 'nextUserRef'.  The names are chosed to be consistent with
-- 'UUID.nextRandom'.
nextWireId :: (MonadIO m) => m (Id a)
nextWireId = Id <$> liftIO UUID.nextRandom

nextWireIdP :: (MonadIO m) => WireIdPAPIVersion -> m WireIdP
nextWireIdP version = WireIdP <$> iid <*> pure (Just version) <*> pure [] <*> pure Nothing <*> idpHandle
  where
    iid = Id <$> liftIO UUID.nextRandom
    idpHandle = iid <&> IdPHandle . pack . show

nextSAMLID :: (MonadIO m) => m (ID a)
nextSAMLID = mkID . UUID.toText <$> liftIO UUID.nextRandom

nextHandle :: (MonadIO m) => m Handle
nextHandle = liftIO $ fromJust . parseHandle . cs . show <$> randomRIO (0 :: Int, 13371137)

-- | Generate a 'SAML.UserRef' subject.
nextSubject :: (HasCallStack, MonadIO m) => m NameID
nextSubject = liftIO $ do
  unameId <-
    randomRIO (0, 1 :: Int) >>= \case
      0 -> either (error . show) id . SAML.mkUNameIDEmail . fromEmail <$> randomEmail
      1 -> SAML.mkUNameIDUnspecified . UUID.toText <$> UUID.nextRandom
      _ -> error "nextSubject: impossible"
  either (error . show) pure $ SAML.mkNameID unameId Nothing Nothing Nothing

nextUserRef :: (MonadIO m) => m SAML.UserRef
nextUserRef = liftIO $ do
  tenant <- UUID.toText <$> UUID.nextRandom
  SAML.UserRef
    (SAML.Issuer $ SAML.unsafeParseURI ("http://" <> tenant))
    <$> nextSubject

getTeams :: (HasCallStack, MonadHttp m, MonadIO m) => UserId -> GalleyReq -> m Galley.TeamList
getTeams u gly = do
  r <-
    get
      ( gly
          . paths ["teams"]
          . zAuthAccess u "conn"
          . expect2xx
      )
  pure $ responseJsonUnsafe r

getTeamMemberIds :: (HasCallStack) => UserId -> TeamId -> TestSpar [UserId]
getTeamMemberIds usr tid = (^. Team.userId) <$$> getTeamMembers usr tid

getTeamMembers :: (HasCallStack) => UserId -> TeamId -> TestSpar [Member.TeamMember]
getTeamMembers usr tid = do
  gly <- view teGalley
  resp <-
    call $
      get (gly . paths ["teams", toByteString' tid, "members"] . zUser usr)
        <!! const 200 === statusCode
  let mems :: TeamMemberList
      Right mems = responseJsonEither resp
  pure $ mems ^. Team.teamMembers

promoteTeamMember :: (HasCallStack) => UserId -> TeamId -> UserId -> TestSpar ()
promoteTeamMember usr tid memid = do
  gly <- view teGalley
  let bdy :: NewTeamMember
      bdy = Member.mkNewTeamMember memid fullPermissions Nothing
  call $
    put (gly . paths ["teams", toByteString' tid, "members"] . zAuthAccess usr "conn" . json bdy)
      !!! const 200 === statusCode

getSelfProfile :: (HasCallStack, MonadHttp m, MonadIO m) => BrigReq -> UserId -> m SelfProfile
getSelfProfile brg usr = do
  rsp <- get $ brg . path "/self" . zUser usr
  pure $ responseJsonUnsafe rsp

zAuthAccess :: UserId -> ByteString -> Request -> Request
zAuthAccess u c = header "Z-Type" "access" . zUser u . zConn c

newTeam :: Galley.BindingNewTeam
newTeam = Galley.BindingNewTeam $ Galley.newNewTeam (unsafeRange "teamName") DefaultIcon

randomEmail :: (MonadIO m) => m Email
randomEmail = do
  uid <- liftIO nextRandom
  pure $ Email ("success+" <> UUID.toText uid) "simulator.amazonses.com"

randomUser :: (HasCallStack, MonadCatch m, MonadIO m, MonadHttp m) => BrigReq -> m User
randomUser brig_ = do
  n <- cs . UUID.toString <$> liftIO UUID.nextRandom
  createUser n brig_

createUser ::
  (HasCallStack, MonadCatch m, MonadIO m, MonadHttp m) =>
  Text ->
  BrigReq ->
  m User
createUser name brig_ = do
  r <- postUser name True Nothing Nothing brig_ <!! const 201 === statusCode
  pure $ responseJsonUnsafe r

-- more flexible variant of 'createUser' (see above).  (check the variant that brig has before you
-- clone this again!)
postUser ::
  (HasCallStack, MonadIO m, MonadHttp m) =>
  Text ->
  Bool ->
  Maybe UserSSOId ->
  Maybe TeamId ->
  BrigReq ->
  m ResponseLBS
postUser name haveEmail ssoid teamid brig_ = do
  email <- if haveEmail then Just <$> randomEmail else pure Nothing
  let p =
        RequestBodyLBS . Aeson.encode $
          object
            [ "name" .= name,
              "email" .= email,
              "password" .= defPassword,
              "cookie" .= defCookieLabel,
              "sso_id" .= ssoid,
              "team_id" .= teamid
            ]
  post (brig_ . path "/i/users" . contentJson . body p)

defPassword :: PlainTextPassword6
defPassword = plainTextPassword6Unsafe "topsecretdefaultpassword"

defCookieLabel :: CookieLabel
defCookieLabel = CookieLabel "auth"

zUser :: UserId -> Request -> Request
zUser = header "Z-User" . toByteString'

zConn :: ByteString -> Request -> Request
zConn = header "Z-Connection"

endpointToReq :: Endpoint -> (Bilge.Request -> Bilge.Request)
endpointToReq ep = Bilge.host (ep ^. host . to cs) . Bilge.port (ep ^. port)

-- spar specifics

shouldRespondWith ::
  forall a.
  (HasCallStack, Show a) =>
  Http a ->
  (a -> Bool) ->
  TestSpar ()
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

makeTestIdP :: (HasCallStack, MonadReader TestEnv m, MonadRandom m, MonadIO m) => m (IdPConfig WireIdP)
makeTestIdP = do
  apiversion <- view teWireIdPAPIVersion
  SampleIdP md _ _ _ <- makeSampleIdPMetadata
  IdPConfig
    <$> (IdPId <$> liftIO UUID.nextRandom)
    <*> pure md
    <*> nextWireIdP apiversion

getTestSPMetadata :: (HasCallStack, MonadReader TestEnv m, MonadIO m) => TeamId -> m SPMetadata
getTestSPMetadata tid = do
  env <- ask
  resp <-
    call . get $
      (env ^. teSpar)
        . ( case env ^. teWireIdPAPIVersion of
              WireIdPAPIV1 -> path "/sso/metadata"
              WireIdPAPIV2 -> paths ["/sso/metadata", toByteString' tid]
          )
        . expect2xx
  raw <- maybe (crash_ "no body") (pure . cs) $ responseBody resp
  either (crash_ . show) pure (SAML.decode raw)
  where
    crash_ = liftIO . throwIO . ErrorCall

-- | See 'registerTestIdPWithMeta'
registerTestIdP ::
  (HasCallStack, MonadRandom m, MonadIO m, MonadReader TestEnv m) =>
  UserId ->
  m IdP
registerTestIdP owner = fst <$> registerTestIdPWithMeta owner

-- | Create a fresh 'IdPMetadata' suitable for testing.
registerTestIdPWithMeta ::
  (HasCallStack, MonadRandom m, MonadIO m, MonadReader TestEnv m) =>
  UserId ->
  m (IdP, (IdPMetadataInfo, SAML.SignPrivCreds))
registerTestIdPWithMeta owner = do
  SampleIdP idpmeta privkey _ _ <- makeSampleIdPMetadata
  env <- ask
  idp <- registerTestIdPFrom idpmeta (env ^. teMgr) owner (env ^. teSpar)
  pure (idp, (IdPMetadataValue (cs $ SAML.encode idpmeta) idpmeta, privkey))

-- | Helper for 'registerTestIdP'.
registerTestIdPFrom ::
  (HasCallStack, MonadIO m, MonadReader TestEnv m) =>
  IdPMetadata ->
  Manager ->
  UserId ->
  SparReq ->
  m IdP
registerTestIdPFrom metadata mgr owner spar = do
  apiVer <- view teWireIdPAPIVersion
  liftIO . runHttpT mgr $ do
    callIdpCreate apiVer spar (Just owner) metadata

getCookie :: (KnownSymbol name) => proxy name -> ResponseLBS -> Either String (SAML.SimpleSetCookie name)
getCookie proxy rsp = do
  web :: Web.SetCookie <-
    Web.parseSetCookie
      <$> maybe
        (Left "no set-cookie header")
        Right
        (lookup "set-cookie" (responseHeaders rsp))
  if Web.setCookieName web == SAML.cookieName proxy
    then Right $ SimpleSetCookie web
    else Left $ "bad cookie name.  (found, expected) == " <> show (Web.setCookieName web, SAML.cookieName proxy)

-- |  we test for expiration date as it's asier than parsing and inspecting the cookie value.
hasPersistentCookieHeader :: ResponseLBS -> Either String ()
hasPersistentCookieHeader rsp = do
  cky <- getCookie (Proxy @"zuid") rsp
  when (isNothing . Web.setCookieExpires $ fromSimpleSetCookie cky) $
    Left $
      "expiration date should NOT empty: " <> show cky

tryLogin :: (HasCallStack) => SignPrivCreds -> IdP -> NameID -> TestSpar SAML.UserRef
tryLogin privkey idp userSubject = do
  env <- ask
  let tid = idp ^. idpExtraInfo . team
  spmeta <- getTestSPMetadata tid
  (_, authnreq) <- call $ callAuthnReq (env ^. teSpar) (idp ^. SAML.idpId)
  idpresp <- runSimpleSP $ mkAuthnResponseWithSubj userSubject privkey idp spmeta authnreq True
  sparresp <- submitAuthnResponse tid idpresp
  liftIO $ do
    statusCode sparresp `shouldBe` 200
    let bdy = maybe "" (cs @LByteString @String) (responseBody sparresp)
    bdy `shouldContain` "<title>wire:sso:success</title>"
  either (error . show) (pure . view userRefL) $
    SAML.parseFromDocument (fromSignedAuthnResponse idpresp)

tryLoginFail :: (HasCallStack) => SignPrivCreds -> IdP -> NameID -> String -> TestSpar ()
tryLoginFail privkey idp userSubject bodyShouldContain = do
  env <- ask
  let tid = idp ^. idpExtraInfo . team
  spmeta <- getTestSPMetadata tid
  (_, authnreq) <- call $ callAuthnReq (env ^. teSpar) (idp ^. SAML.idpId)
  idpresp <- runSimpleSP $ mkAuthnResponseWithSubj userSubject privkey idp spmeta authnreq True
  sparresp <- submitAuthnResponse tid idpresp
  liftIO $ do
    let bdy = maybe "" (cs @LByteString @String) (responseBody sparresp)
    bdy `shouldContain` bodyShouldContain

-- | see also: 'callAuthnReq'
negotiateAuthnRequest ::
  (HasCallStack, MonadIO m, MonadReader TestEnv m) =>
  IdP ->
  m SAML.AuthnRequest
negotiateAuthnRequest idp = negotiateAuthnRequest' idp id

negotiateAuthnRequest' ::
  (HasCallStack, MonadIO m, MonadReader TestEnv m) =>
  IdP ->
  (Request -> Request) ->
  m SAML.AuthnRequest
negotiateAuthnRequest' idp modreq = do
  env <- ask
  resp :: ResponseLBS <-
    call $
      get
        ( modreq
            . (env ^. teSpar)
            . paths (cs <$> ["sso", "initiate-login", idPIdToST $ idp ^. SAML.idpId])
            . expect2xx
        )
  (_, authnreq) <- either error pure . parseAuthnReqResp $ cs <$> responseBody resp
  pure authnreq

submitAuthnResponse ::
  (HasCallStack, MonadIO m, MonadReader TestEnv m) =>
  TeamId ->
  SignedAuthnResponse ->
  m ResponseLBS
submitAuthnResponse = submitAuthnResponse' id

submitAuthnResponse' ::
  (HasCallStack, MonadIO m, MonadReader TestEnv m) =>
  (Request -> Request) ->
  TeamId ->
  SignedAuthnResponse ->
  m ResponseLBS
submitAuthnResponse' reqmod tid (SignedAuthnResponse authnresp) = do
  env <- ask
  req :: Request <-
    formDataBody [partLBS "SAMLResponse" . EL.encode . XML.renderLBS XML.def $ authnresp] empty
  let p =
        case env ^. teWireIdPAPIVersion of
          WireIdPAPIV1 -> path "/sso/finalize-login/"
          WireIdPAPIV2 -> paths ["/sso/finalize-login", toByteString' tid]
  call $ post' req (reqmod . (env ^. teSpar) . p)

loginSsoUserFirstTime ::
  (HasCallStack, MonadIO m, MonadReader TestEnv m) =>
  IdP ->
  SAML.SignPrivCreds ->
  m UserId
loginSsoUserFirstTime idp privCreds = loginSsoUserFirstTime' idp privCreds <&> fst

loginSsoUserFirstTime' ::
  (HasCallStack, MonadIO m, MonadReader TestEnv m) =>
  IdP ->
  SAML.SignPrivCreds ->
  m (UserId, Cookie)
loginSsoUserFirstTime' idp privCreds = do
  nameid <- unspecifiedNameID . UUID.toText <$> liftIO UUID.nextRandom
  loginCreatedSsoUser nameid idp privCreds

loginCreatedSsoUser ::
  (HasCallStack, MonadIO m, MonadReader TestEnv m) =>
  NameID ->
  IdP ->
  SAML.SignPrivCreds ->
  m (UserId, Cookie)
loginCreatedSsoUser nameid idp privCreds = do
  env <- ask
  let tid = idp ^. idpExtraInfo . team
  authnReq <- negotiateAuthnRequest idp
  spmeta <- getTestSPMetadata tid
  authnResp <- runSimpleSP $ mkAuthnResponseWithSubj nameid privCreds idp spmeta authnReq True
  sparAuthnResp <- submitAuthnResponse tid authnResp

  let wireCookie = fromMaybe (error (show sparAuthnResp)) . lookup "Set-Cookie" $ responseHeaders sparAuthnResp
  accessResp :: ResponseLBS <-
    call $
      post ((env ^. teBrig) . path "/access" . header "Cookie" wireCookie . expect2xx)

  let uid :: UserId
      uid = Id . fromMaybe (error "bad user field in /access response body") . UUID.fromText $ uidRaw

      uidRaw :: (HasCallStack) => Text
      uidRaw = accessToken ^?! Aeson.key "user" . _String

      accessToken :: (HasCallStack) => Aeson.Value
      accessToken = tok
        where
          tok =
            either (error . ("parse error in /access response body: " <>)) id $
              Aeson.eitherDecode raw
          raw =
            fromMaybe (error "no body in /access response") $
              responseBody accessResp

      decodeCookie :: ResponseLBS -> Cookie
      decodeCookie = fromMaybe (error "missing zuid cookie") . Bilge.getCookie "zuid"

  pure (uid, decodeCookie sparAuthnResp)

callAuthnReq ::
  forall m.
  (HasCallStack, MonadIO m, MonadHttp m) =>
  SparReq ->
  SAML.IdPId ->
  m (URI, SAML.AuthnRequest)
callAuthnReq sparreq_ idpid = assert test_parseAuthnReqResp $ do
  resp <- callAuthnReq' (sparreq_ . expect2xx) idpid
  either (err resp) pure $ parseAuthnReqResp (cs <$> responseBody resp)
  where
    err :: forall n a. (MonadIO n) => ResponseLBS -> String -> n a
    err resp = liftIO . throwIO . ErrorCall . (<> ("; " <> show (responseBody resp)))

test_parseAuthnReqResp :: Bool
test_parseAuthnReqResp = isRight tst1
  where
    tst1 = parseAuthnReqResp @(Either String) (Just raw)
    _tst2 = XML.parseText XML.def raw
    raw = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\"><html xml:lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\"><body onload=\"document.forms[0].submit()\"><noscript><p><strong>Note:</strong>Since your browser does not support JavaScript, you must press the Continue button once to proceed.</p></noscript><form action=\"http://idp.net/sso/request\" method=\"post\"><input name=\"SAMLRequest\" type=\"hidden\" value=\"PHNhbWxwOkF1dGhuUmVxdWVzdCB4bWxuczpzYW1sYT0idXJuOm9hc2lzOm5hbWVzOnRjOlNBTUw6Mi4wOmFzc2VydGlvbiIgeG1sbnM6c2FtbG09InVybjpvYXNpczpuYW1lczp0YzpTQU1MOjIuMDptZXRhZGF0YSIgeG1sbnM6ZHM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvMDkveG1sZHNpZyMiIElEPSJpZGVhMDUwZmM0YzBkODQxNzJiODcwMjIzMmNlZmJiMGE3IiBJc3N1ZUluc3RhbnQ9IjIwMTgtMDctMDJUMTk6Mzk6MDYuNDQ3OTg3MVoiIFZlcnNpb249IjIuMCIgeG1sbnM6c2FtbHA9InVybjpvYXNpczpuYW1lczp0YzpTQU1MOjIuMDpwcm90b2NvbCI+PElzc3VlciB4bWxucz0idXJuOm9hc2lzOm5hbWVzOnRjOlNBTUw6Mi4wOmFzc2VydGlvbiI+aHR0cHM6Ly9hcHAud2lyZS5jb20vPC9Jc3N1ZXI+PC9zYW1scDpBdXRoblJlcXVlc3Q+\"/><noscript><input type=\"submit\" value=\"Continue\"/></noscript></form></body></html>"

parseAuthnReqResp ::
  forall n.
  (MonadError String n) =>
  Maybe LText ->
  n (URI, SAML.AuthnRequest)
parseAuthnReqResp Nothing = throwError "no response body"
parseAuthnReqResp (Just raw) = do
  xml :: XML.Document <-
    either (throwError . ("malformed html in response body: " <>) . show) pure $
      XML.parseText XML.def raw
  reqUri :: URI <-
    safeHead "form" (XML.fromDocument xml XML.$// XML.element (XML.Name "form" (Just "http://www.w3.org/1999/xhtml") Nothing))
      >>= safeHead "action" . XML.attribute "action"
      >>= SAML.parseURI'
  reqBody :: SAML.AuthnRequest <-
    safeHead "input" (XML.fromDocument xml XML.$// XML.element (XML.Name "input" (Just "http://www.w3.org/1999/xhtml") Nothing))
      >>= safeHead "value" . XML.attribute "value"
      >>= either (throwError . show) pure . EL.decode . cs
      >>= either (throwError . show) pure . SAML.decodeElem . cs
  pure (reqUri, reqBody)

safeHead :: forall n a. (MonadError String n) => String -> [a] -> n a
safeHead _ (a : _) = pure a
safeHead msg [] = throwError $ msg <> ": []"

callAuthnReq' :: (MonadHttp m) => SparReq -> SAML.IdPId -> m ResponseLBS
callAuthnReq' sparreq_ idpid = do
  get $ sparreq_ . path (cs $ "/sso/initiate-login/" -/ SAML.idPIdToST idpid)

callIdpGet :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> SAML.IdPId -> m IdP
callIdpGet sparreq_ muid idpid = do
  resp <- callIdpGet' (sparreq_ . expect2xx) muid idpid
  either (liftIO . throwIO . ErrorCall . show) pure $
    responseJsonEither @IdP resp

callIdpGet' :: (MonadHttp m) => SparReq -> Maybe UserId -> SAML.IdPId -> m ResponseLBS
callIdpGet' sparreq_ muid idpid = do
  get $ sparreq_ . maybe id zUser muid . path (cs $ "/identity-providers/" -/ SAML.idPIdToST idpid)

callIdpGetRaw :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> SAML.IdPId -> m Text
callIdpGetRaw sparreq_ muid idpid = do
  resp <- callIdpGetRaw' (sparreq_ . expect2xx) muid idpid
  maybe (liftIO . throwIO $ ErrorCall "Nothing") (pure . cs) (responseBody resp)

callIdpGetRaw' :: (MonadHttp m) => SparReq -> Maybe UserId -> SAML.IdPId -> m ResponseLBS
callIdpGetRaw' sparreq_ muid idpid = do
  get $ sparreq_ . maybe id zUser muid . path (cs $ "/identity-providers/" -/ SAML.idPIdToST idpid -/ "raw")

callIdpGetAll :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> m IdPList
callIdpGetAll sparreq_ muid = do
  resp <- callIdpGetAll' (sparreq_ . expect2xx) muid
  either (liftIO . throwIO . ErrorCall . show) pure $
    responseJsonEither resp

callIdpGetAll' :: (MonadHttp m) => SparReq -> Maybe UserId -> m ResponseLBS
callIdpGetAll' sparreq_ muid = do
  get $ sparreq_ . maybe id zUser muid . path "/identity-providers"

callIdpCreate :: (MonadIO m, MonadHttp m) => WireIdPAPIVersion -> SparReq -> Maybe UserId -> SAML.IdPMetadata -> m IdP
callIdpCreate apiversion sparreq_ muid metadata = do
  resp <- callIdpCreate' apiversion (sparreq_ . expect2xx) muid metadata
  either (liftIO . throwIO . ErrorCall . show) pure $
    responseJsonEither @IdP resp

callIdpCreate' :: (MonadIO m, MonadHttp m) => WireIdPAPIVersion -> SparReq -> Maybe UserId -> SAML.IdPMetadata -> m ResponseLBS
callIdpCreate' apiversion sparreq_ muid metadata = do
  explicitQueryParam <- do
    -- `&api_version=v1` is implicit and can be omitted from the query, but we want to test
    -- both, and not spend extra time on it.
    liftIO $ randomRIO (True, False)
  post $
    sparreq_
      . maybe id zUser muid
      . path "/identity-providers/"
      . ( case apiversion of
            WireIdPAPIV1 -> Bilge.query [("api_version", Just "v1") | explicitQueryParam]
            WireIdPAPIV2 -> Bilge.query [("api_version", Just "v2")]
        )
      . body (RequestBodyLBS . LT.encodeUtf8 $ SAML.encode metadata)
      . header "Content-Type" "application/xml"

callIdpCreateRaw :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> ByteString -> LByteString -> m IdP
callIdpCreateRaw sparreq_ muid ctyp metadata = do
  resp <- callIdpCreateRaw' (sparreq_ . expect2xx) muid ctyp metadata
  either (liftIO . throwIO . ErrorCall . show) pure $
    responseJsonEither @IdP resp

callIdpCreateRaw' :: (MonadHttp m) => SparReq -> Maybe UserId -> ByteString -> LByteString -> m ResponseLBS
callIdpCreateRaw' sparreq_ muid ctyp metadata = do
  post $
    sparreq_
      . maybe id zUser muid
      . path "/identity-providers/"
      . body (RequestBodyLBS metadata)
      . header "Content-Type" ctyp

callIdpCreateWithHandle :: (MonadIO m, MonadHttp m) => WireIdPAPIVersion -> SparReq -> Maybe UserId -> SAML.IdPMetadata -> IdPHandle -> m IdP
callIdpCreateWithHandle apiversion sparreq_ muid metadata idpHandle = do
  resp <- callIdpCreateWithHandle' apiversion (sparreq_ . expect2xx) muid metadata idpHandle
  either (liftIO . throwIO . ErrorCall . show) pure $
    responseJsonEither @IdP resp

callIdpCreateWithHandle' :: (HasCallStack, MonadIO m, MonadHttp m) => WireIdPAPIVersion -> SparReq -> Maybe UserId -> IdPMetadata -> IdPHandle -> m ResponseLBS
callIdpCreateWithHandle' apiversion sparreq_ muid metadata idpHandle = do
  explicitQueryParam <- do
    -- `&api_version=v1` is implicit and can be omitted from the query, but we want to test
    -- both, and not spend extra time on it.
    liftIO $ randomRIO (True, False)
  let versionParam =
        case apiversion of
          WireIdPAPIV1 -> if explicitQueryParam then Just "v1" else Nothing
          WireIdPAPIV2 -> Just "v2"
  post $
    sparreq_
      . maybe id zUser muid
      . path "/identity-providers/"
      . Bilge.query
        [ ("api_version", versionParam),
          ("handle", Just . cs . unIdPHandle $ idpHandle)
        ]
      . body (RequestBodyLBS . cs $ SAML.encode metadata)
      . header "Content-Type" "application/xml"

callIdpCreateReplace :: (MonadIO m, MonadHttp m) => WireIdPAPIVersion -> SparReq -> Maybe UserId -> IdPMetadata -> IdPId -> m IdP
callIdpCreateReplace apiversion sparreq_ muid metadata idpid = do
  resp <- callIdpCreateReplace' apiversion (sparreq_ . expect2xx) muid metadata idpid
  either (liftIO . throwIO . ErrorCall . show) pure $
    responseJsonEither @IdP resp

callIdpCreateReplace' :: (HasCallStack, MonadIO m, MonadHttp m) => WireIdPAPIVersion -> SparReq -> Maybe UserId -> IdPMetadata -> IdPId -> m ResponseLBS
callIdpCreateReplace' apiversion sparreq_ muid metadata idpid = do
  explicitQueryParam <- do
    -- `&api_version=v1` is implicit and can be omitted from the query, but we want to test
    -- both, and not spend extra time on it.
    liftIO $ randomRIO (True, False)
  post $
    sparreq_
      . maybe id zUser muid
      . path "/identity-providers/"
      . Bilge.query
        [ ( "api_version",
            case apiversion of
              WireIdPAPIV1 -> if explicitQueryParam then Just "v1" else Nothing
              WireIdPAPIV2 -> Just "v2"
          ),
          ( "replaces",
            Just . cs . idPIdToST $ idpid
          )
        ]
      . body (RequestBodyLBS . cs $ SAML.encode metadata)
      . header "Content-Type" "application/xml"

callIdpUpdate' :: (Monad m, MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> IdPId -> IdPMetadataInfo -> m IdP
callIdpUpdate' sparreq_ muid idpid metainfo = do
  resp <- callIdpUpdate (sparreq_ . expect2xx) muid idpid metainfo
  either (liftIO . throwIO . ErrorCall . show) pure $
    responseJsonEither @IdP resp

callIdpUpdate :: (MonadHttp m) => SparReq -> Maybe UserId -> IdPId -> IdPMetadataInfo -> m ResponseLBS
callIdpUpdate sparreq_ muid idpid (IdPMetadataValue metadata _) = do
  put $
    sparreq_
      . maybe id zUser muid
      . paths ["identity-providers", toByteString' $ idPIdToST idpid]
      . body (RequestBodyLBS $ cs metadata)
      . header "Content-Type" "application/xml"

callIdpUpdateWithHandle :: (MonadHttp m) => SparReq -> Maybe UserId -> IdPId -> IdPMetadataInfo -> IdPHandle -> m ResponseLBS
callIdpUpdateWithHandle sparreq_ muid idpid (IdPMetadataValue metadata _) idpHandle = do
  put $
    sparreq_
      . maybe id zUser muid
      . paths ["identity-providers", toByteString' $ idPIdToST idpid]
      . Bilge.query [("handle", Just . cs . unIdPHandle $ idpHandle)]
      . body (RequestBodyLBS $ cs metadata)
      . header "Content-Type" "application/xml"

callIdpDelete :: (Functor m, MonadHttp m) => SparReq -> Maybe UserId -> SAML.IdPId -> m ()
callIdpDelete sparreq_ muid idpid = void $ callIdpDelete' (sparreq_ . expect2xx) muid idpid

callIdpDelete' :: (MonadHttp m) => SparReq -> Maybe UserId -> SAML.IdPId -> m ResponseLBS
callIdpDelete' sparreq_ muid idpid = do
  delete $
    sparreq_
      . maybe id zUser muid
      . path (cs $ "/identity-providers/" -/ SAML.idPIdToST idpid)

callIdpDeletePurge' :: (MonadHttp m) => SparReq -> Maybe UserId -> SAML.IdPId -> m ResponseLBS
callIdpDeletePurge' sparreq_ muid idpid = do
  delete $
    sparreq_
      . maybe id zUser muid
      . path (cs $ "/identity-providers/" -/ SAML.idPIdToST idpid)
      . queryItem "purge" "true"

callGetDefaultSsoCode :: (MonadHttp m) => SparReq -> m ResponseLBS
callGetDefaultSsoCode sparreq_ = do
  get $
    sparreq_
      . path "/sso/settings/"

callSetDefaultSsoCode :: (MonadHttp m) => SparReq -> SAML.IdPId -> m ResponseLBS
callSetDefaultSsoCode sparreq_ ssoCode = do
  let settings =
        RequestBodyLBS . Aeson.encode $
          object
            [ "default_sso_code" .= (SAML.fromIdPId ssoCode :: UUID)
            ]
  put $
    sparreq_
      . path "/i/sso/settings/"
      . body settings
      . header "Content-Type" "application/json"

callDeleteDefaultSsoCode :: (MonadHttp m) => SparReq -> m ResponseLBS
callDeleteDefaultSsoCode sparreq_ = do
  let settings =
        RequestBodyLBS . Aeson.encode $
          object
            [ "default_sso_code" .= Aeson.Null
            ]
  put $
    sparreq_
      . path "/i/sso/settings/"
      . body settings
      . header "Content-Type" "application/json"

-- helpers talking to spar's cassandra directly

-- | Look up 'UserId' under 'UserSSOId' on spar's cassandra directly.
ssoToUidSpar :: (HasCallStack, MonadIO m, MonadReader TestEnv m) => TeamId -> UserSSOId -> m (Maybe UserId)
ssoToUidSpar tid ssoid = do
  veid <- either (error . ("could not parse brig sso_id: " <>)) pure $ Intra.veidFromUserSSOId ssoid
  runSpar $
    runValidExternalIdEither
      SAMLUserStore.get
      (ScimExternalIdStore.lookup tid)
      veid

runSimpleSP :: (MonadReader TestEnv m, MonadIO m) => SAML.SimpleSP a -> m a
runSimpleSP action = do
  env <- ask
  liftIO $ do
    ctx <- SAML.mkSimpleSPCtx (env ^. teOpts . to saml) []
    result <- SAML.runSimpleSP ctx action
    either (throwIO . ErrorCall . show) pure result

runSpar ::
  (MonadReader TestEnv m, MonadIO m) =>
  Sem CanonicalEffs a ->
  m a
runSpar action = do
  result <- runSparE action
  liftIO $ either (throwIO . ErrorCall . show) pure result

runSparE ::
  (MonadReader TestEnv m, MonadIO m) =>
  Sem CanonicalEffs a ->
  m (Either SparError a)
runSparE action = do
  ctx <- (^. teSparEnv) <$> ask
  liftIO $ runSparToIO ctx action

getSsoidViaSelf :: (HasCallStack) => UserId -> TestSpar UserSSOId
getSsoidViaSelf uid = maybe (error "not found") pure =<< getSsoidViaSelf' uid

getSsoidViaSelf' :: (HasCallStack) => UserId -> TestSpar (Maybe UserSSOId)
getSsoidViaSelf' uid = do
  musr <- aFewTimes (runSpar $ Intra.getBrigUser Intra.NoPendingInvitations uid) isJust
  pure $ ssoIdentity =<< (userIdentity =<< musr)

getUserIdViaRef :: (HasCallStack) => UserRef -> TestSpar UserId
getUserIdViaRef uref = maybe (error "not found") pure =<< getUserIdViaRef' uref

getUserIdViaRef' :: (HasCallStack) => UserRef -> TestSpar (Maybe UserId)
getUserIdViaRef' uref = do
  aFewTimes (runSpar $ SAMLUserStore.get uref) isJust

checkErr :: (HasCallStack) => Int -> Maybe TestErrorLabel -> Assertions ()
checkErr status mlabel = do
  const status === statusCode
  case mlabel of
    Nothing -> pure ()
    Just label -> const (Right label) === responseJsonEither

checkErrHspec :: (HasCallStack) => Int -> TestErrorLabel -> ResponseLBS -> Bool
checkErrHspec status label resp = status == statusCode resp && responseJsonEither resp == Right label

-- | copied from brig integration tests
stdInvitationRequest :: User.Email -> TeamInvitation.InvitationRequest
stdInvitationRequest = stdInvitationRequest' Nothing Nothing

-- | copied from brig integration tests
stdInvitationRequest' :: Maybe User.Locale -> Maybe Role -> User.Email -> TeamInvitation.InvitationRequest
stdInvitationRequest' loc role email =
  TeamInvitation.InvitationRequest loc role Nothing email Nothing

setRandomHandleBrig :: (HasCallStack) => UserId -> TestSpar ()
setRandomHandleBrig uid = do
  env <- ask
  call (changeHandleBrig (env ^. teBrig) uid =<< liftIO randomHandle)
    !!! (const 200 === statusCode)
  where
    randomHandle = liftIO $ do
      nrs <- replicateM 21 (randomRIO (97, 122)) -- a-z
      pure (cs (chr <$> nrs))

changeHandleBrig ::
  (MonadHttp m, HasCallStack) =>
  BrigReq ->
  UserId ->
  Text ->
  m ResponseLBS
changeHandleBrig brig uid handlTxt = do
  put
    ( brig
        . path "/self/handle"
        . zUser uid
        . zConn "user"
        . contentJson
        . json (HandleUpdate handlTxt)
    )

updateProfileBrig ::
  (MonadHttp m, HasCallStack) =>
  BrigReq ->
  UserId ->
  UserUpdate ->
  m ResponseLBS
updateProfileBrig brig uid uupd =
  put
    ( brig
        . path "/self"
        . zUser uid
        . zConn "user"
        . contentJson
        . json uupd
    )

updateTeamMemberRole :: (MonadReader TestEnv m, MonadIO m) => TeamId -> UserId -> UserId -> Role.Role -> m ()
updateTeamMemberRole tid adminUid targetUid role = do
  spar <- asks (^. teGalley)
  void . call . put $
    spar
      . zUser adminUid
      . zConn "user"
      . paths ["teams", toByteString' tid, "members"]
      . json (Member.mkNewTeamMember targetUid (rolePermissions role) Nothing)
      . expect2xx

-- https://wearezeta.atlassian.net/browse/SQSERVICES-1279: change role after successful creation/activation.
checkChangeRoleOfTeamMember :: TeamId -> UserId -> UserId -> TestSpar ()
checkChangeRoleOfTeamMember tid adminId targetId = forM_ [minBound ..] $ \role -> do
  updateTeamMemberRole tid adminId targetId role
  [member'] <- filter ((== targetId) . (^. Member.userId)) <$> getTeamMembers adminId tid
  liftIO $ (member' ^. Member.permissions . to Member.permissionsRole) `shouldBe` Just role

eventually :: (HasCallStack) => TestSpar a -> TestSpar a
eventually = recoverAll (limitRetries 3 <> exponentialBackoff 100000) . const

getIdPByIssuer :: (HasCallStack) => Issuer -> TeamId -> TestSpar (Maybe IdP)
getIdPByIssuer issuer tid = do
  idpApiVersion <- view teWireIdPAPIVersion
  runSpar $ case idpApiVersion of
    WireIdPAPIV1 -> IdPConfigStore.getIdPByIssuerV1Maybe issuer
    WireIdPAPIV2 -> IdPConfigStore.getIdPByIssuerV2Maybe issuer tid
