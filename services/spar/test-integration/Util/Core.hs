{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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
    xit,
    shouldRespondWith,
    module Test.Hspec,
    aFewTimes,

    -- * HTTP
    call,
    endpointToReq,
    endpointToSettings,
    endpointToURL,

    -- * Other
    defPassword,
    getUserBrig,
    createUserWithTeam,
    createUserWithTeamDisableSSO,
    getSSOEnabledInternal,
    putSSOEnabledInternal,
    inviteAndRegisterUser,
    createTeamMember,
    deleteUserOnBrig,
    getTeams,
    getTeamMembers,
    promoteTeamMember,
    getSelfProfile,
    nextWireId,
    nextWireIdP,
    nextHandle,
    nextSAMLID,
    nextSubject,
    nextUserRef,
    createRandomPhoneUser,
    zUser,
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
    hasDeleteBindCookieHeader,
    hasSetBindCookieHeader,
    submitAuthnResponse,
    submitAuthnResponse',
    loginSsoUserFirstTime,
    callAuthnReqPrecheck',
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
    callIdpUpdate',
    callIdpDelete,
    callIdpDelete',
    callIdpDeletePurge',
    initCassandra,
    ssoToUidSpar,
    runSparCass,
    runSparCassWithEnv,
    runSimpleSP,
    runSpar,
    getSsoidViaSelf,
    getSsoidViaSelf',
    getUserIdViaRef,
    getUserIdViaRef',
    callGetDefaultSsoCode,
    callSetDefaultSsoCode,
    callDeleteDefaultSsoCode,
  )
where

import Bilge hiding (getCookie) -- we use Web.Cookie instead of the http-client type
import Bilge.Assert ((!!!), (<!!), (===))
import qualified Brig.Types.Activation as Brig
import Brig.Types.Common (UserIdentity (..), UserSSOId (..))
import Brig.Types.User (User (..), selfUser, userIdentity)
import qualified Brig.Types.User as Brig
import qualified Brig.Types.User.Auth as Brig
import Cassandra as Cas
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Fail (MonadFail)
import Control.Retry
import Crypto.Random.Types (MonadRandom)
import Data.Aeson as Aeson hiding (json)
import Data.Aeson.Lens as Aeson
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Base64.Lazy as EL
import Data.ByteString.Conversion
import Data.Handle (Handle (Handle))
import Data.Id
import Data.Misc (PlainTextPassword (..))
import Data.Proxy
import Data.Range
import Data.String.Conversions
import qualified Data.Text.Ascii as Ascii
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Data.UUID as UUID hiding (fromByteString, null)
import Data.UUID.V4 as UUID (nextRandom)
import qualified Data.Yaml as Yaml
import GHC.TypeLits
import qualified Galley.Types.Teams as Galley
import Imports hiding (head)
import Network.HTTP.Client.MultipartFormData
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.Warp.Internal as Warp
import qualified Options.Applicative as OPA
import SAML2.WebSSO as SAML
import qualified SAML2.WebSSO.API.Example as SAML
import SAML2.WebSSO.Test.Lenses (userRefL)
import SAML2.WebSSO.Test.MockResponse
import SAML2.WebSSO.Test.Util (SampleIdP (..), makeSampleIdPMetadata)
import Spar.API.Types
import Spar.App (toLevel)
import qualified Spar.App as Spar
import qualified Spar.Data as Data
import qualified Spar.Intra.Brig as Intra
import qualified Spar.Options
import Spar.Run
import Spar.Types
import qualified System.Logger.Extended as Log
import System.Random (randomRIO)
import Test.Hspec hiding (it, pending, pendingWith, xit)
import qualified Test.Hspec
import qualified Text.XML as XML
import qualified Text.XML.Cursor as XML
import Text.XML.DSig (SignPrivCreds)
import qualified Text.XML.DSig as SAML
import URI.ByteString
import Util.Options
import Util.Types
import qualified Web.Cookie as Web
import Wire.API.Team.Feature (TeamFeatureStatus (..), TeamFeatureStatusValue (..))
import qualified Wire.API.Team.Invitation as TeamInvitation
import qualified Wire.API.User as User

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
    <$> ( OPA.strOption $
            OPA.long "integration-config"
              <> OPA.short 'i'
              <> OPA.help "Integration config to load"
              <> OPA.showDefault
              <> OPA.value defaultIntPath
        )
    <*> ( OPA.strOption $
            OPA.long "service-config"
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
mkEnv :: HasCallStack => IntegrationConfig -> Opts -> IO TestEnv
mkEnv _teTstOpts _teOpts = do
  _teMgr :: Manager <- newManager defaultManagerSettings
  sparCtxLogger <- Log.mkLogger (toLevel $ saml _teOpts ^. SAML.cfgLogLevel) (logNetStrings _teOpts) (logFormat _teOpts)
  _teCql :: ClientState <- initCassandra _teOpts sparCtxLogger
  let _teBrig = endpointToReq (cfgBrig _teTstOpts)
      _teGalley = endpointToReq (cfgGalley _teTstOpts)
      _teSpar = endpointToReq (cfgSpar _teTstOpts)
      _teSparEnv = Spar.Env {..}
      sparCtxOpts = _teOpts
      sparCtxCas = _teCql
      sparCtxHttpManager = _teMgr
      sparCtxHttpBrig = _teBrig empty
      sparCtxHttpGalley = _teGalley empty
      sparCtxRequestId = RequestId "<fake request id>"
  pure TestEnv {..}

destroyEnv :: HasCallStack => TestEnv -> IO ()
destroyEnv _ = pure ()

it ::
  HasCallStack =>
  -- or, more generally:
  -- MonadIO m, Example (TestEnv -> m ()), Arg (TestEnv -> m ()) ~ TestEnv
  String ->
  TestSpar () ->
  SpecWith TestEnv
it msg bdy = Test.Hspec.it msg $ runReaderT bdy

xit ::
  HasCallStack =>
  -- or, more generally:
  -- MonadIO m, Example (TestEnv -> m ()), Arg (TestEnv -> m ()) ~ TestEnv
  String ->
  TestSpar () ->
  SpecWith TestEnv
xit msg bdy = Test.Hspec.xit msg $ runReaderT bdy

pending :: (HasCallStack, MonadIO m) => m ()
pending = liftIO Test.Hspec.pending

pendingWith :: (HasCallStack, MonadIO m) => String -> m ()
pendingWith = liftIO . Test.Hspec.pendingWith

-- | Run a probe several times, until a "good" value materializes or until patience runs out.
-- If all retries were unsuccessful, 'aFewTimes' will return the last obtained value, even
-- if it does not satisfy the predicate.
aFewTimes :: TestSpar a -> (a -> Bool) -> TestSpar a
aFewTimes action good = do
  env <- ask
  liftIO $
    retrying
      (exponentialBackoff 1000 <> limitRetries 10)
      (\_ -> pure . not . good)
      (\_ -> action `runReaderT` env)

-- | Duplicate of 'Spar.Intra.Brig.getBrigUser'.
getUserBrig :: HasCallStack => UserId -> TestSpar (Maybe User)
getUserBrig uid = do
  env <- ask
  let req =
        (env ^. teBrig) . path "/self"
          . header "Z-User" (toByteString' uid)
  resp <- call $ get req
  case statusCode resp of
    200 -> do
      let user = selfUser $ responseJsonUnsafe resp
      pure $
        if (userDeleted user)
          then Nothing
          else Just user
    404 -> pure Nothing
    bad -> error $ show bad

createUserWithTeam :: (HasCallStack, MonadHttp m, MonadIO m, MonadFail m) => BrigReq -> GalleyReq -> m (UserId, TeamId)
createUserWithTeam brg gly = do
  (uid, tid) <- createUserWithTeamDisableSSO brg gly
  putSSOEnabledInternal gly tid TeamFeatureEnabled
  pure (uid, tid)

createUserWithTeamDisableSSO :: (HasCallStack, MonadHttp m, MonadIO m, MonadFail m) => BrigReq -> GalleyReq -> m (UserId, TeamId)
createUserWithTeamDisableSSO brg gly = do
  e <- randomEmail
  n <- UUID.toString <$> liftIO UUID.nextRandom
  let p =
        RequestBodyLBS . Aeson.encode $
          object
            [ "name" .= n,
              "email" .= Brig.fromEmail e,
              "password" .= defPassword,
              "team" .= newTeam
            ]
  bdy <- selfUser . responseJsonUnsafe <$> post (brg . path "/i/users" . contentJson . body p)
  let (uid, Just tid) = (Brig.userId bdy, Brig.userTeam bdy)
  (team : _) <- (^. Galley.teamListTeams) <$> getTeams uid gly
  () <-
    Control.Exception.assert {- "Team ID in registration and team table do not match" -} (tid == team ^. Galley.teamId) $
      pure ()
  selfTeam <- Brig.userTeam . Brig.selfUser <$> getSelfProfile brg uid
  () <-
    Control.Exception.assert {- "Team ID in self profile and team table do not match" -} (selfTeam == Just tid) $
      pure ()
  return (uid, tid)

getSSOEnabledInternal :: (HasCallStack, MonadHttp m, MonadIO m) => GalleyReq -> TeamId -> m ResponseLBS
getSSOEnabledInternal gly tid = do
  get $
    gly
      . paths ["i", "teams", toByteString' tid, "features", "sso"]

putSSOEnabledInternal :: (HasCallStack, MonadHttp m, MonadIO m) => GalleyReq -> TeamId -> TeamFeatureStatusValue -> m ()
putSSOEnabledInternal gly tid enabled = do
  void . put $
    gly
      . paths ["i", "teams", toByteString' tid, "features", "sso"]
      . json (TeamFeatureStatus enabled)
      . expect2xx

-- | cloned from `/services/brig/test/integration/API/Team/Util.hs`.
inviteAndRegisterUser ::
  (MonadIO m, MonadCatch m, MonadFail m, MonadHttp m, HasCallStack) =>
  BrigReq ->
  UserId ->
  TeamId ->
  m User
inviteAndRegisterUser brig u tid = do
  inviteeEmail <- randomEmail
  let invite = TeamInvitation.InvitationRequest inviteeEmail (User.Name "Bob") Nothing Nothing Nothing Nothing
  inv <- responseJsonError =<< postInvitation tid u invite
  Just inviteeCode <- getInvitationCode tid (TeamInvitation.inInvitation inv)
  rspInvitee <-
    post
      ( brig . path "/register"
          . contentJson
          . body (accept' inviteeEmail inviteeCode)
      )
      <!! const 201
      === statusCode
  let Just invitee = responseJsonMaybe rspInvitee
  unless (Just tid == userTeam invitee) $ error "Team ID in registration and team table do not match"
  selfTeam <- userTeam . selfUser <$> getSelfProfile brig (userId invitee)
  unless (selfTeam == Just tid) $ error "Team ID in self profile and team table do not match"
  return invitee
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
      (MonadIO m, MonadHttp m, HasCallStack) =>
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
      return $ fromByteString . fromMaybe (error "No code?") $ encodeUtf8 <$> (lbs ^? key "code" . _String)

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
  Galley.Permissions ->
  m UserId
createTeamMember brigreq galleyreq teamid perms = do
  let randomtxt = liftIO $ UUID.toText <$> UUID.nextRandom
      randomssoid = Brig.UserSSOId <$> randomtxt <*> randomtxt
  name <- randomtxt
  ssoid <- randomssoid
  resp :: ResponseLBS <-
    postUser name False (Just ssoid) (Just teamid) brigreq
      <!! const 201 === statusCode
  let nobody :: UserId = Brig.userId (responseJsonUnsafe @Brig.User resp)
      tmem :: Galley.TeamMember = Galley.newTeamMember nobody perms Nothing
  addTeamMember galleyreq teamid (Galley.newNewTeamMember tmem)
  pure nobody

-- | FUTUREWORK(fisx): use the specified & supported flows for scaffolding; this is a hack
-- that builds up the internal structure from scratch, without too much thought.  For
-- instance, the team field in the user in brig won't be updated.
addTeamMember ::
  (HasCallStack, MonadCatch m, MonadIO m, MonadHttp m) =>
  GalleyReq ->
  TeamId ->
  Galley.NewTeamMember ->
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
  (HasCallStack, MonadMask m, MonadCatch m, MonadIO m, MonadHttp m) =>
  BrigReq ->
  UserId ->
  m ()
deleteUserOnBrig brigreq uid = do
  deleteUserNoWait brigreq uid
  recoverAll (exponentialBackoff 30000 <> limitRetries 5) $ \_ -> do
    profile <- getSelfProfile brigreq uid
    liftIO $ selfUser profile `shouldSatisfy` Brig.userDeleted

-- | Delete a user from Brig but don't wait.
deleteUserNoWait ::
  (HasCallStack, MonadCatch m, MonadIO m, MonadHttp m) =>
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
nextWireId :: MonadIO m => m (Id a)
nextWireId = Id <$> liftIO UUID.nextRandom

nextWireIdP :: MonadIO m => m WireIdP
nextWireIdP = WireIdP <$> (Id <$> liftIO UUID.nextRandom) <*> pure [] <*> pure Nothing

nextSAMLID :: MonadIO m => m (ID a)
nextSAMLID = mkID . UUID.toText <$> liftIO UUID.nextRandom

nextHandle :: MonadIO m => m Handle
nextHandle = liftIO $ Handle . cs . show <$> randomRIO (0 :: Int, 13371137)

-- | Generate a 'SAML.UserRef' subject.
nextSubject :: (HasCallStack, MonadIO m) => m NameID
nextSubject = liftIO $ do
  unameId <-
    randomRIO (0, 1 :: Int) >>= \case
      0 -> either (error . show) id . SAML.mkUNameIDEmail . Brig.fromEmail <$> randomEmail
      1 -> SAML.mkUNameIDUnspecified . UUID.toText <$> UUID.nextRandom
      _ -> error "nextSubject: impossible"
  either (error . show) pure $ SAML.mkNameID unameId Nothing Nothing Nothing

nextUserRef :: MonadIO m => m SAML.UserRef
nextUserRef = liftIO $ do
  tenant <- UUID.toText <$> UUID.nextRandom
  subject <- nextSubject
  pure $
    SAML.UserRef
      (SAML.Issuer $ SAML.unsafeParseURI ("http://" <> tenant))
      subject

createRandomPhoneUser :: (HasCallStack, MonadCatch m, MonadIO m, MonadHttp m) => BrigReq -> m (UserId, Brig.Phone)
createRandomPhoneUser brig_ = do
  usr <- randomUser brig_
  let uid = Brig.userId usr
  phn <- liftIO randomPhone
  -- update phone
  let phoneUpdate = RequestBodyLBS . Aeson.encode $ Brig.PhoneUpdate phn
  put (brig_ . path "/self/phone" . contentJson . zUser uid . zConn "c" . body phoneUpdate)
    !!! (const 202 === statusCode)
  -- activate
  act <- getActivationCode brig_ (Right phn)
  case act of
    Nothing -> liftIO . throwIO $ ErrorCall "missing activation key/code"
    Just kc -> activate brig_ kc !!! const 200 === statusCode
  -- check new phone
  get (brig_ . path "/self" . zUser uid) !!! do
    const 200 === statusCode
    const (Right (Just phn)) === (fmap Brig.userPhone . responseJsonEither)
  return (uid, phn)

getTeams :: (HasCallStack, MonadHttp m, MonadIO m) => UserId -> GalleyReq -> m Galley.TeamList
getTeams u gly = do
  r <-
    get
      ( gly
          . paths ["teams"]
          . zAuthAccess u "conn"
          . expect2xx
      )
  return $ responseJsonUnsafe r

getTeamMembers :: HasCallStack => UserId -> TeamId -> TestSpar [UserId]
getTeamMembers usr tid = do
  gly <- view teGalley
  resp <-
    call $
      get (gly . paths ["teams", toByteString' tid, "members"] . zUser usr)
        <!! const 200 === statusCode
  let mems :: Galley.TeamMemberList
      Right mems = responseJsonEither resp
  pure $ (^. Galley.userId) <$> (mems ^. Galley.teamMembers)

promoteTeamMember :: HasCallStack => UserId -> TeamId -> UserId -> TestSpar ()
promoteTeamMember usr tid memid = do
  gly <- view teGalley
  let bdy :: Galley.NewTeamMember
      bdy = Galley.newNewTeamMember $ Galley.newTeamMember memid Galley.fullPermissions Nothing
  call $
    put (gly . paths ["teams", toByteString' tid, "members"] . zAuthAccess usr "conn" . json bdy)
      !!! const 200 === statusCode

getSelfProfile :: (HasCallStack, MonadHttp m, MonadIO m) => BrigReq -> UserId -> m Brig.SelfProfile
getSelfProfile brg usr = do
  rsp <- get $ brg . path "/self" . zUser usr
  return $ responseJsonUnsafe rsp

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
  nrs <- map show <$> replicateM 14 (randomRIO (0, 9) :: IO Int)
  let phone = Brig.parsePhone . cs $ "+0" ++ concat nrs
  return $ fromMaybe (error "Invalid random phone#") phone

randomUser :: (HasCallStack, MonadCatch m, MonadIO m, MonadHttp m) => BrigReq -> m Brig.User
randomUser brig_ = do
  n <- cs . UUID.toString <$> liftIO UUID.nextRandom
  createUser n brig_

createUser ::
  (HasCallStack, MonadCatch m, MonadIO m, MonadHttp m) =>
  ST ->
  BrigReq ->
  m Brig.User
createUser name brig_ = do
  r <- postUser name True Nothing Nothing brig_ <!! const 201 === statusCode
  return $ responseJsonUnsafe r

-- more flexible variant of 'createUser' (see above).  (check the variant that brig has before you
-- clone this again!)
postUser ::
  (HasCallStack, MonadIO m, MonadHttp m) =>
  ST ->
  Bool ->
  Maybe Brig.UserSSOId ->
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

defPassword :: PlainTextPassword
defPassword = PlainTextPassword "secret"

defCookieLabel :: Brig.CookieLabel
defCookieLabel = Brig.CookieLabel "auth"

getActivationCode ::
  (HasCallStack, MonadIO m, MonadHttp m) =>
  BrigReq ->
  Either Brig.Email Brig.Phone ->
  m (Maybe (Brig.ActivationKey, Brig.ActivationCode))
getActivationCode brig_ ep = do
  let qry = either (queryItem "email" . toByteString') (queryItem "phone" . toByteString') ep
  r <- get $ brig_ . path "/i/users/activation-code" . qry
  let lbs = fromMaybe "" $ responseBody r
  let akey = Brig.ActivationKey . Ascii.unsafeFromText <$> (lbs ^? Aeson.key "key" . Aeson._String)
  let acode = Brig.ActivationCode . Ascii.unsafeFromText <$> (lbs ^? Aeson.key "code" . Aeson._String)
  return $ (,) <$> akey <*> acode

activate ::
  (HasCallStack, MonadIO m, MonadHttp m) =>
  BrigReq ->
  Brig.ActivationPair ->
  m ResponseLBS
activate brig_ (k, c) =
  get $
    brig_
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
endpointToSettings endpoint =
  Warp.defaultSettings
    { Warp.settingsHost = Imports.fromString . cs $ endpoint ^. epHost,
      Warp.settingsPort = fromIntegral $ endpoint ^. epPort
    }

endpointToURL :: MonadIO m => Endpoint -> ST -> m URI
endpointToURL endpoint urlpath = either err pure url
  where
    url = parseURI' ("http://" <> urlhost <> ":" <> urlport) <&> (=/ urlpath)
    urlhost = cs $ endpoint ^. epHost
    urlport = cs . show $ endpoint ^. epPort
    err = liftIO . throwIO . ErrorCall . show . (,(endpoint, url))

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

makeTestIdP :: (HasCallStack, MonadRandom m, MonadIO m) => m (IdPConfig WireIdP)
makeTestIdP = do
  SampleIdP md _ _ _ <- makeSampleIdPMetadata
  IdPConfig
    <$> (IdPId <$> liftIO UUID.nextRandom)
    <*> (pure md)
    <*> nextWireIdP

getTestSPMetadata :: (HasCallStack, MonadReader TestEnv m, MonadIO m) => m SPMetadata
getTestSPMetadata = do
  env <- ask
  resp <- call . get $ (env ^. teSpar) . path "/sso/metadata" . expect2xx
  raw <- maybe (crash_ "no body") (pure . cs) $ responseBody resp
  either (crash_ . show) pure (SAML.decode raw)
  where
    crash_ = liftIO . throwIO . ErrorCall

-- | See 'registerTestIdPWithMeta'
registerTestIdP ::
  (HasCallStack, MonadRandom m, MonadIO m, MonadReader TestEnv m) =>
  m (UserId, TeamId, IdP)
registerTestIdP = do
  (uid, tid, idp, _) <- registerTestIdPWithMeta
  pure (uid, tid, idp)

-- | Create a fresh 'IdPMetadata' suitable for testing.  Call 'createUserWithTeam' and create the
-- idp in the resulting team.  The user returned is the owner of the team.
registerTestIdPWithMeta ::
  (HasCallStack, MonadRandom m, MonadIO m, MonadReader TestEnv m) =>
  m (UserId, TeamId, IdP, (IdPMetadataInfo, SAML.SignPrivCreds))
registerTestIdPWithMeta = do
  (SampleIdP idpmeta privkey _ _) <- makeSampleIdPMetadata
  env <- ask
  (uid, tid, idp) <- registerTestIdPFrom idpmeta (env ^. teMgr) (env ^. teBrig) (env ^. teGalley) (env ^. teSpar)
  pure (uid, tid, idp, (IdPMetadataValue (cs $ SAML.encode idpmeta) idpmeta, privkey))

-- | Helper for 'registerTestIdP'.
registerTestIdPFrom ::
  (HasCallStack, MonadIO m) =>
  IdPMetadata ->
  Manager ->
  BrigReq ->
  GalleyReq ->
  SparReq ->
  m (UserId, TeamId, IdP)
registerTestIdPFrom metadata mgr brig galley spar = do
  liftIO . runHttpT mgr $ do
    (uid, tid) <- createUserWithTeam brig galley
    (uid,tid,) <$> callIdpCreate spar (Just uid) metadata

getCookie :: KnownSymbol name => proxy name -> ResponseLBS -> Either String (SAML.SimpleSetCookie name)
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

-- | In 'setResponseCookie' we set an expiration date iff cookie is persistent.  So here we test for
-- expiration date.  Easier than parsing and inspecting the cookie value.
hasPersistentCookieHeader :: ResponseLBS -> Either String ()
hasPersistentCookieHeader rsp = do
  cky <- getCookie (Proxy @"zuid") rsp
  when (isNothing . Web.setCookieExpires $ fromSimpleSetCookie cky) $
    Left $
      "expiration date should NOT empty: " <> show cky

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

tryLogin :: HasCallStack => SignPrivCreds -> IdP -> NameID -> TestSpar SAML.UserRef
tryLogin privkey idp userSubject = do
  env <- ask
  spmeta <- getTestSPMetadata
  (_, authnreq) <- call $ callAuthnReq (env ^. teSpar) (idp ^. SAML.idpId)
  idpresp <- runSimpleSP $ mkAuthnResponseWithSubj userSubject privkey idp spmeta authnreq True
  sparresp <- submitAuthnResponse idpresp
  liftIO $ do
    statusCode sparresp `shouldBe` 200
    let bdy = maybe "" (cs @LBS @String) (responseBody sparresp)
    bdy `shouldContain` "<title>wire:sso:success</title>"
  either (error . show) (pure . view userRefL) $
    SAML.parseFromDocument (fromSignedAuthnResponse idpresp)

tryLoginFail :: HasCallStack => SignPrivCreds -> IdP -> NameID -> String -> TestSpar ()
tryLoginFail privkey idp userSubject bodyShouldContain = do
  env <- ask
  spmeta <- getTestSPMetadata
  (_, authnreq) <- call $ callAuthnReq (env ^. teSpar) (idp ^. SAML.idpId)
  idpresp <- runSimpleSP $ mkAuthnResponseWithSubj userSubject privkey idp spmeta authnreq True
  sparresp <- submitAuthnResponse idpresp
  liftIO $ do
    let bdy = maybe "" (cs @LBS @String) (responseBody sparresp)
    bdy `shouldContain` bodyShouldContain

-- | see also: 'callAuthnReq'
negotiateAuthnRequest ::
  (HasCallStack, MonadIO m, MonadReader TestEnv m) =>
  IdP ->
  m SAML.AuthnRequest
negotiateAuthnRequest idp =
  negotiateAuthnRequest' DoInitiateLogin idp id >>= \case
    (req, cky) -> case maybe (Left "missing") isDeleteBindCookie cky of
      Right () -> pure req
      Left msg -> error $ "unexpected bind cookie: " <> show (cky, msg)

doInitiatePath :: DoInitiate -> [ST]
doInitiatePath DoInitiateLogin = ["sso", "initiate-login"]
doInitiatePath DoInitiateBind = ["sso-initiate-bind"]

negotiateAuthnRequest' ::
  (HasCallStack, MonadIO m, MonadReader TestEnv m) =>
  DoInitiate ->
  IdP ->
  (Request -> Request) ->
  m (SAML.AuthnRequest, Maybe SetBindCookie)
negotiateAuthnRequest' (doInitiatePath -> doInit) idp modreq = do
  env <- ask
  resp :: ResponseLBS <-
    call $
      get
        ( modreq
            . (env ^. teSpar)
            . paths (cs <$> (doInit <> [idPIdToST $ idp ^. SAML.idpId]))
            . expect2xx
        )
  (_, authnreq) <- either error pure . parseAuthnReqResp $ cs <$> responseBody resp
  let wireCookie =
        SAML.SimpleSetCookie . Web.parseSetCookie
          <$> lookup "Set-Cookie" (responseHeaders resp)
  pure (authnreq, wireCookie)

submitAuthnResponse ::
  (HasCallStack, MonadIO m, MonadReader TestEnv m) =>
  SignedAuthnResponse ->
  m ResponseLBS
submitAuthnResponse = submitAuthnResponse' id

submitAuthnResponse' ::
  (HasCallStack, MonadIO m, MonadReader TestEnv m) =>
  (Request -> Request) ->
  SignedAuthnResponse ->
  m ResponseLBS
submitAuthnResponse' reqmod (SignedAuthnResponse authnresp) = do
  env <- ask
  req :: Request <-
    formDataBody [partLBS "SAMLResponse" . EL.encode . XML.renderLBS XML.def $ authnresp] empty
  call $ post' req (reqmod . (env ^. teSpar) . path "/sso/finalize-login/")

loginSsoUserFirstTime :: (HasCallStack, MonadIO m, MonadReader TestEnv m) => IdP -> SAML.SignPrivCreds -> m UserId
loginSsoUserFirstTime idp privCreds = do
  env <- ask
  authnReq <- negotiateAuthnRequest idp
  spmeta <- getTestSPMetadata
  authnResp <- runSimpleSP $ mkAuthnResponse privCreds idp spmeta authnReq True
  sparAuthnResp <- submitAuthnResponse authnResp
  let wireCookie = maybe (error (show sparAuthnResp)) id . lookup "Set-Cookie" $ responseHeaders sparAuthnResp
  accessResp :: ResponseLBS <-
    call $
      post ((env ^. teBrig) . path "/access" . header "Cookie" wireCookie . expect2xx)
  let uid :: UserId
      uid = Id . fromMaybe (error "bad user field in /access response body") . UUID.fromText $ uidRaw
      uidRaw :: HasCallStack => ST
      uidRaw = accessToken ^?! Aeson.key "user" . _String
      accessToken :: HasCallStack => Aeson.Value
      accessToken = tok
        where
          tok =
            either (error . ("parse error in /access response body: " <>)) id $
              Aeson.eitherDecode raw
          raw =
            fromMaybe (error "no body in /access response") $
              responseBody accessResp
  pure uid

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
    err :: forall n a. MonadIO n => ResponseLBS -> String -> n a
    err resp = liftIO . throwIO . ErrorCall . (<> ("; " <> show (responseBody resp)))

test_parseAuthnReqResp :: Bool
test_parseAuthnReqResp = isRight tst1
  where
    tst1 = parseAuthnReqResp @(Either String) (Just raw)
    _tst2 = XML.parseText XML.def raw
    raw = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\"><html xml:lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\"><body onload=\"document.forms[0].submit()\"><noscript><p><strong>Note:</strong>Since your browser does not support JavaScript, you must press the Continue button once to proceed.</p></noscript><form action=\"http://idp.net/sso/request\" method=\"post\"><input name=\"SAMLRequest\" type=\"hidden\" value=\"PHNhbWxwOkF1dGhuUmVxdWVzdCB4bWxuczpzYW1sYT0idXJuOm9hc2lzOm5hbWVzOnRjOlNBTUw6Mi4wOmFzc2VydGlvbiIgeG1sbnM6c2FtbG09InVybjpvYXNpczpuYW1lczp0YzpTQU1MOjIuMDptZXRhZGF0YSIgeG1sbnM6ZHM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvMDkveG1sZHNpZyMiIElEPSJpZGVhMDUwZmM0YzBkODQxNzJiODcwMjIzMmNlZmJiMGE3IiBJc3N1ZUluc3RhbnQ9IjIwMTgtMDctMDJUMTk6Mzk6MDYuNDQ3OTg3MVoiIFZlcnNpb249IjIuMCIgeG1sbnM6c2FtbHA9InVybjpvYXNpczpuYW1lczp0YzpTQU1MOjIuMDpwcm90b2NvbCI+PElzc3VlciB4bWxucz0idXJuOm9hc2lzOm5hbWVzOnRjOlNBTUw6Mi4wOmFzc2VydGlvbiI+aHR0cHM6Ly9hcHAud2lyZS5jb20vPC9Jc3N1ZXI+PC9zYW1scDpBdXRoblJlcXVlc3Q+\"/><noscript><input type=\"submit\" value=\"Continue\"/></noscript></form></body></html>"

parseAuthnReqResp ::
  forall n.
  MonadError String n =>
  Maybe LT ->
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

safeHead :: forall n a. (MonadError String n, Show a) => String -> [a] -> n a
safeHead _ (a : _) = pure a
safeHead msg [] = throwError $ msg <> ": []"

callAuthnReq' :: (MonadIO m, MonadHttp m) => SparReq -> SAML.IdPId -> m ResponseLBS
callAuthnReq' sparreq_ idpid = do
  get $ sparreq_ . path (cs $ "/sso/initiate-login/" -/ SAML.idPIdToST idpid)

callAuthnReqPrecheck' :: (MonadIO m, MonadHttp m) => SparReq -> SAML.IdPId -> m ResponseLBS
callAuthnReqPrecheck' sparreq_ idpid = do
  head $ sparreq_ . path (cs $ "/sso/initiate-login/" -/ SAML.idPIdToST idpid)

callIdpGet :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> SAML.IdPId -> m IdP
callIdpGet sparreq_ muid idpid = do
  resp <- callIdpGet' (sparreq_ . expect2xx) muid idpid
  either (liftIO . throwIO . ErrorCall . show) pure $
    responseJsonEither @IdP resp

callIdpGet' :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> SAML.IdPId -> m ResponseLBS
callIdpGet' sparreq_ muid idpid = do
  get $ sparreq_ . maybe id zUser muid . path (cs $ "/identity-providers/" -/ SAML.idPIdToST idpid)

callIdpGetRaw :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> SAML.IdPId -> m Text
callIdpGetRaw sparreq_ muid idpid = do
  resp <- callIdpGetRaw' (sparreq_ . expect2xx) muid idpid
  maybe (liftIO . throwIO $ ErrorCall "Nothing") (pure . cs) (responseBody resp)

callIdpGetRaw' :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> SAML.IdPId -> m ResponseLBS
callIdpGetRaw' sparreq_ muid idpid = do
  get $ sparreq_ . maybe id zUser muid . path (cs $ "/identity-providers/" -/ SAML.idPIdToST idpid -/ "raw")

callIdpGetAll :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> m IdPList
callIdpGetAll sparreq_ muid = do
  resp <- callIdpGetAll' (sparreq_ . expect2xx) muid
  either (liftIO . throwIO . ErrorCall . show) pure $
    responseJsonEither resp

callIdpGetAll' :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> m ResponseLBS
callIdpGetAll' sparreq_ muid = do
  get $ sparreq_ . maybe id zUser muid . path "/identity-providers"

callIdpCreate :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> SAML.IdPMetadata -> m IdP
callIdpCreate sparreq_ muid metadata = do
  resp <- callIdpCreate' (sparreq_ . expect2xx) muid metadata
  either (liftIO . throwIO . ErrorCall . show) pure $
    responseJsonEither @IdP resp

callIdpCreate' :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> SAML.IdPMetadata -> m ResponseLBS
callIdpCreate' sparreq_ muid metadata = do
  post $
    sparreq_
      . maybe id zUser muid
      . path "/identity-providers/"
      . body (RequestBodyLBS . cs $ SAML.encode metadata)
      . header "Content-Type" "application/xml"

callIdpCreateRaw :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> SBS -> LBS -> m IdP
callIdpCreateRaw sparreq_ muid ctyp metadata = do
  resp <- callIdpCreateRaw' (sparreq_ . expect2xx) muid ctyp metadata
  either (liftIO . throwIO . ErrorCall . show) pure $
    responseJsonEither @IdP resp

callIdpCreateRaw' :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> SBS -> LBS -> m ResponseLBS
callIdpCreateRaw' sparreq_ muid ctyp metadata = do
  post $
    sparreq_
      . maybe id zUser muid
      . path "/identity-providers/"
      . body (RequestBodyLBS metadata)
      . header "Content-Type" ctyp

callIdpCreateReplace :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> IdPMetadata -> IdPId -> m IdP
callIdpCreateReplace sparreq_ muid metadata idpid = do
  resp <- callIdpCreateReplace' (sparreq_ . expect2xx) muid metadata idpid
  either (liftIO . throwIO . ErrorCall . show) pure $
    responseJsonEither @IdP resp

callIdpCreateReplace' :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> IdPMetadata -> IdPId -> m ResponseLBS
callIdpCreateReplace' sparreq_ muid metadata idpid = do
  post $
    sparreq_
      . maybe id zUser muid
      . path "/identity-providers/"
      . body (RequestBodyLBS . cs $ SAML.encode metadata)
      . queryItem "replaces" (cs $ idPIdToST idpid)
      . header "Content-Type" "application/xml"

callIdpUpdate' :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> IdPId -> IdPMetadataInfo -> m ResponseLBS
callIdpUpdate' sparreq_ muid idpid (IdPMetadataValue metadata _) = do
  put $
    sparreq_
      . maybe id zUser muid
      . paths ["identity-providers", toByteString' $ idPIdToST idpid]
      . body (RequestBodyLBS $ cs metadata)
      . header "Content-Type" "application/xml"

callIdpDelete :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> SAML.IdPId -> m ()
callIdpDelete sparreq_ muid idpid = void $ callIdpDelete' (sparreq_ . expect2xx) muid idpid

callIdpDelete' :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> SAML.IdPId -> m ResponseLBS
callIdpDelete' sparreq_ muid idpid = do
  delete $
    sparreq_
      . maybe id zUser muid
      . path (cs $ "/identity-providers/" -/ SAML.idPIdToST idpid)

callIdpDeletePurge' :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> SAML.IdPId -> m ResponseLBS
callIdpDeletePurge' sparreq_ muid idpid = do
  delete $
    sparreq_
      . maybe id zUser muid
      . path (cs $ "/identity-providers/" -/ SAML.idPIdToST idpid)
      . queryItem "purge" "true"

callGetDefaultSsoCode :: (MonadIO m, MonadHttp m) => SparReq -> m ResponseLBS
callGetDefaultSsoCode sparreq_ = do
  get $
    sparreq_
      . path "/sso/settings/"

callSetDefaultSsoCode :: (MonadIO m, MonadHttp m) => SparReq -> SAML.IdPId -> m ResponseLBS
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

callDeleteDefaultSsoCode :: (MonadIO m, MonadHttp m) => SparReq -> m ResponseLBS
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
ssoToUidSpar :: (HasCallStack, MonadIO m, MonadReader TestEnv m) => Brig.UserSSOId -> m (Maybe UserId)
ssoToUidSpar ssoid = do
  ssoref <- either (error . ("could not parse UserRef: " <>)) pure $ Intra.fromUserSSOId ssoid
  runSparCass @Client $ Data.getSAMLUser ssoref

runSparCass ::
  (HasCallStack, m ~ Client, MonadIO m', MonadReader TestEnv m') =>
  m a ->
  m' a
runSparCass action = do
  env <- ask
  liftIO $ runClient (env ^. teCql) action

runSparCassWithEnv ::
  ( HasCallStack,
    m ~ ReaderT Data.Env (ExceptT TTLError Cas.Client),
    MonadIO m',
    MonadReader TestEnv m'
  ) =>
  m a ->
  m' a
runSparCassWithEnv action = do
  env <- ask
  denv <- Data.mkEnv <$> (pure $ env ^. teOpts) <*> liftIO getCurrentTime
  val <- runSparCass (runExceptT (action `runReaderT` denv))
  either (liftIO . throwIO . ErrorCall . show) pure val

runSimpleSP :: (MonadReader TestEnv m, MonadIO m) => SAML.SimpleSP a -> m a
runSimpleSP action = do
  env <- ask
  liftIO $ do
    ctx <- SAML.mkSimpleSPCtx (env ^. teOpts . to saml) []
    result <- SAML.runSimpleSP ctx action
    either (throwIO . ErrorCall . show) pure result

runSpar :: (MonadReader TestEnv m, MonadIO m) => Spar.Spar a -> m a
runSpar (Spar.Spar action) = do
  env <- (^. teSparEnv) <$> ask
  liftIO $ do
    result <- runExceptT $ action `runReaderT` env
    either (throwIO . ErrorCall . show) pure result

getSsoidViaSelf :: HasCallStack => UserId -> TestSpar UserSSOId
getSsoidViaSelf uid = maybe (error "not found") pure =<< getSsoidViaSelf' uid

getSsoidViaSelf' :: HasCallStack => UserId -> TestSpar (Maybe UserSSOId)
getSsoidViaSelf' uid = do
  musr <- aFewTimes (runSpar $ Intra.getBrigUser uid) isJust
  pure $ case userIdentity =<< musr of
    Just (SSOIdentity ssoid _ _) -> Just ssoid
    Just (FullIdentity _ _) -> Nothing
    Just (EmailIdentity _) -> Nothing
    Just (PhoneIdentity _) -> Nothing
    Nothing -> Nothing

getUserIdViaRef :: HasCallStack => UserRef -> TestSpar UserId
getUserIdViaRef uref = maybe (error "not found") pure =<< getUserIdViaRef' uref

getUserIdViaRef' :: HasCallStack => UserRef -> TestSpar (Maybe UserId)
getUserIdViaRef' uref = do
  aFewTimes (runSparCass $ Data.getSAMLUser uref) isJust
