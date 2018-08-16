{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
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
module Util
  ( TestEnv(..), teMgr, teCql, teBrig, teGalley, teSpar, teNewIdP, teIdPEndpoint, teUserId, teTeamId, teIdP, teIdPHandle, teOpts, teTstOpts
  , Select, mkEnv, destroyEnv, it, pending, pendingWith
  , IntegrationConfig(..)
  , BrigReq
  , GalleyReq
  , SparReq
  , ResponseLBS
  , TestErrorLabel(..)
  , createUserWithTeam
  , createTeamMember
  , createRandomPhoneUser
  , zUser
  , shouldRespondWith
  , call
  , ping
  , createTestIdP
  , negotiateAuthnRequest
  , submitAuthnResponse
  , responseJSON
  , callAuthnReqPrecheck'
  , callAuthnReq, callAuthnReq'
  , callIdpGet, callIdpGet'
  , callIdpCreate, callIdpCreate'
  , callIdpDelete, callIdpDelete'
  , initCassandra
  , module Test.Hspec
  , module Util.MockIdP
  ) where

import Bilge
import Bilge.Assert ((!!!), (===), (<!!))
import Cassandra as Cas
import Control.Exception
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson as Aeson hiding (json)
import Data.Aeson.Lens as Aeson
import Data.ByteString.Conversion
import Data.Either
import Data.EitherR (fmapL)
import Data.Id
import Data.Maybe
import Data.Misc (PlainTextPassword(..))
import Data.Range
import Data.String.Conversions
import Data.UUID as UUID hiding (null, fromByteString)
import Data.UUID.V4 as UUID (nextRandom)
import GHC.Stack (HasCallStack)
import Network.HTTP.Client.MultipartFormData
import Lens.Micro
import Prelude hiding (head)
import SAML2.WebSSO
import SAML2.WebSSO.Test.Credentials
import SAML2.WebSSO.Test.MockResponse
import Spar.API ()
import Spar.Options as Options
import Spar.Run
import Spar.Types
import System.Random (randomRIO)
import Test.Hspec hiding (it, xit, pending, pendingWith)
import URI.ByteString
import Util.MockIdP
import Util.Options
import Util.Types

import qualified Brig.Types.Activation as Brig
import qualified Brig.Types.User as Brig
import qualified Brig.Types.User.Auth as Brig
import qualified Control.Concurrent.Async as Async
import qualified Data.ByteString.Base64.Lazy as EL
import qualified Data.Text.Ascii as Ascii
import qualified Galley.Types.Teams as Galley
import qualified Network.Wai.Handler.Warp as Warp
import qualified SAML2.WebSSO as SAML
import qualified Test.Hspec
import qualified Text.XML as XML
import qualified Text.XML.Cursor as XML
import qualified Text.XML.DSig as SAML
import qualified Text.XML.Util as SAML


mkEnv :: HasCallStack => IntegrationConfig -> Opts -> IO TestEnv
mkEnv _teTstOpts _teOpts = do
  _teMgr :: Manager <- newManager defaultManagerSettings
  _teCql :: ClientState <- initCassandra _teOpts =<< mkLogger _teOpts
  let mkreq :: (IntegrationConfig -> Endpoint) -> (Request -> Request)
      mkreq selector = Bilge.host (selector _teTstOpts ^. epHost . to cs)
                     . Bilge.port (selector _teTstOpts ^. epPort)

      _teBrig    = mkreq cfgBrig
      _teGalley  = mkreq cfgGalley
      _teSpar    = mkreq cfgSpar

      _teNewIdP  = cfgNewIdp _teTstOpts
      _teIdPEndpoint = cfgMockIdp _teTstOpts

  _teIdPHandle <- let app = serveSampleIdP _teNewIdP
                      srv = Warp.runSettings (endpointToSettings _teIdPEndpoint) app
                  in Async.async srv

  (_teUserId, _teTeamId, _teIdP) <- createTestIdPFrom _teNewIdP _teMgr _teBrig _teGalley _teSpar

  pure TestEnv {..}

destroyEnv :: HasCallStack => TestEnv -> IO ()
destroyEnv = Async.cancel . (^. teIdPHandle)


it :: (HasCallStack, m ~ IO)
       -- or, more generally:
       -- MonadIO m, Example (TestEnv -> m ()), Arg (TestEnv -> m ()) ~ TestEnv
   => String -> ReaderT TestEnv m () -> SpecWith TestEnv
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
    <- postUser name Nothing (Just ssoid) (Just teamid) brigreq
       <!! const 201 === statusCode
  let nobody :: UserId            = Brig.userId (decodeBody' @Brig.User resp)
      tmem   :: Galley.TeamMember = Galley.newTeamMember nobody perms
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
    createUser n "success@simulator.amazonses.com" brig_

createUser :: (HasCallStack, MonadCatch m, MonadIO m, MonadHttp m)
           => ST -> ST -> BrigReq -> m Brig.User
createUser name email brig_ = do
    r <- postUser name (Just email) Nothing Nothing brig_ <!! const 201 === statusCode
    return $ decodeBody' r

-- more flexible variant of 'createUser' (see above).
postUser :: (HasCallStack, MonadIO m, MonadHttp m)
         => ST -> Maybe ST -> Maybe Brig.UserSSOId -> Maybe TeamId -> BrigReq -> m ResponseLBS
postUser name email ssoid teamid brig_ = do
    email' <- maybe (pure Nothing) (fmap (Just . Brig.fromEmail) . mkEmailRandomLocalSuffix) email
    let p = RequestBodyLBS . Aeson.encode $ object
            [ "name"            .= name
            , "email"           .= email'
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

mkEmailRandomLocalSuffix :: MonadIO m => ST -> m Brig.Email
mkEmailRandomLocalSuffix e = do
    uid <- liftIO UUID.nextRandom
    case Brig.parseEmail e of
        Just (Brig.Email loc dom) -> return $ Brig.Email (loc <> "+" <> UUID.toText uid) dom
        Nothing              -> fail $ "Invalid email address: " ++ cs e

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


-- spar specifics

shouldRespondWith :: forall a. (HasCallStack, Show a, Eq a)
                  => Http a -> (a -> Bool) -> ReaderT TestEnv IO ()
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


-- | Create new user, team, idp from 'sampleIdP'.  The issuer id can be used to do this several
-- times without getting the error that this issuer is already used for another team.
createTestIdP :: (HasCallStack, MonadReader TestEnv m, MonadIO m) => UUID -> m (UserId, TeamId, IdP)
createTestIdP issuerid = do
  env <- ask
  let sampleNewIdP = sampleIdP (mkurl "/meta") (mkurl "/resp")
        & nidpIssuer .~ Issuer (mkurl $ "/_" <> UUID.toText issuerid)
      Endpoint ephost (cs . show -> epport) = env ^. teTstOpts . to cfgMockIdp
      mkurl = either (error . show) id . SAML.parseURI' . (("http://" <> ephost <> ":" <> epport) <>)
  createTestIdPFrom sampleNewIdP (env ^. teMgr) (env ^. teBrig) (env ^. teGalley) (env ^. teSpar)

-- | Create new user, team, idp from given 'NewIdP'.
createTestIdPFrom :: (HasCallStack, MonadIO m)
                  => NewIdP -> Manager -> BrigReq -> GalleyReq -> SparReq -> m (UserId, TeamId, IdP)
createTestIdPFrom newidp mgr brig galley spar = do
  liftIO . runHttpT mgr $ do
    (uid, tid) <- createUserWithTeam brig galley
    (uid, tid,) <$> callIdpCreate spar (Just uid) newidp

negotiateAuthnRequest :: (HasCallStack, MonadIO m, MonadReader TestEnv m)
                      => m (IdP, SAML.SignPrivCreds, SAML.AuthnRequest)
negotiateAuthnRequest = do
  env <- ask
  let idp = env ^. teIdP
  resp :: ResponseLBS
    <- call $ get
           ( (env ^. teSpar)
           . path ("/sso/initiate-login/" <> (cs . UUID.toText . fromIdPId . (^. SAML.idpId) $ idp))
           . expect2xx
           )
  (_, authnreq) <- either error pure . parseAuthnReqResp $ cs <$> responseBody resp
  pure (idp, sampleIdPPrivkey, authnreq)


submitAuthnResponse :: (HasCallStack, MonadIO m, MonadReader TestEnv m)
                    => SignedAuthnResponse -> m ResponseLBS
submitAuthnResponse (SignedAuthnResponse authnresp) = do
  env <- ask
  req :: Request
    <- formDataBody [partLBS "SAMLResponse" . EL.encode . XML.renderLBS XML.def $ authnresp] empty
  call $ post' req ((env ^. teSpar) . path "/sso/finalize-login")


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
  get $ sparreq_ . path ("/sso/initiate-login/" <> cs (SAML.idPIdToST idpid))

callAuthnReqPrecheck' :: (MonadIO m, MonadHttp m) => SparReq -> SAML.IdPId -> m ResponseLBS
callAuthnReqPrecheck' sparreq_ idpid = do
  head $ sparreq_ . path ("/sso/initiate-login/" <> cs (SAML.idPIdToST idpid))

callIdpGet :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> SAML.IdPId -> m IdP
callIdpGet sparreq_ muid idpid = do
  resp <- callIdpGet' (sparreq_ . expect2xx) muid idpid
  either (liftIO . throwIO . ErrorCall . show) pure
    $ responseJSON @IdP resp

callIdpGet' :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> SAML.IdPId -> m ResponseLBS
callIdpGet' sparreq_ muid idpid = do
  get $ sparreq_ . maybe id zUser muid . path ("/identity-providers/" <> cs (SAML.idPIdToST idpid))

callIdpCreate :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> SAML.NewIdP -> m IdP
callIdpCreate sparreq_ muid newidp = do
  resp <- callIdpCreate' (sparreq_ . expect2xx) muid newidp
  either (liftIO . throwIO . ErrorCall . show) pure
    $ responseJSON @IdP resp

callIdpCreate' :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> SAML.NewIdP -> m ResponseLBS
callIdpCreate' sparreq_ muid newidp = do
  post $ sparreq_ . maybe id zUser muid . path "/identity-providers/" . json newidp

callIdpDelete :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> SAML.IdPId -> m ()
callIdpDelete sparreq_ muid idpid = void $ callIdpDelete' (sparreq_ . expect2xx) muid idpid

callIdpDelete' :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> SAML.IdPId -> m ResponseLBS
callIdpDelete' sparreq_ muid idpid = do
  delete $ sparreq_ . maybe id zUser muid . path ("/identity-providers/" <> cs (SAML.idPIdToST idpid))
