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
  ( TestEnv(..), teMgr, teCql, teBrig, teGalley, teSpar, teNewIdp, teMockIdp, teOpts
  , Select, mkEnv, it, pending, pendingWith
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
  , sampleIdP
  , samplePublicKey1
  , samplePublicKey2
  , responseJSON
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
import Lens.Micro
import Spar.API ()
import Spar.Options as Options
import Spar.Run
import Spar.Types
import System.Random (randomRIO)
import Test.Hspec hiding (it, xit, pending, pendingWith)
import URI.ByteString
import URI.ByteString.QQ
import Util.MockIdP
import Util.Options
import Util.Types

import qualified Brig.Types.Activation as Brig
import qualified Brig.Types.User as Brig
import qualified Brig.Types.User.Auth as Brig
import qualified Data.ByteString.Base64.Lazy as EL
import qualified Data.Text.Ascii as Ascii
import qualified Data.X509 as X509
import qualified Galley.Types.Teams as Galley
import qualified SAML2.WebSSO as SAML
import qualified Test.Hspec
import qualified Text.XML as XML
import qualified Text.XML.Cursor as XML
import qualified Text.XML.DSig as SAML
import qualified Text.XML.Util as SAML


mkEnv :: IntegrationConfig -> Opts -> IO TestEnv
mkEnv integrationOpts _teOpts = do
  _teMgr :: Manager <- newManager defaultManagerSettings
  _teCql :: ClientState <- initCassandra _teOpts =<< mkLogger _teOpts
  let mkreq :: (IntegrationConfig -> Endpoint) -> (Request -> Request)
      mkreq selector = Bilge.host (selector integrationOpts ^. epHost . to cs)
                     . Bilge.port (selector integrationOpts ^. epPort)

      _teBrig    = mkreq cfgBrig
      _teGalley  = mkreq cfgGalley
      _teSpar    = mkreq cfgSpar
      _teNewIdp  = cfgNewIdp integrationOpts
      _teMockIdp = cfgMockIdp integrationOpts

  pure $ TestEnv {..}

it :: m ~ IO
       -- or, more generally:
       -- MonadIO m, Example (TestEnv -> m ()), Arg (TestEnv -> m ()) ~ TestEnv
   => String -> ReaderT TestEnv m () -> SpecWith TestEnv
it msg bdy = Test.Hspec.it msg $ runReaderT bdy

pending :: MonadIO m => m ()
pending = liftIO Test.Hspec.pending

pendingWith :: MonadIO m => String -> m ()
pendingWith = liftIO . Test.Hspec.pendingWith


createUserWithTeam :: (HasCallStack, MonadHttp m, MonadIO m) => BrigReq -> GalleyReq -> m (UserId, TeamId)
createUserWithTeam brg gly = do
    e <- randomEmail
    n <- UUID.toString <$> liftIO UUID.nextRandom
    let p = RequestBodyLBS . encode $ object
            [ "name"            .= n
            , "email"           .= Brig.fromEmail e
            , "password"        .= defPassword
            , "team"            .= newTeam
            ]
    bdy <- decodeBody <$> post (brg . path "/i/users" . contentJson . body p)
    let (Just uid, Just (Just tid)) = (Brig.userId <$> bdy, Brig.userTeam <$> bdy)
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
  nobody :: UserId
    <- maybe (throwM $ ErrorCall "createTeamMember: failed to parse response")
             (pure . Brig.userId)
             (decodeBody @Brig.User resp)
  let tmem :: Galley.TeamMember = Galley.newTeamMember nobody perms
  addTeamMember galleyreq teamid (Galley.newNewTeamMember tmem)
  pure nobody

addTeamMember :: (HasCallStack, MonadCatch m, MonadIO m, MonadHttp m)
              => GalleyReq -> TeamId -> Galley.NewTeamMember -> m ()
addTeamMember galleyreq tid mem =
    void $ post ( galleyreq
                . paths ["i", "teams", toByteString' tid, "members"]
                . contentJson
                . expect2xx
                . lbytes (encode mem)
                )

createRandomPhoneUser :: (HasCallStack, MonadCatch m, MonadIO m, MonadHttp m) => BrigReq -> m (UserId, Brig.Phone)
createRandomPhoneUser brig_ = do
    usr <- randomUser brig_
    let uid = Brig.userId usr
    phn <- liftIO randomPhone
    -- update phone
    let phoneUpdate = RequestBodyLBS . encode $ Brig.PhoneUpdate phn
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
        const (Just phn) === (Brig.userPhone <=< decodeBody)

    return (uid, phn)

decodeBody :: (HasCallStack, FromJSON a) => ResponseLBS -> Maybe a
decodeBody = responseBody >=> decode'

getTeams :: (HasCallStack, MonadHttp m, MonadIO m) => UserId -> GalleyReq -> m Galley.TeamList
getTeams u gly = do
    r <- get ( gly
             . paths ["teams"]
             . zAuthAccess u "conn"
             . expect2xx
             )
    return $ fromMaybe (error "getTeams: failed to parse response") (decodeBody r)

getSelfProfile :: (HasCallStack, MonadHttp m, MonadIO m) => BrigReq -> UserId -> m Brig.SelfProfile
getSelfProfile brg usr = do
    rsp <- get $ brg . path "/self" . zUser usr
    return $ fromMaybe (error $ "getSelfProfile: failed to decode: " ++ show rsp) (decodeBody rsp)

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
    return $ fromMaybe (error "createUser: failed to parse response") (decodeBody r)

-- more flexible variant of 'createUser' (see above).
postUser :: (HasCallStack, MonadIO m, MonadHttp m)
         => ST -> Maybe ST -> Maybe Brig.UserSSOId -> Maybe TeamId -> BrigReq -> m ResponseLBS
postUser name email ssoid teamid brig_ = do
    email' <- maybe (pure Nothing) (fmap (Just . Brig.fromEmail) . mkEmailRandomLocalSuffix) email
    let p = RequestBodyLBS . encode $ object
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

call :: Http a -> ReaderT TestEnv IO a
call req = ask >>= \env -> liftIO $ runHttpT (env ^. teMgr) req

ping :: (Request -> Request) -> Http ()
ping req = void . get $ req . path "/i/status" . expect2xx


createTestIdP :: (HasCallStack, MonadReader TestEnv m, MonadIO m) => m (UserId, TeamId, SAML.IdPId)
createTestIdP = do
  env <- ask
  liftIO . runHttpT (env ^. teMgr) $ do
    (uid, tid) <- createUserWithTeam (env ^. teBrig) (env ^. teGalley)
    (uid, tid,) . (^. SAML.idpId) <$> callIdpCreate (env ^. teSpar) (Just uid) sampleIdP

-- TODO: sampleIdP must be the data for our MockIdP
-- TODO add 'Chan's for optionally diverging from the happy path (for testing validation)

sampleIdP :: HasCallStack => NewIdP
sampleIdP = NewIdP
  { _nidpMetadata        = [uri|http://idp.net/meta|]
  , _nidpIssuer          = SAML.Issuer [uri|http://idp.net/|]
  , _nidpRequestUri      = [uri|http://idp.net/sso/request|]
  , _nidpPublicKey       = samplePublicKey1
  }

samplePublicKey1 :: X509.SignedCertificate
samplePublicKey1 = either (error . show) id $ SAML.parseKeyInfo "<KeyInfo xmlns=\"http://www.w3.org/2000/09/xmldsig#\"><X509Data><X509Certificate>MIIDBTCCAe2gAwIBAgIQev76BWqjWZxChmKkGqoAfDANBgkqhkiG9w0BAQsFADAtMSswKQYDVQQDEyJhY2NvdW50cy5hY2Nlc3Njb250cm9sLndpbmRvd3MubmV0MB4XDTE4MDIxODAwMDAwMFoXDTIwMDIxOTAwMDAwMFowLTErMCkGA1UEAxMiYWNjb3VudHMuYWNjZXNzY29udHJvbC53aW5kb3dzLm5ldDCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAMgmGiRfLh6Fdi99XI2VA3XKHStWNRLEy5Aw/gxFxchnh2kPdk/bejFOs2swcx7yUWqxujjCNRsLBcWfaKUlTnrkY7i9x9noZlMrijgJy/Lk+HH5HX24PQCDf+twjnHHxZ9G6/8VLM2e5ZBeZm+t7M3vhuumEHG3UwloLF6cUeuPdW+exnOB1U1fHBIFOG8ns4SSIoq6zw5rdt0CSI6+l7b1DEjVvPLtJF+zyjlJ1Qp7NgBvAwdiPiRMU4l8IRVbuSVKoKYJoyJ4L3eXsjczoBSTJ6VjV2mygz96DC70MY3avccFrk7tCEC6ZlMRBfY1XPLyldT7tsR3EuzjecSa1M8CAwEAAaMhMB8wHQYDVR0OBBYEFIks1srixjpSLXeiR8zES5cTY6fBMA0GCSqGSIb3DQEBCwUAA4IBAQCKthfK4C31DMuDyQZVS3F7+4Evld3hjiwqu2uGDK+qFZas/D/eDunxsFpiwqC01RIMFFN8yvmMjHphLHiBHWxcBTS+tm7AhmAvWMdxO5lzJLS+UWAyPF5ICROe8Mu9iNJiO5JlCo0Wpui9RbB1C81Xhax1gWHK245ESL6k7YWvyMYWrGqr1NuQcNS0B/AIT1Nsj1WY7efMJQOmnMHkPUTWryVZlthijYyd7P2Gz6rY5a81DAFqhDNJl2pGIAE6HWtSzeUEh3jCsHEkoglKfm4VrGJEuXcALmfCMbdfTvtu4rlsaP2hQad+MG/KJFlenoTK34EMHeBPDCpqNDz8UVNk</X509Certificate></X509Data></KeyInfo>"

samplePublicKey2 :: X509.SignedCertificate
samplePublicKey2 = either (error . show) id $ SAML.parseKeyInfo "<ds:KeyInfo xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\"><ds:X509Data><ds:X509Certificate>MIIDpDCCAoygAwIBAgIGAWOMMryDMA0GCSqGSIb3DQEBCwUAMIGSMQswCQYDVQQGEwJVUzETMBEGA1UECAwKQ2FsaWZvcm5pYTEWMBQGA1UEBwwNU2FuIEZyYW5jaXNjbzENMAsGA1UECgwET2t0YTEUMBIGA1UECwwLU1NPUHJvdmlkZXIxEzARBgNVBAMMCmRldi02MDc2NDgxHDAaBgkqhkiG9w0BCQEWDWluZm9Ab2t0YS5jb20wHhcNMTgwNTIzMDg1MTA1WhcNMjgwNTIzMDg1MjA1WjCBkjELMAkGA1UEBhMCVVMxEzARBgNVBAgMCkNhbGlmb3JuaWExFjAUBgNVBAcMDVNhbiBGcmFuY2lzY28xDTALBgNVBAoMBE9rdGExFDASBgNVBAsMC1NTT1Byb3ZpZGVyMRMwEQYDVQQDDApkZXYtNjA3NjQ4MRwwGgYJKoZIhvcNAQkBFg1pbmZvQG9rdGEuY29tMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA2HkpOuMhVFUCptrVB/Zm36cuFM+YMQjKdtqEoBJDLbtSbb7uFuvm5rMJ+1VSK5GKAM/Bec5WXTE2WMkifK5JaGOLS7q8+pgiWmqKE3KHMUmLAioe/1jzHkCobxis0FIVhyarRY97w0VMbDGzhPiU7pEopYpicJBzRL2UrzR+PebGgllvnaPzlg8ePtr9/xMv0QTJlYEyCctO4vT5Qa5Xlfek3Ox5yMJM1JPXzn7yuJN5R/Nf8jFprsdBSxNMzkcTRFGy8as2GCt/Xh9H+ef4CxSgRK5UXcUCrb5YMnBehEp2YiuWtw8QsGRR8elgnF3Uw9J2xEDkZIhurPy8OYmGNQIDAQABMA0GCSqGSIb3DQEBCwUAA4IBAQA7kxxg2aVjo7Oml83bUWk4UtaQKYMEY74mygG/JV09g1DVMAPAyjaaMFamDSjortKarMQ3ET5tj2DggQBsWQNzsr3iZkmijab8JLwzA2+I1q63S68OaW5uaR5iMR8zZCTh/fWWYqa1AP64XeGHp+RLGfbp/eToNfkQWu7fH2QtDMOeLe5VmIV9pOFHnySszoR/epMd3sdDLVgmz4qbrMTBWD+5rxWdYS2glmRXl7IIQHrdBTRMll7S6ks5prqKFTwfPvZVrTnzD83a39wl2jBJhOQLjmSfSwP9H0YFNb/NRaDbSDS7BPuAlotZsaPZIN95tu+t9wmFwdxcVG/9q/Vu</ds:X509Certificate></ds:X509Data></ds:KeyInfo>"


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

callIdpGet :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> SAML.IdPId -> m IdP
callIdpGet sparreq_ muid idpid = do
  resp <- callIdpGet' (sparreq_ . expect2xx) muid idpid
  either (liftIO . throwIO . ErrorCall . show) pure
    $ responseJSON @IdP resp

callIdpGet' :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> SAML.IdPId -> m ResponseLBS
callIdpGet' sparreq_ muid idpid = do
  get $ sparreq_ . maybe id zUser muid . path ("/identity-providers/" <> cs (SAML.idPIdToST idpid))

callIdpCreate :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> NewIdP -> m IdP
callIdpCreate sparreq_ muid newidp = do
  resp <- callIdpCreate' (sparreq_ . expect2xx) muid newidp
  either (liftIO . throwIO . ErrorCall . show) pure
    $ responseJSON @IdP resp

callIdpCreate' :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> NewIdP -> m ResponseLBS
callIdpCreate' sparreq_ muid newidp = do
  post $ sparreq_ . maybe id zUser muid . path "/identity-providers/" . json newidp

callIdpDelete :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> SAML.IdPId -> m ()
callIdpDelete sparreq_ muid idpid = void $ callIdpDelete' (sparreq_ . expect2xx) muid idpid

callIdpDelete' :: (MonadIO m, MonadHttp m) => SparReq -> Maybe UserId -> SAML.IdPId -> m ResponseLBS
callIdpDelete' sparreq_ muid idpid = do
  delete $ sparreq_ . maybe id zUser muid . path ("/identity-providers/" <> cs (SAML.idPIdToST idpid))
