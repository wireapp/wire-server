{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This is a sample application composed of the end-points in "SAML.WebSSO.API" plus a minimum of
-- functionality to make a running web application.  Some parts of this module could be handy to
-- build other apps, but it is more likely to serve as a tutorial.
module SAML2.WebSSO.API.Example where

import Control.Arrow ((&&&))
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Data.EitherR (fmapL)
import Data.Kind (Type)
import Data.Map as Map
import Data.Proxy
import Data.String.Conversions
import Data.UUID as UUID
import Data.Void (Void)
import GHC.Stack
import Network.Wai hiding (Response)
import SAML2.Util
import SAML2.WebSSO
import Servant.API hiding (URI (..))
import Servant.Server
import Text.Hamlet.XML
import Text.XML
import URI.ByteString

----------------------------------------------------------------------
-- a simple concrete monad

newtype SimpleSP a = SimpleSP (ReaderT SimpleSPCtx (ExceptT SimpleError IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader SimpleSPCtx, MonadError SimpleError)

data SimpleSPCtx = SimpleSPCtx
  { _spctxConfig :: Config,
    _spctxIdP :: [IdPConfig_],
    _spctxReq :: MVar RequestStore,
    _spctxAss :: MVar AssertionStore
  }

type RequestStore = Map.Map (ID AuthnRequest) (Issuer, Time)

type AssertionStore = Map.Map (ID Assertion) Time

makeLenses ''SimpleSPCtx

type MonadApp m = (GetAllIdPs SimpleError m, SPHandler SimpleError m)

-- | If you read the 'Config' initially in 'IO' and then pass it into the monad via 'Reader', you
-- safe disk load and redundant debug logs.
instance SPHandler SimpleError SimpleSP where
  type NTCTX SimpleSP = SimpleSPCtx
  nt ctx (SimpleSP m) = Handler . ExceptT . fmap (fmapL toServerError) . runExceptT $ m `runReaderT` ctx

runSimpleSP :: SimpleSPCtx -> SimpleSP a -> IO (Either SimpleError a)
runSimpleSP ctx (SimpleSP action) = runExceptT $ action `runReaderT` ctx

mkSimpleSPCtx :: Config -> [IdPConfig_] -> IO SimpleSPCtx
mkSimpleSPCtx cfg idps = SimpleSPCtx cfg idps <$> newMVar mempty <*> newMVar mempty

instance HasLogger SimpleSP where
  logger level msg = getConfig >>= \cfg -> SimpleSP (loggerIO (cfg ^. cfgLogLevel) level msg)

instance HasCreateUUID SimpleSP where
  createUUID = SimpleSP $ createUUIDIO

instance HasNow SimpleSP where
  getNow = SimpleSP $ getNowIO

simpleStoreID ::
  (MonadIO m, MonadReader ctx m) =>
  Lens' ctx (MVar (Map (ID a) Time)) ->
  ID a ->
  Time ->
  m ()
simpleStoreID sel item endOfLife = do
  store <- asks (^. sel)
  liftIO $ modifyMVar_ store (pure . simpleStoreID' item endOfLife)

simpleStoreID' :: ID a -> Time -> Map (ID a) Time -> Map (ID a) Time
simpleStoreID' = Map.insert

simpleUnStoreID ::
  (MonadIO m, MonadReader ctx m) =>
  Lens' ctx (MVar (Map (ID a) Time)) ->
  (ID a) ->
  m ()
simpleUnStoreID sel item = do
  store <- asks (^. sel)
  liftIO $ modifyMVar_ store (pure . simpleUnStoreID' item)

simpleUnStoreID' :: ID a -> Map (ID a) Time -> Map (ID a) Time
simpleUnStoreID' = Map.delete

simpleIsAliveID ::
  (MonadIO m, MonadReader ctx m, SP m) =>
  Lens' ctx (MVar (Map (ID a) Time)) ->
  ID a ->
  m Bool
simpleIsAliveID sel item = do
  now <- getNow
  store <- asks (^. sel)
  items <- liftIO $ readMVar store
  pure $ simpleIsAliveID' now item items

simpleIsAliveID' :: Time -> ID a -> Map (ID a) Time -> Bool
simpleIsAliveID' now item items = maybe False (>= now) (Map.lookup item items)

simpleStoreRequest ::
  (MonadIO m, MonadReader ctx m) =>
  Lens' ctx (MVar (Map (ID a) (Issuer, Time))) ->
  ID a ->
  Issuer ->
  Time ->
  m ()
simpleStoreRequest sel item issuer endOfLife = do
  store <- asks (^. sel)
  liftIO $ modifyMVar_ store (pure . simpleStoreRequest' item (issuer, endOfLife))

simpleStoreRequest' :: ID a -> (Issuer, Time) -> Map (ID a) (Issuer, Time) -> Map (ID a) (Issuer, Time)
simpleStoreRequest' = Map.insert

simpleUnStoreRequest ::
  (MonadIO m, MonadReader ctx m) =>
  Lens' ctx (MVar (Map (ID a) (Issuer, Time))) ->
  (ID a) ->
  m ()
simpleUnStoreRequest sel item = do
  store <- asks (^. sel)
  liftIO $ modifyMVar_ store (pure . simpleUnStoreRequest' item)

simpleUnStoreRequest' :: ID a -> Map (ID a) (Issuer, Time) -> Map (ID a) (Issuer, Time)
simpleUnStoreRequest' = Map.delete

simpleIsAliveRequest ::
  (MonadIO m, MonadReader ctx m, SP m) =>
  Lens' ctx (MVar (Map (ID a) (Issuer, Time))) ->
  ID a ->
  m Bool
simpleIsAliveRequest sel item = do
  now <- getNow
  store <- asks (^. sel)
  items <- liftIO $ readMVar store
  pure $ simpleIsAliveRequest' now item items

simpleIsAliveRequest' :: Time -> ID a -> Map (ID a) (Issuer, Time) -> Bool
simpleIsAliveRequest' now item items = maybe False ((>= now) . snd) (Map.lookup item items)

simpleGetRequest ::
  (MonadIO m, MonadReader ctx m, SP m) =>
  Lens' ctx (MVar (Map (ID a) (Issuer, Time))) ->
  ID a ->
  m (Maybe (Issuer, Time))
simpleGetRequest sel item = do
  store <- asks (^. sel)
  items <- liftIO $ readMVar store
  pure $ Map.lookup item items

instance SPStoreRequest AuthnRequest SimpleSP where
  storeRequest = simpleStoreRequest spctxReq
  unStoreRequest = simpleUnStoreRequest spctxReq
  isAliveRequest = simpleIsAliveRequest spctxReq
  getRequest = simpleGetRequest spctxReq

instance SPStoreAssertion Assertion SimpleSP where
  storeAssertionInternal = simpleStoreID spctxAss
  unStoreAssertion = simpleUnStoreID spctxAss
  isAliveAssertion = simpleIsAliveID spctxAss

instance HasConfig SimpleSP where
  getConfig = (^. spctxConfig) <$> SimpleSP ask

instance SPStoreIdP SimpleError SimpleSP where
  type IdPConfigExtra SimpleSP = ()
  type IdPConfigSPId SimpleSP = Void
  storeIdPConfig _ = error "instance SPStoreIdP SimpleError SimpleSP: storeIdPConfig not implemented."
  getIdPConfig = simpleGetIdPConfigBy (asks (^. spctxIdP)) (^. idpId)
  getIdPConfigByIssuerOptionalSPId issuer _ = simpleGetIdPConfigBy (asks (^. spctxIdP)) (^. idpMetadata . edIssuer) issuer

simpleGetIdPConfigBy ::
  (MonadError (Error err) m, HasConfig m, Show a, Ord a) =>
  m [IdPConfig_] ->
  (IdPConfig_ -> a) ->
  a ->
  m IdPConfig_
simpleGetIdPConfigBy getIdps mkkey idpname = do
  idps <- getIdps
  maybe crash' pure . Map.lookup idpname $ mkmap idps
  where
    crash' = throwError (UnknownIdP . cs . show $ idpname)
    mkmap = Map.fromList . fmap (mkkey &&& id)

class (SPStoreIdP err m) => GetAllIdPs err m where
  getAllIdPs :: m [IdPConfig (IdPConfigExtra m)]

instance GetAllIdPs SimpleError SimpleSP where
  getAllIdPs = asks (^. spctxIdP)

----------------------------------------------------------------------
-- the app

-- | The most straight-forward 'Application' that can be constructed from 'api', 'API'.
app :: Config -> [IdPConfig_] -> IO Application
app cfg idps = app' (Proxy @SimpleSP) =<< mkSimpleSPCtx cfg idps

app' ::
  forall (m :: Type -> Type).
  (SP m, MonadApp m) =>
  Proxy m ->
  NTCTX m ->
  IO Application
app' Proxy ctx = do
  let served :: Application
      served =
        serve
          (Proxy @APPAPI)
          (hoistServer (Proxy @APPAPI) (nt @SimpleError @m ctx) appapi :: Server APPAPI)
  pure . setHttpCachePolicy $ served

type SPAPI =
  Header "Cookie" Cky :> Get '[HTML] LoginStatus
    :<|> "logout" :> "local" :> GetRedir '[HTML] (WithCookieAndLocation ST)
    :<|> "logout" :> "single" :> GetRedir '[HTML] (WithCookieAndLocation ST)

type APPAPI =
  "sp" :> SPAPI
    :<|> "sso" :> API

spapi :: (MonadApp m) => ServerT SPAPI m
spapi = loginStatus :<|> localLogout :<|> singleLogout

appapi :: (MonadApp m) => ServerT APPAPI m
appapi = spapi :<|> api "toy-sp" (HandleVerdictRedirect (simpleOnSuccess SubjectFoldCase))

loginStatus :: (GetAllIdPs err m, SP m) => Maybe Cky -> m LoginStatus
loginStatus cookie = do
  idpids <- getAllIdPs
  loginOpts <- mkLoginOption `mapM` idpids
  logoutPath <- getPath' SpPathLocalLogout
  pure $ maybe (NotLoggedIn loginOpts) (LoggedInAs logoutPath . cs . setSimpleCookieValue) cookie

mkLoginOption :: (Monad m, SP m) => IdPConfig a -> m (ST, ST)
mkLoginOption icfg = (renderURI $ icfg ^. idpMetadata . edIssuer . fromIssuer,) <$> getPath' (SsoPathAuthnReq (icfg ^. idpId))

-- | only logout on this SP.
localLogout :: (SPHandler SimpleError m) => m (WithCookieAndLocation ST)
localLogout = do
  uri <- getPath SpPathHome
  cky <- toggleCookie "/" Nothing
  pure . addHeader cky . addHeader uri $ "Logged out locally, redirecting to " <> renderURI uri

-- | acts weird (handles /sso/meta path)
singleLogout :: (HasCallStack, SP m, Applicative m) => m (WithCookieAndLocation ST)
singleLogout =
  -- if we just say "error" instead of "pure . error" here, the routing algorithm in servant
  -- will construct the handler and crash, even if the route for the handler does not match
  -- the path in the request.
  pure $ error "not implemented: singleLogout"

data LoginStatus
  = NotLoggedIn [(ST {- issuer -}, ST {- authreq path -})]
  | LoggedInAs ST ST
  deriving (Eq, Show)

instance MimeRender HTML LoginStatus where
  mimeRender Proxy (NotLoggedIn loginOpts) =
    mkHtml
      [xml|
        <body>
          [not logged in]
          $forall loginOpt <- loginOpts
            ^{mkform loginOpt}
      |]
    where
      mkform :: (ST, ST) -> [Node]
      mkform (issuer, path) =
        [xml|
            <form action=#{path} method="get">
              <input type="submit" value="log in via #{issuer}">
          |]
  mimeRender Proxy (LoggedInAs logoutPath name) =
    mkHtml
      [xml|
        <body>
        [logged in as #{name}]
          <form action=#{logoutPath} method="get">
            <input type="submit" value="logout">
          <p>
            (this is local logout; logout via IdP is not implemented.)
      |]

----------------------------------------------------------------------
-- uri paths

data Path
  = SpPathHome
  | SpPathLocalLogout
  | SpPathSingleLogout
  | SsoPathMeta IdPId
  | SsoPathAuthnReq IdPId
  | SsoPathAuthnResp IdPId
  deriving (Eq, Show)

getPath' :: forall m. (Monad m, HasConfig m) => Path -> m ST
getPath' = fmap renderURI . getPath

getPath :: forall m. (Monad m, HasConfig m) => Path -> m URI
getPath path = do
  cfg <- getConfig
  let sp, sso :: ST -> URI
      sp = ((cfg ^. cfgSPAppURI) =/)
      sso = ((cfg ^. cfgSPSsoURI) =/)
      withidp :: IdPId -> URI -> URI
      withidp (IdPId uuid) = (=/ UUID.toText uuid)
  pure $ case path of
    SpPathHome -> sp ""
    SpPathLocalLogout -> sp "/logout/local"
    SpPathSingleLogout -> sp "/logout/single"
    SsoPathMeta ip -> withidp ip $ sso "/meta"
    SsoPathAuthnReq ip -> withidp ip $ sso "/authreq"
    SsoPathAuthnResp ip -> withidp ip $ sso "/authresp"
