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
import Data.Map as Map
import Data.Proxy
import Data.String.Conversions
import Data.Void (Void)
import SAML2.WebSSO
import Servant.API hiding (URI (..))
import Servant.Server
import Text.Hamlet.XML
import Text.XML

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
  Lens' ctx (MVar RequestStore) ->
  ID AuthnRequest ->
  Issuer ->
  Time ->
  m ()
simpleStoreRequest sel item issuer endOfLife = do
  store <- asks (^. sel)
  liftIO $ modifyMVar_ store (pure . simpleStoreRequest' item (issuer, endOfLife))

simpleStoreRequest' :: ID AuthnRequest -> (Issuer, Time) -> RequestStore -> RequestStore
simpleStoreRequest' = Map.insert

simpleUnStoreRequest ::
  (MonadIO m, MonadReader ctx m) =>
  Lens' ctx (MVar RequestStore) ->
  (ID AuthnRequest) ->
  m ()
simpleUnStoreRequest sel item = do
  store <- asks (^. sel)
  liftIO $ modifyMVar_ store (pure . simpleUnStoreRequest' item)

simpleUnStoreRequest' :: ID AuthnRequest -> RequestStore -> RequestStore
simpleUnStoreRequest' = Map.delete

simpleGetIdpIssuer ::
  (MonadIO m, MonadReader ctx m, SP m) =>
  Lens' ctx (MVar RequestStore) ->
  ID AuthnRequest ->
  m (Maybe Issuer)
simpleGetIdpIssuer sel item = do
  store <- asks (^. sel)
  items <- liftIO $ readMVar store
  simpleGetIdpIssuer' item items <$> getNow

simpleGetIdpIssuer' ::
  ID AuthnRequest ->
  RequestStore ->
  Time ->
  (Maybe Issuer)
simpleGetIdpIssuer' item items now = case Map.lookup item items of
  Just (issuer, expiresAt) | now < expiresAt -> Just issuer
  _ -> Nothing

instance SPStoreRequest AuthnRequest SimpleSP where
  storeRequest = simpleStoreRequest spctxReq
  unStoreRequest = simpleUnStoreRequest spctxReq
  getIdpIssuer = simpleGetIdpIssuer spctxReq

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

type SPAPI =
  Header "Cookie" Cky :> Get '[HTML] LoginStatus
    :<|> "logout" :> "local" :> GetRedir '[HTML] (WithCookieAndLocation ST)
    :<|> "logout" :> "single" :> GetRedir '[HTML] (WithCookieAndLocation ST)

type APPAPI =
  "sp" :> SPAPI
    :<|> "sso" :> API

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
