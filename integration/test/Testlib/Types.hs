module Testlib.Types where

import Control.Exception as E
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as L
import Data.CaseInsensitive qualified as CI
import Data.Default
import Data.Function ((&))
import Data.Functor
import Data.Hex
import Data.IORef
import Data.List
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.Records
import GHC.Stack
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types qualified as HTTP
import Network.URI
import Testlib.Env
import Testlib.Printing
import Testlib.Service
import UnliftIO (MonadUnliftIO)
import Prelude

data Response = Response
  { jsonBody :: Maybe Aeson.Value,
    body :: ByteString,
    status :: Int,
    headers :: [HTTP.Header],
    request :: HTTP.Request
  }
  deriving (Show)

instance HasField "json" Response (App Aeson.Value) where
  getField response = maybe (assertFailure "Response has no json body") pure response.jsonBody

showRequest :: HTTP.Request -> String
showRequest r =
  T.unpack (T.decodeUtf8 (HTTP.method r))
    <> " "
    <> uriToString id (HTTP.getUri r) ""

showHeaders :: [HTTP.Header] -> String
showHeaders r =
  intercalate "\n" $
    r <&> \(name, value) ->
      C8.unpack (CI.original name) <> ": " <> C8.unpack value

getRequestBody :: HTTP.Request -> Maybe BS.ByteString
getRequestBody req = case HTTP.requestBody req of
  HTTP.RequestBodyLBS lbs -> pure (L.toStrict lbs)
  HTTP.RequestBodyBS bs -> pure bs
  _ -> Nothing

prettyResponse :: Response -> String
prettyResponse r =
  unlines $
    concat
      [ pure $ colored yellow "request: \n" <> showRequest r.request,
        pure $ colored yellow "request headers: \n" <> showHeaders (HTTP.requestHeaders r.request),
        case getRequestBody r.request of
          Nothing -> []
          Just b ->
            [ colored yellow "request body:",
              T.unpack . T.decodeUtf8 $ case Aeson.decode (L.fromStrict b) of
                Just v -> L.toStrict (Aeson.encodePretty (v :: Aeson.Value))
                Nothing -> hex b
            ],
        pure $ colored blue "response status: " <> show r.status,
        pure $ colored blue "response body:",
        pure $
          ( T.unpack . T.decodeUtf8 $
              case r.jsonBody of
                Just b -> L.toStrict (Aeson.encodePretty b)
                Nothing -> r.body
          )
      ]

data AssertionFailure = AssertionFailure
  { callstack :: CallStack,
    response :: Maybe Response,
    msg :: String
  }

instance Show AssertionFailure where
  show (AssertionFailure _ _ msg) = "AssertionFailure _ _ " <> show msg

instance Exception AssertionFailure where
  displayException (AssertionFailure _ _ msg) = msg

newtype App a = App {unApp :: ReaderT Env IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadMask,
      MonadCatch,
      MonadThrow,
      MonadReader Env,
      MonadBase IO,
      MonadUnliftIO,
      MonadBaseControl IO
    )

runAppWithEnv :: Env -> App a -> IO a
runAppWithEnv e m = runReaderT (unApp m) e

-- | Convert an action in the 'App' monad to an 'IO' action.
appToIO :: App a -> App (IO a)
appToIO action = do
  f <- appToIOKleisli (const action)
  pure $ f ()

appToIOKleisli :: (a -> App b) -> App (a -> IO b)
appToIOKleisli k = do
  env <- ask
  pure $ \a -> runAppWithEnv env (k a)

getServiceMap :: String -> App ServiceMap
getServiceMap fedDomain = do
  env <- ask
  assertJust ("Could not find service map for federation domain: " <> fedDomain) (Map.lookup fedDomain (env.serviceMap))

getMLSState :: App MLSState
getMLSState = do
  ref <- asks (.mls)
  liftIO $ readIORef ref

setMLSState :: MLSState -> App ()
setMLSState s = do
  ref <- asks (.mls)
  liftIO $ writeIORef ref s

modifyMLSState :: (MLSState -> MLSState) -> App ()
modifyMLSState f = do
  ref <- asks (.mls)
  liftIO $ modifyIORef ref f

getBaseDir :: App FilePath
getBaseDir = fmap (.baseDir) getMLSState

data AppFailure = AppFailure String

instance Show AppFailure where
  show (AppFailure msg) = msg

instance Exception AppFailure where
  displayException (AppFailure msg) = msg

instance MonadFail App where
  fail msg = assertFailure ("Pattern matching failure: " <> msg)

assertFailure :: HasCallStack => String -> App a
assertFailure msg =
  forceList msg $
    liftIO $
      E.throw (AssertionFailure callStack Nothing msg)
  where
    forceList [] y = y
    forceList (x : xs) y = seq x (forceList xs y)

assertJust :: HasCallStack => String -> Maybe a -> App a
assertJust _ (Just x) = pure x
assertJust msg Nothing = assertFailure msg

addFailureContext :: String -> App a -> App a
addFailureContext msg = modifyFailureMsg (\m -> m <> "\nThis failure happened in this context:\n" <> msg)

modifyFailureMsg :: (String -> String) -> App a -> App a
modifyFailureMsg modMessage = modifyFailure (\e -> e {msg = modMessage e.msg})

modifyFailure :: (AssertionFailure -> AssertionFailure) -> App a -> App a
modifyFailure modifyAssertion action = do
  env <- ask
  liftIO
    ( E.catch
        (runAppWithEnv env action)
        ( \(e :: AssertionFailure) ->
            E.throw (modifyAssertion e)
        )
    )

data ServiceOverrides = ServiceOverrides
  { dbBrig :: Value -> App Value,
    dbCannon :: Value -> App Value,
    dbCargohold :: Value -> App Value,
    dbGalley :: Value -> App Value,
    dbGundeck :: Value -> App Value,
    dbNginz :: Value -> App Value,
    dbSpar :: Value -> App Value,
    dbBackgroundWorker :: Value -> App Value,
    dbStern :: Value -> App Value
  }

instance Default ServiceOverrides where
  def = defaultServiceOverrides

instance Semigroup ServiceOverrides where
  a <> b =
    ServiceOverrides
      { dbBrig = dbBrig a >=> dbBrig b,
        dbCannon = dbCannon a >=> dbCannon b,
        dbCargohold = dbCargohold a >=> dbCargohold b,
        dbGalley = dbGalley a >=> dbGalley b,
        dbGundeck = dbGundeck a >=> dbGundeck b,
        dbNginz = dbNginz a >=> dbNginz b,
        dbSpar = dbSpar a >=> dbSpar b,
        dbBackgroundWorker = dbBackgroundWorker a >=> dbBackgroundWorker b,
        dbStern = dbStern a >=> dbStern b
      }

instance Monoid ServiceOverrides where
  mempty = defaultServiceOverrides

defaultServiceOverrides :: ServiceOverrides
defaultServiceOverrides =
  ServiceOverrides
    { dbBrig = pure,
      dbCannon = pure,
      dbCargohold = pure,
      dbGalley = pure,
      dbGundeck = pure,
      dbNginz = pure,
      dbSpar = pure,
      dbBackgroundWorker = pure,
      dbStern = pure
    }

defaultServiceOverridesToMap :: Map.Map Service (Value -> App Value)
defaultServiceOverridesToMap = ([minBound .. maxBound] <&> (,pure)) & Map.fromList

-- | Overrides the service configurations with the given overrides.
-- e.g.
-- `let overrides =
--    def
--      { dbBrig =
--          setField "optSettings.setFederationStrategy" "allowDynamic"
--            >=> removeField "optSettings.setFederationDomainConfigs"
--      }
--  withOverrides overrides defaultServiceOverridesToMap`
withOverrides :: ServiceOverrides -> Map.Map Service (Value -> App Value) -> Map.Map Service (Value -> App Value)
withOverrides overrides =
  Map.mapWithKey
    ( \svr f ->
        case svr of
          Brig -> f >=> overrides.dbBrig
          Cannon -> f >=> overrides.dbCannon
          Cargohold -> f >=> overrides.dbCargohold
          Galley -> f >=> overrides.dbGalley
          Gundeck -> f >=> overrides.dbGundeck
          Nginz -> f >=> overrides.dbNginz
          Spar -> f >=> overrides.dbSpar
          BackgroundWorker -> f >=> overrides.dbBackgroundWorker
          Stern -> f >=> overrides.dbStern
          FederatorInternal -> f
    )
