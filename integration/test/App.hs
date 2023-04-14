{-# OPTIONS_GHC -Wno-unused-matches #-}

module App where

import Config
import Control.Concurrent.Async (mapConcurrently_)
import Control.Exception (finally)
import qualified Control.Exception as E
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader
import Control.Retry (fibonacciBackoff, limitRetriesByCumulativeDelay, retrying)
import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Key as KM
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.CaseInsensitive as CI
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (Proxy))
import qualified Data.Scientific as Sci
import Data.Tagged
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Yaml as Yaml
import GHC.Exception
import GHC.Records
import GHC.Stack
import Imports hiding (ask, asks, local)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Network.Socket as N
import Network.URI (uriToString)
import System.FilePath ((</>))
import System.IO (openBinaryTempFile)
import qualified System.IO.Error as Error
import System.Process (CreateProcess (..), createProcess, proc, terminateProcess, waitForProcess)
import Test.Tasty.Options
import Test.Tasty.Providers
import qualified Test.Tasty.Providers as Tasty
import Test.Tasty.Providers.ConsoleFormat

yellow :: String
yellow = "\x1b[38;5;11m"

blue :: String
blue = "\x1b[38;5;6m"

purpleish :: String
purpleish = "\x1b[38;5;13m"

orange :: String
orange = "\x1b[38;5;3m"

red :: String
red = "\x1b[38;5;1m"

resetColor :: String
resetColor = "\x1b[0m"

colored :: String -> String -> String
colored color s = color <> s <> resetColor

newtype App a = App {unApp :: ReaderT Env IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance IsTest (App ()) where
  run opts action _ = do
    env <- mkEnv (lookupOption opts) (lookupOption opts) (lookupOption opts)
    result :: Tasty.Result <-
      (runAppWithEnv env action >> pure (Tasty.testPassed ""))
        `E.catches` [ E.Handler
                        ( \(e :: AssertionFailure) -> do
                            pure (testFailedDetails (displayException e) (printFailureDetails e))
                        ),
                      E.Handler
                        (\(ex :: SomeException) -> pure (testFailed (show ex)))
                    ]
    pure result

  testOptions = Tagged [Option (Proxy @ConfigFile), Option (Proxy @ServiceConfigsDir), Option (Proxy @ServicesCwdBase)]

data AppFailure = AppFailure String

instance Show AppFailure where
  show (AppFailure msg) = msg

instance Exception AppFailure where
  displayException (AppFailure msg) = msg

failApp :: String -> App a
failApp msg = throw (AppFailure msg)

withModifiedServices :: Map Service (Value -> App Value) -> App a -> App a
withModifiedServices services k = do
  ports <- Map.traverseWithKey (\_ _ -> liftIO openFreePort) services

  let updateServiceMapInConfig :: Value -> App Value
      updateServiceMapInConfig config =
        foldlM
          ( \c (srv, (port, _)) ->
              c
                & serviceName srv
                %.= object
                  [ "host" .= ("127.0.0.1" :: String),
                    "port" .= port
                  ]
          )
          config
          (Map.assocs ports)

  instances <- for (Map.assocs services) $ \(srv, modifyConfig) -> do
    basedir <- App $ asks (.serviceConfigsDir)
    let srvName = serviceName srv
        cfgFile = basedir </> srvName </> "conf" </> (srvName <> ".yaml")
    config <- do
      eith <- liftIO (Yaml.decodeFileEither cfgFile)
      case eith of
        Left err -> failApp ("Error while parsing " <> cfgFile <> ": " <> Yaml.prettyPrintParseException err)
        Right value -> pure value
    config' <- updateServiceMapInConfig config >>= modifyConfig
    (tempFile, fh) <- liftIO $ openBinaryTempFile "/tmp" (srvName <> ".yaml")
    liftIO $ BS.hPut fh (Yaml.encode config')
    hClose fh

    cwd <-
      App $
        asks (.servicesCwdBase) <&> \case
          NoServicesCwdBase -> Nothing
          ServicesCwdBase dir -> Just (dir </> srvName)

    let exe =
          case cwd of
            Nothing -> srvName
            Just d -> "../../dist" </> srvName

    (port, socket) <- maybe (failApp "the impossible in withServices happened") pure (Map.lookup srv ports)
    liftIO $ N.close socket
    (_, _, _, ph) <- liftIO $ createProcess (proc exe ["-c", tempFile]) {cwd = cwd}
    pure ph

  let stopInstances = liftIO $ do
        for_ instances terminateProcess
        for_ instances waitForProcess

  let updateServiceMap serviceMap =
        Map.foldrWithKey
          ( \srv (newPort, _) sm ->
              case srv of
                Brig -> sm {brig = sm.brig {port = fromIntegral newPort}}
                Galley -> sm {galley = sm.galley {port = fromIntegral newPort}}
                Cannon -> sm {cannon = sm.cannon {port = fromIntegral newPort}}
          )
          serviceMap
          ports

  let modifyEnv env =
        env
          { context =
              env.context
                { serviceMap =
                    updateServiceMap env.context.serviceMap
                }
          }

  let waitForAllServices = do
        env <- App ask
        liftIO $
          mapConcurrently_
            (\srv -> runReaderT (unApp (waitUntilServiceUp srv)) env)
            (Map.keys ports)

  App $
    ReaderT
      ( \env ->
          runReaderT
            ( local
                modifyEnv
                ( unApp $ do
                    waitForAllServices
                    k
                )
            )
            env
            `finally` stopInstances
      )

waitUntilServiceUp :: HasCallStack => Service -> App ()
waitUntilServiceUp srv = do
  isUp <-
    retrying
      (limitRetriesByCumulativeDelay (4 * 1000 * 1000) (fibonacciBackoff (200 * 1000)))
      (\_ isUp -> pure (not isUp))
      ( \_ -> do
          req <- baseRequest srv Unversioned "/i/status"
          env <- App ask
          eith <-
            liftIO $
              E.try
                ( runAppWithEnv env $ do
                    res <- submit "GET" req
                    pure (res.status `elem` [200, 204])
                )
          pure $ either (\(e :: HTTP.HttpException) -> False) id eith
      )
  unless isUp $
    failApp ("Time out for service " <> show srv <> " to come up")

-- | Open a TCP socket on a random free port. This is like 'warp''s
--   openFreePort.
--
--   Since 0.0.0.1
openFreePort :: IO (Int, N.Socket)
openFreePort =
  E.bracketOnError (N.socket N.AF_INET N.Stream N.defaultProtocol) N.close $
    \sock -> do
      N.bind sock $ N.SockAddrInet 0 $ N.tupleToHostAddress (127, 0, 0, 1)
      N.getSocketName sock >>= \case
        N.SockAddrInet port _ -> pure (fromIntegral port, sock)
        addr ->
          E.throwIO $
            Error.mkIOError
              Error.userErrorType
              ( "openFreePort was unable to create socket with a SockAddrInet. "
                  <> "Got "
                  <> show addr
              )
              Nothing
              Nothing

withModifiedService :: Service -> (Value -> App Value) -> App a -> App a
withModifiedService srv modConfig k = do
  withModifiedServices (Map.singleton srv modConfig) k

printFailureDetails :: AssertionFailure -> ResultDetailsPrinter
printFailureDetails (AssertionFailure stack mbResponse _) = ResultDetailsPrinter $ \testLevel _withFormat -> do
  let nindent = 2 * testLevel + 2
  putStrLn (indent nindent (prettyStack stack))
  for_ mbResponse $ \r -> putStrLn (indent nindent (prettyReponse r))

indent :: Int -> String -> String
indent n s =
  unlines (map (pad <>) (lines s))
  where
    pad = replicate n ' '

runAppWithEnv :: Env -> App a -> IO a
runAppWithEnv e m = runReaderT (unApp m) e

getContext :: App Context
getContext = App $ asks (.context)

getManager :: App HTTP.Manager
getManager = App $ asks (.manager)

onFailureAddResponse :: Response -> App a -> App a
onFailureAddResponse r m = App $ do
  e <- ask
  liftIO $ E.catch (runAppWithEnv e m) $ \(AssertionFailure stack _ msg) -> do
    E.throw (AssertionFailure stack (Just r) msg)

getPrekey :: App Value
getPrekey = App $ do
  pks <- asks (.prekeys)
  (i, pk) <- atomicModifyIORef pks getPK
  pure $ object ["id" .= i, "key" .= pk]
  where
    getPK [] = error "Out of prekeys"
    getPK (k : ks) = (ks, k)

getLastPrekey :: App Value
getLastPrekey = App $ do
  pks <- asks (.lastPrekeys)
  lpk <- atomicModifyIORef pks getPK
  pure $ object ["id" .= lastPrekeyId, "key" .= lpk]
  where
    getPK [] = error "Out of prekeys"
    getPK (k : ks) = (ks, k)

    lastPrekeyId :: Int
    lastPrekeyId = 65535

data AssertionFailure = AssertionFailure
  { callstack :: CallStack,
    response :: Maybe Response,
    msg :: String
  }

instance Show AssertionFailure where
  show (AssertionFailure _ _ msg) = "AssertionFailure _ _ " <> show msg

instance Exception AssertionFailure where
  displayException (AssertionFailure _ _ msg) = msg

(@?=) ::
  (Eq a, Show a, HasCallStack) =>
  -- | The actual value
  a ->
  -- | The expected value
  a ->
  App ()
a @?= b = unless (a == b) $ do
  assertFailure $ "Expected: " <> show b <> "\n" <> "Actual: " <> show a

-- TODO: add some nice JSON diffing
(@%?=) ::
  (ProducesJSON a, ProducesJSON b, HasCallStack) =>
  -- | The actual value
  a ->
  -- | The expected value
  b ->
  App ()
a @%?= b = do
  xa <- prodJSON a
  xb <- prodJSON b
  unless (xa == xb) $ do
    pa <- prettyJSON xa
    pb <- prettyJSON xb
    assertFailure $ "Expected:\n" <> pb <> "\n" <> "Actual:\n" <> pa

assertFailure :: HasCallStack => String -> App a
assertFailure msg =
  deepseq msg $
    liftIO $
      E.throw (AssertionFailure callStack Nothing msg)

assertBool :: HasCallStack => String -> Bool -> App ()
assertBool _ True = pure ()
assertBool msg False = assertFailure msg

expectFailure :: HasCallStack => (AssertionFailure -> App ()) -> App a -> App ()
expectFailure checkFailure action = do
  env <- App $ ask
  res :: Either AssertionFailure x <-
    liftIO
      (E.try (runAppWithEnv env action))
  case res of
    Left e@(AssertionFailure cs mr msg) ->
      checkFailure e
    Right x -> assertFailure "Expected AssertionFailure, but none occured"

prettyStack :: CallStack -> String
prettyStack cs =
  intercalate "\n" $
    [colored yellow "call stack: "]
      <> (drop 1 . prettyCallStackLines) cs

class ProducesJSON a where
  prodJSON :: HasCallStack => a -> App Value

instance {-# OVERLAPPABLE #-} ToJSON a => ProducesJSON a where
  prodJSON = pure . toJSON

instance {-# OVERLAPPING #-} ToJSON a => ProducesJSON (App a) where
  prodJSON m = m <&> toJSON

(%.) :: (HasCallStack, ProducesJSON a) => a -> String -> App Value
(%.) x k = do
  ob <- asObject x
  case KM.lookup (KM.fromString k) ob of
    Nothing -> assertFailureWithJSON ob $ "Field \"" <> k <> "\" is missing from object:"
    Just v -> pure v

(%.?) :: (HasCallStack, ProducesJSON a) => a -> String -> App (Maybe Value)
(%.?) x k = do
  ob <- asObject x
  pure $ KM.lookup (KM.fromString k) ob

-- Update nested fields
-- E.g. ob & "foo.bar.baz" %.= ("quux" :: String)
(%.=) :: forall a b. (HasCallStack, ProducesJSON a, ToJSON b) => String -> b -> a -> App Value
(%.=) selector v x = do
  (%.~) @a @Value selector (\_ -> pure (toJSON v)) x

-- Update nested fields, using the old value with a stateful action
(%.~) :: (HasCallStack, ProducesJSON a, ToJSON b) => String -> (Maybe Value -> App b) -> a -> App Value
(%.~) selector up x = do
  v <- prodJSON x
  let keys = splitOn "." selector
  case keys of
    (k : ks) -> go k ks v
    [] -> assertFailure "No key provided"
  where
    go k [] v = do
      ob <- asObject v
      let k' = KM.fromString k
      newValue <- toJSON <$> up (KM.lookup k' ob)
      pure $ Object $ KM.insert k' newValue ob
    go k (k2 : ks) v = do
      val <- v %. k
      newValue <- go k2 ks val
      ob <- asObject v
      pure $ Object $ KM.insert (KM.fromString k) newValue ob

pprintJSON :: ProducesJSON a => a -> App ()
pprintJSON = prettyJSON >=> putStrLn

prettyJSON :: ProducesJSON a => a -> App String
prettyJSON x =
  prodJSON x <&> Aeson.encodePretty <&> LC8.unpack

assertFailureWithJSON :: HasCallStack => ProducesJSON a => a -> String -> App b
assertFailureWithJSON v msg = do
  msg' <- ((msg <> "\n") <>) <$> prettyJSON v
  assertFailure msg'

constrName :: Value -> String
constrName (Object _) = "Object"
constrName (Array _) = "Array"
constrName (String _) = "String"
constrName (Number _) = "Number"
constrName (Bool _) = "Bool"
constrName Null = "Null"

typeWasExpectedButGot :: String -> Value -> String
typeWasExpectedButGot expectedType x = "Expected " <> expectedType <> " but got " <> constrName x <> ":"

asStringM :: HasCallStack => ProducesJSON a => a -> App (Maybe String)
asStringM x =
  prodJSON x >>= \case
    (String s) -> pure (Just (T.unpack s))
    _ -> pure Nothing

asString :: HasCallStack => ProducesJSON a => a -> App String
asString x =
  prodJSON x >>= \case
    (String s) -> pure (T.unpack s)
    v -> assertFailureWithJSON x ("String" `typeWasExpectedButGot` v)

asObject :: HasCallStack => ProducesJSON a => a -> App Object
asObject x =
  prodJSON x >>= \case
    (Object o) -> pure o
    v -> assertFailureWithJSON x ("Object" `typeWasExpectedButGot` v)

asInt :: HasCallStack => ProducesJSON a => a -> App Int
asInt x =
  prodJSON x >>= \case
    (Number n) ->
      case Sci.floatingOrInteger n of
        Left (_ :: Double) -> assertFailure "Expected an integral, but got a floating point"
        Right i -> pure i
    v -> assertFailureWithJSON x ("Number" `typeWasExpectedButGot` v)

asList :: HasCallStack => ProducesJSON a => a -> App [Value]
asList x =
  prodJSON x >>= \case
    (Array arr) -> pure (toList arr)
    v -> assertFailureWithJSON x ("Array" `typeWasExpectedButGot` v)

asBool :: HasCallStack => ProducesJSON a => a -> App Bool
asBool x =
  prodJSON x >>= \case
    (Bool b) -> pure b
    v -> assertFailureWithJSON x ("Bool" `typeWasExpectedButGot` v)

objId :: ProducesJSON a => a -> App String
objId x = do
  v <- prodJSON x
  case v of
    Object ob -> ob %. "id" & asString
    String t -> pure (T.unpack t)
    other -> assertFailureWithJSON other (typeWasExpectedButGot "Object or String" other)

-- There's probably nicer way to write this
objQid :: ProducesJSON a => a -> App Value
objQid ob = do
  m <- firstSuccess [select ob, inField]
  case m of
    Nothing -> do
      assertFailureWithJSON ob "Could not get a qualified id from value:"
    Just v -> pure v
  where
    select x = runMaybeT $ do
      vdom <- MaybeT $ x %.? "domain"
      dom <- MaybeT $ asStringM vdom
      vid <- MaybeT $ x %.? "id"
      id_ <- MaybeT $ asStringM vid
      pure $ object ["domain" .= dom, "id" .= id_]

    inField = do
      m <- ob %.? "qualified_id"
      case m of
        Nothing -> pure Nothing
        Just x -> select x

objPath :: ProducesJSON a => a -> App String
objPath x = do
  ob <- prodJSON x
  id_ <- ob %. "id" & asString
  domain_ <- ob %. "domain" & asString
  pure $ domain_ <> "/" <> id_

firstSuccess :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstSuccess [] = pure Nothing
firstSuccess (x : xs) =
  x >>= \case
    Nothing -> firstSuccess xs
    Just y -> pure (Just y)

data Response = Response
  { jsonBody :: Maybe Aeson.Value,
    body :: ByteString,
    status :: Int,
    headers :: [HTTP.Header],
    request :: HTTP.Request
  }

instance HasField "json" Response (App Aeson.Value) where
  getField response = maybe (assertFailure "Response has no json body") pure response.jsonBody

data Versioned = Versioned | Unversioned | ExplicitVersion Int

baseRequest :: Service -> Versioned -> String -> App HTTP.Request
baseRequest service versioned path = do
  ctx <- getContext
  pathPrefix <- case versioned of
    Versioned -> do
      v <- App $ asks (.context.version)
      pure ("v" <> show v <> "/")
    Unversioned -> pure ""
    ExplicitVersion v -> do
      pure ("v" <> show v <> "/")

  liftIO . HTTP.parseRequest $
    let HostPort h p = serviceHostPort ctx.serviceMap service
     in "http://" <> h <> ":" <> show p <> pathPrefix <> path

addJSONObject :: [Aeson.Pair] -> HTTP.Request -> HTTP.Request
addJSONObject = addJSON . Aeson.object

addJSON :: Aeson.Value -> HTTP.Request -> HTTP.Request
addJSON obj req =
  req
    { HTTP.requestBody = HTTP.RequestBodyLBS (Aeson.encode obj),
      HTTP.requestHeaders =
        ("Content-Type", "application/json")
          : HTTP.requestHeaders req
    }

addHeader :: String -> String -> HTTP.Request -> HTTP.Request
addHeader name value req =
  req {HTTP.requestHeaders = (CI.mk . C8.pack $ name, C8.pack value) : HTTP.requestHeaders req}

zUser :: String -> HTTP.Request -> HTTP.Request
zUser = addHeader "Z-User"

zConnection :: String -> HTTP.Request -> HTTP.Request
zConnection = addHeader "Z-Connection"

submit :: ByteString -> HTTP.Request -> App Response
submit method req0 = do
  let req = req0 {HTTP.method = method}
  manager <- getManager
  res <- liftIO $ HTTP.httpLbs req manager
  pure $
    Response
      { jsonBody = Aeson.decode (HTTP.responseBody res),
        body = L.toStrict (HTTP.responseBody res),
        status = HTTP.statusCode (HTTP.responseStatus res),
        headers = HTTP.responseHeaders res,
        request = req
      }

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

getRequestBody :: HTTP.Request -> Maybe ByteString
getRequestBody req = case HTTP.requestBody req of
  HTTP.RequestBodyLBS lbs -> pure (L.toStrict lbs)
  HTTP.RequestBodyBS bs -> pure bs
  _ -> Nothing

hline :: String
hline = replicate 40 '-'

prettyReponse :: Response -> String
prettyReponse r =
  unlines $
    concat
      [ pure hline,
        pure $ colored yellow "request: \n" <> showRequest r.request,
        pure $ colored yellow "request headers: \n" <> showHeaders (HTTP.requestHeaders r.request),
        case getRequestBody r.request of
          Nothing -> []
          Just b ->
            [ colored yellow "request body:",
              T.unpack . T.decodeUtf8 $ case Aeson.decode (L.fromStrict b) of
                Just v -> L.toStrict (Aeson.encodePretty (v :: Aeson.Value))
                Nothing -> b
            ],
        pure $ colored blue "response status: " <> show r.status,
        pure $ colored blue "response body:",
        pure $
          ( T.unpack . T.decodeUtf8 $
              case r.jsonBody of
                Just b -> L.toStrict (Aeson.encodePretty b)
                Nothing -> r.body
          ),
        pure hline
      ]

printResponse :: Response -> IO ()
printResponse = putStrLn . prettyReponse

withResponse :: HasCallStack => Response -> (Response -> App a) -> App a
withResponse r k = onFailureAddResponse r (k r)

bindResponse :: HasCallStack => App Response -> (Response -> App a) -> App a
bindResponse m k = m >>= \r -> withResponse r k
