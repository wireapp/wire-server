{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
-- SPDX-License-Identifier: AGPL-3.0-only

-- | Native Kubernetes effects. Decode external data once, at this boundary.
module Testlib.Ingress.Kubernetes
  ( Client,
    loadClient,
    withNamespace,
    apply,
    checkGatewayClass,
    waitProgrammed,
    waitDeployment,
    forwardTarget,
    ForwardTarget,
    withForward,
    diagnostics,
    KubernetesError (..),
    ResourceStatus,
    Scope (..),
    validateScope,
    hasCondition,
    Channels,
    initialChannels,
    ForwardError (..),
    receiveFrame,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.IORef
import Data.List (find)
import Data.Maybe (isJust)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word16)
import Data.X509 (SignedCertificate)
import Data.X509.CertificateStore (makeCertificateStore)
import Data.X509.Memory (readSignedObjectFromMemory)
import qualified Network.Connection as Connection
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.HTTP.Types (renderQuery, statusCode, urlEncode)
import qualified Network.Socket as Socket
import qualified Network.TLS as TLS
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as WS
import System.Timeout (timeout)
import Testlib.Ingress.Kubeconfig
import Prelude

data Client = Client
  { manager :: HTTP.Manager,
    baseRequest :: HTTP.Request,
    tlsParameters :: TLS.ClientParams
  }

data KubernetesError
  = InvalidResponse String
  | TransportFailed String
  | RequestFailed String Int
  | ResourceNotFound String
  | TimedOut String
  | ForwardingFailed ForwardError
  deriving stock (Show)

instance Exception KubernetesError

data ForwardTarget = ForwardTarget
  { namespace :: String,
    pod :: String,
    port :: Word16
  }

segment :: String -> String
segment = BS8.unpack . urlEncode True . Text.encodeUtf8 . Text.pack

loadClient :: IO Client
loadClient = do
  (path, config) <- loadConfig
  base <- HTTP.parseRequest config.server
  unless (HTTP.secure base && HTTP.path base == "/" && BS.null (HTTP.queryString base)) $
    ioError (userError "Kubernetes server must be an HTTPS origin")
  ca <- readMaterial path config.authority
  let certificates = readSignedObjectFromMemory ca :: [SignedCertificate]
  when (null certificates) $ ioError (userError "Kubeconfig contains no CA certificates")
  credential <- forM config.clientCertificate $ \(cert, key) -> do
    result <- TLS.credentialLoadX509FromMemory <$> readMaterial path cert <*> readMaterial path key
    either (const $ ioError $ userError "Invalid Kubernetes client certificate") pure result
  token <- traverse (readMaterial path) config.bearerToken
  let defaults = TLS.defaultParamsClient (BS8.unpack $ HTTP.host base) BS.empty
      parameters =
        defaults
          { TLS.clientShared = (TLS.clientShared defaults) {TLS.sharedCAStore = makeCertificateStore certificates},
            TLS.clientHooks = (TLS.clientHooks defaults) {TLS.onCertificateRequest = const (pure credential)}
          }
      headers = maybe [] (\value -> [("Authorization", "Bearer " <> BS8.takeWhile (`notElem` ['\r', '\n']) value)]) token
  manager <- HTTP.newManager $ mkManagerSettings (Connection.TLSSettings parameters) Nothing
  pure
    Client
      { manager,
        tlsParameters = parameters,
        baseRequest = base {HTTP.requestHeaders = headers, HTTP.redirectCount = 0, HTTP.responseTimeout = HTTP.responseTimeoutMicro 20000000}
      }

request :: (FromJSON a) => Client -> BS.ByteString -> String -> [(BS.ByteString, Maybe BS.ByteString)] -> Maybe Value -> IO a
request client method path query body = do
  let req =
        client.baseRequest
          { HTTP.method = method,
            HTTP.path = BS8.pack path,
            HTTP.queryString = renderQuery True query,
            HTTP.requestHeaders = ("Content-Type", "application/json") : HTTP.requestHeaders client.baseRequest,
            HTTP.requestBody = maybe (HTTP.RequestBodyBS BS.empty) (HTTP.RequestBodyLBS . encode) body,
            HTTP.checkResponse = \_ _ -> pure ()
          }
  response <- HTTP.httpLbs req client.manager `catch` \(_ :: HTTP.HttpException) -> throwIO (TransportFailed path)
  let status = statusCode (HTTP.responseStatus response)
  unless (status >= 200 && status < 300) $ throwIO (RequestFailed (BS8.unpack method <> " " <> path) status)
  either (const $ throwIO $ InvalidResponse path) pure $ eitherDecode (HTTP.responseBody response)

get :: (FromJSON a) => Client -> String -> IO a
get client path = request client "GET" path [] Nothing

newtype Items a = Items [a]

instance (FromJSON a) => FromJSON (Items a) where
  parseJSON = withObject "resource list" $ \o -> Items <$> o .: "items"

newtype Metadata = Metadata {uid :: String}

instance FromJSON Metadata where
  parseJSON = withObject "resource" $ \o -> do
    meta <- o .: "metadata"
    Metadata <$> meta .: "uid"

data ResourceType = ResourceType {kind :: String, plural :: String, namespaced :: Bool}

instance FromJSON ResourceType where
  parseJSON = withObject "API resource" $ \o -> ResourceType <$> o .: "kind" <*> o .: "name" <*> o .: "namespaced"

newtype Discovery = Discovery [ResourceType]

instance FromJSON Discovery where
  parseJSON = withObject "API discovery" $ \o -> Discovery <$> o .: "resources"

data ManifestHeader = ManifestHeader {apiVersion :: String, kind :: String, namespace :: Maybe String}

instance FromJSON ManifestHeader where
  parseJSON = withObject "manifest" $ \o -> do
    metadata <- o .: "metadata"
    ManifestHeader <$> o .: "apiVersion" <*> o .: "kind" <*> metadata .:? "namespace"

apiPath :: String -> String
apiPath "v1" = "/api/v1"
apiPath version = "/apis/" <> version

namespacePath :: String -> String
namespacePath name = "/namespaces/" <> segment name

-- The requested scope owns the resource; manifest metadata cannot override it.
data Scope = Cluster | Namespace String
  deriving stock (Eq, Show)

validateScope :: Scope -> Bool -> Maybe String -> Either String ()
validateScope Cluster False Nothing = Right ()
validateScope (Namespace expected) True declared
  | maybe True (== expected) declared = Right ()
validateScope _ _ _ = Left "Manifest scope does not match the fixture namespace"

create :: Client -> Scope -> Value -> IO Metadata
create client scope value = do
  header <- either (throwIO . InvalidResponse) pure $ parseEither (parseJSON @ManifestHeader) value
  Discovery resources <- get client (apiPath header.apiVersion)
  descriptor <-
    maybe (throwIO $ ResourceNotFound header.kind) pure $
      find (\resource -> resource.kind == header.kind && '/' `notElem` resource.plural) resources
  either (throwIO . InvalidResponse) pure (validateScope scope descriptor.namespaced header.namespace)
  let path = apiPath header.apiVersion <> (case scope of Cluster -> ""; Namespace namespace -> namespacePath namespace) <> "/" <> descriptor.plural
  request client "POST" path [] (Just value)

apply :: Client -> String -> Value -> IO ()
apply client namespace = void . create client (Namespace namespace)

withNamespace :: Client -> String -> (String -> IO a) -> IO a
withNamespace client name action = bracket acquire release (const $ action name)
  where
    acquire = create client Cluster (object ["apiVersion" .= String "v1", "kind" .= String "Namespace", "metadata" .= object ["name" .= name]])
    release metadata =
      void $
        request @Value
          client
          "DELETE"
          ("/api/v1" <> namespacePath name)
          []
          (Just $ object ["apiVersion" .= String "v1", "kind" .= String "DeleteOptions", "preconditions" .= object ["uid" .= metadata.uid]])

checkGatewayClass :: Client -> String -> IO ()
checkGatewayClass client name = void $ get @Value client ("/apis/gateway.networking.k8s.io/v1/gatewayclasses/" <> segment name)

data Condition = Condition {conditionType :: String, status :: String, observedGeneration :: Maybe Integer}

instance FromJSON Condition where
  parseJSON = withObject "condition" $ \o -> Condition <$> o .: "type" <*> o .: "status" <*> o .:? "observedGeneration"

data ResourceStatus = ResourceStatus
  { name :: String,
    generation :: Maybe Integer,
    conditions :: [Condition],
    availableReplicas :: Int
  }

instance FromJSON ResourceStatus where
  parseJSON = withObject "resource status" $ \o -> do
    meta <- o .: "metadata"
    status <- o .:? "status" .!= mempty
    ResourceStatus
      <$> meta .: "name"
      <*> meta .:? "generation"
      <*> status .:? "conditions" .!= []
      <*> status .:? "availableReplicas" .!= 0

hasCondition :: String -> ResourceStatus -> Bool
hasCondition wanted resource = any matches resource.conditions
  where
    matches condition =
      condition.conditionType == wanted
        && condition.status == "True"
        && maybe True (\generation -> Just generation == resource.generation) condition.observedGeneration

waitFor :: String -> IO Bool -> IO ()
waitFor description check = do
  result <- timeout 180000000 loop
  unless (isJust result) $ throwIO (TimedOut description)
  where
    loop = check >>= \ready -> unless ready (threadDelay 1000000 >> loop)

waitProgrammed :: Client -> String -> String -> String -> IO ()
waitProgrammed client namespace plural name =
  waitFor (plural <> "/" <> name) $
    hasCondition "Programmed" <$> get client ("/apis/gateway.networking.k8s.io/v1" <> namespacePath namespace <> "/" <> segment plural <> "/" <> segment name)

waitDeployment :: Client -> String -> String -> IO ()
waitDeployment client namespace name = waitFor ("deployment/" <> name) $ do
  status <- get @ResourceStatus client ("/apis/apps/v1" <> namespacePath namespace <> "/deployments/" <> segment name)
  pure (status.availableReplicas > 0)

data ServicePort = ServicePort {port :: Word16, targetPort :: Word16}

instance FromJSON ServicePort where
  parseJSON = withObject "service port" $ \o -> ServicePort <$> o .: "port" <*> o .: "targetPort"

newtype Service = Service [ServicePort]

instance FromJSON Service where
  parseJSON = withObject "service" $ \o -> o .: "spec" >>= \spec -> Service <$> spec .: "ports"

forwardTarget :: Client -> String -> String -> IO ForwardTarget
forwardTarget client controllerNamespace ownerNamespace = do
  let selector = BS8.pack $ "gateway.envoyproxy.io/owning-gateway-namespace=" <> ownerNamespace <> ",gateway.envoyproxy.io/owning-gateway-name=regression"
      root = "/api/v1" <> namespacePath controllerNamespace
      selected :: (FromJSON a) => String -> IO [a]
      selected kind = do
        Items items <- request client "GET" (root <> "/" <> kind) [("labelSelector", Just selector)] Nothing
        pure items
  waitFor "ready Envoy proxy" $ any (hasCondition "Ready") <$> selected "pods"
  pods <- selected "pods"
  pod <- maybe (throwIO $ ResourceNotFound "ready Envoy proxy") (pure . (.name)) (find (hasCondition "Ready") pods)
  services <- selected "services"
  ports <- case services of
    [Service ports] -> pure ports
    _ -> throwIO (InvalidResponse "Expected one dedicated proxy Service")
  https <- maybe (throwIO $ ResourceNotFound "proxy HTTPS port") pure $ find ((== 443) . (.port)) ports
  pure ForwardTarget {namespace = controllerNamespace, pod, port = https.targetPort}

-- Each probe gets a fresh native WebSocket tunnel. TLS consumes its byte stream
-- directly; no loopback listening socket or background kubectl process is needed.
withForward :: Client -> ForwardTarget -> (TLS.Backend -> IO a) -> IO a
withForward client target action = do
  let host = BS8.unpack (HTTP.host client.baseRequest)
      service = show (HTTP.port client.baseRequest)
  addresses <- Socket.getAddrInfo (Just Socket.defaultHints {Socket.addrSocketType = Socket.Stream}) (Just host) (Just service)
  address <- case addresses of first : _ -> pure first; [] -> ioError (userError "Kubernetes server address did not resolve")
  bracket (Socket.socket (Socket.addrFamily address) Socket.Stream Socket.defaultProtocol) Socket.close $ \socket -> do
    Socket.connect socket (Socket.addrAddress address)
    context <- TLS.contextNew socket client.tlsParameters
    TLS.handshake context
    stream <- WS.makeStream (do bytes <- TLS.recvData context; pure $ if BS.null bytes then Nothing else Just bytes) (maybe (pure ()) (TLS.sendData context))
    let path = "/api/v1/namespaces/" <> segment target.namespace <> "/pods/" <> segment target.pod <> "/portforward?ports=" <> show target.port
        headers = ("Sec-WebSocket-Protocol", "v4.channel.k8s.io") : HTTP.requestHeaders client.baseRequest
    WS.runClientWithStream stream (host <> ":" <> service) path WS.defaultConnectionOptions headers $ \connection -> do
      channels <- newIORef initialChannels
      buffer <- newIORef BS.empty
      let receive = do
            frame <- WS.receiveData connection
            state <- readIORef channels
            case receiveFrame target.port state frame of
              Left err -> throwIO (ForwardingFailed err)
              Right (next, bytes) -> do
                writeIORef channels next
                if BS.null bytes then receive else pure bytes
          readExactly count = do
            bytes <- readIORef buffer
            if BS.length bytes >= count
              then do
                let (result, rest) = BS.splitAt count bytes
                writeIORef buffer rest
                pure result
              else do
                more <- receive
                writeIORef buffer (bytes <> more)
                readExactly count
      action TLS.Backend {TLS.backendFlush = pure (), TLS.backendClose = pure (), TLS.backendSend = WS.sendBinaryData connection . BS.cons 0, TLS.backendRecv = readExactly}

-- The two independent channels each announce the destination port once.
-- This is the protocol state, rather than a list of magic channel numbers.
data ChannelState = AwaitingPort | Streaming
  deriving stock (Eq, Show)

data Channels = Channels {dataChannel :: ChannelState, errorChannel :: ChannelState}
  deriving stock (Eq, Show)

data ForwardError = InvalidChannel | IncorrectPort | RemoteForwardError
  deriving stock (Eq, Show)

initialChannels :: Channels
initialChannels = Channels AwaitingPort AwaitingPort

receiveFrame :: Word16 -> Channels -> BS.ByteString -> Either ForwardError (Channels, BS.ByteString)
receiveFrame port state frame = case BS.uncons frame of
  Just (0, bytes) -> case state.dataChannel of
    AwaitingPort -> announce bytes state {dataChannel = Streaming}
    Streaming -> Right (state, bytes)
  Just (1, bytes) -> case state.errorChannel of
    AwaitingPort -> announce bytes state {errorChannel = Streaming}
    Streaming -> Left RemoteForwardError
  _ -> Left InvalidChannel
  where
    announce bytes next
      | bytes == BS.pack [fromIntegral port, fromIntegral (port `div` 256)] = Right (next, BS.empty)
      | otherwise = Left IncorrectPort

-- Diagnostic failures must not replace the original test exception.
diagnostics :: Client -> String -> IO ()
diagnostics client namespace = forM_ resources $ \(api, kind) -> do
  result <- try @KubernetesError $ get @(Items Value) client (apiPath api <> namespacePath namespace <> "/" <> kind)
  case result of
    Left failure -> print failure
    Right (Items items) -> forM_ items $ \item ->
      case parseEither summarize item of
        Left _ -> putStrLn ("Invalid diagnostics for " <> kind)
        Right summary -> LBS.putStrLn $ encode summary
  where
    resources =
      [("v1", "pods"), ("v1", "events")]
        <> [("gateway.networking.k8s.io/v1", kind) | kind <- ["gateways", "listenersets", "httproutes"]]
        <> [("gateway.envoyproxy.io/v1alpha1", kind) | kind <- ["clienttrafficpolicies", "envoypatchpolicies"]]
    summarize :: Value -> Parser Value
    summarize = withObject "diagnostics" $ \o -> do
      meta <- o .: "metadata"
      name <- meta .: "name"
      status <- o .:? "status"
      reason <- o .:? "reason"
      message <- o .:? "message"
      pure $ object ["name" .= (name :: String), "status" .= (status :: Maybe Value), "reason" .= (reason :: Maybe String), "message" .= (message :: Maybe String)]
