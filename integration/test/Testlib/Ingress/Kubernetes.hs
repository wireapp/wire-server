{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
-- SPDX-License-Identifier: AGPL-3.0-only

-- | Native Kubernetes REST and WebSocket transport for the chart fixtures.
module Testlib.Ingress.Kubernetes where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import Data.List (find)
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.X509 (SignedCertificate)
import Data.X509.CertificateStore (makeCertificateStore)
import Data.X509.Memory (readSignedObjectFromMemory)
import qualified Data.Yaml as Yaml
import qualified Network.Connection as Connection
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.HTTP.Types (statusCode, urlEncode)
import qualified Network.Socket as Socket
import qualified Network.TLS as TLS
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as WS
import System.Directory (getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath
import System.Timeout (timeout)
import Prelude

data Client = Client
  { manager :: HTTP.Manager,
    baseRequest :: HTTP.Request,
    tlsParameters :: TLS.ClientParams
  }

data ForwardTarget = ForwardTarget
  { namespace :: String,
    pod :: String,
    port :: Int
  }

-- Deliberately no Show instance for Client: the request contains credentials.
require :: Bool -> String -> IO ()
require predicate message = unless predicate $ ioError (userError message)

field :: (FromJSON a) => Key -> Value -> IO a
field key value = either (ioError . userError) pure $ parseEither (withObject "Kubernetes object" (.: key)) value

optionalField :: (FromJSON a) => Key -> Value -> Maybe a
optionalField key value = either (const Nothing) Just $ parseEither (withObject "Kubernetes object" (.: key)) value

segment :: String -> String
segment = BS8.unpack . urlEncode True . Text.encodeUtf8 . Text.pack

-- Kubeconfig client certificates and bearer tokens cover local and CI clusters.
-- Unsupported auth is rejected rather than invoking credential executables.
loadClient :: IO Client
loadClient = do
  home <- getHomeDirectory
  configPath <- fromMaybe (home </> ".kube/config") <$> lookupEnv "KUBECONFIG"
  require (length (splitSearchPath configPath) == 1) "Use a single kubeconfig file for the native ingress runner"
  config <- Yaml.decodeFileThrow configPath
  current <- field "current-context" config
  contexts <- field "contexts" config
  context <- named current contexts >>= field "context"
  clusterName <- field "cluster" context
  userName <- field "user" context
  cluster <- field "clusters" config >>= named clusterName >>= field "cluster"
  user <- field "users" config >>= named userName >>= field "user"
  require (optionalField @Bool "insecure-skip-tls-verify" cluster /= Just True) "Insecure Kubernetes TLS verification is not supported"
  require (all (\key -> isNothing (optionalField @Value key user)) ["exec", "auth-provider"]) "Use client certificates or a bearer token; exec/auth-provider kubeconfigs are not supported"
  server <- field "server" cluster
  base <- HTTP.parseRequest server
  require (HTTP.secure base && HTTP.path base == "/" && BS.null (HTTP.queryString base)) "Kubernetes server must be an HTTPS origin"
  require (isNothing (optionalField @String "tls-server-name" cluster) && isNothing (optionalField @String "proxy-url" cluster)) "tls-server-name and proxy-url kubeconfigs are not supported"
  ca <- material configPath cluster "certificate-authority-data" "certificate-authority"
  let certificates = readSignedObjectFromMemory ca :: [SignedCertificate]
  require (not (null certificates)) "Kubeconfig contains no CA certificates"
  tokenFile <- traverse (BS.readFile . resolve configPath) (optionalField "tokenFile" user)
  let token = (Text.encodeUtf8 <$> optionalField "token" user) `orElse` tokenFile
  credential <- case (optionalField @Value "client-certificate-data" user, optionalField @Value "client-certificate" user) of
    (Nothing, Nothing) -> pure Nothing
    _ -> do
      cert <- material configPath user "client-certificate-data" "client-certificate"
      key <- material configPath user "client-key-data" "client-key"
      either (ioError . userError . ("Invalid Kubernetes client certificate: " <>)) (pure . Just) $ TLS.credentialLoadX509FromMemory cert key
  require (isJust credential || isJust token) "Kubeconfig needs a client certificate or bearer token"
  let defaults = TLS.defaultParamsClient (BS8.unpack $ HTTP.host base) BS.empty
      parameters =
        defaults
          { TLS.clientShared = (TLS.clientShared defaults) {TLS.sharedCAStore = makeCertificateStore certificates},
            TLS.clientHooks = (TLS.clientHooks defaults) {TLS.onCertificateRequest = const (pure credential)}
          }
      headers = maybe [] (\value -> [("Authorization", "Bearer " <> BS8.takeWhile (/= '\n') value)]) token
  manager <- HTTP.newManager $ mkManagerSettings (Connection.TLSSettings parameters) Nothing
  pure Client {manager, baseRequest = base {HTTP.requestHeaders = headers, HTTP.redirectCount = 0, HTTP.responseTimeout = HTTP.responseTimeoutMicro 20000000}, tlsParameters = parameters}
  where
    named :: String -> [Value] -> IO Value
    named name entries = maybe (ioError $ userError ("Missing kubeconfig entry: " <> name)) pure $ find ((== Just name) . optionalField "name") entries
    resolve file path = if isAbsolute path then path else takeDirectory file </> path
    material file value embedded pathKey = case optionalField embedded value of
      Just encoded -> either (const $ ioError $ userError "Invalid base64 kubeconfig data") pure (Base64.decode $ Text.encodeUtf8 encoded)
      Nothing -> field pathKey value >>= BS.readFile . resolve file
    orElse (Just x) _ = Just x
    orElse Nothing y = y

request :: Client -> BS.ByteString -> String -> Maybe Value -> IO Value
request client method path body = do
  let (base, query) = break (== '?') path
      req =
        client.baseRequest
          { HTTP.method = method,
            HTTP.path = BS8.pack base,
            HTTP.queryString = BS8.pack query,
            HTTP.requestHeaders = ("Content-Type", "application/json") : HTTP.requestHeaders client.baseRequest,
            HTTP.requestBody = maybe (HTTP.RequestBodyBS BS.empty) (HTTP.RequestBodyLBS . encode) body,
            HTTP.checkResponse = \_ _ -> pure ()
          }
  response <- HTTP.httpLbs req client.manager `catch` \(_ :: HTTP.HttpException) -> ioError (userError ("Kubernetes transport failed: " <> path))
  let status = statusCode (HTTP.responseStatus response)
  require (status >= 200 && status < 300) ("Kubernetes " <> BS8.unpack method <> " " <> path <> " returned HTTP " <> show status)
  either (const $ ioError $ userError "Invalid Kubernetes JSON response") pure $ eitherDecode (HTTP.responseBody response)

get :: Client -> String -> IO Value
get client path = request client "GET" path Nothing

apiPath :: String -> String
apiPath "v1" = "/api/v1"
apiPath version = "/apis/" <> version

-- API discovery keeps CRDs (including ListenerSet and Envoy policies) native,
-- without assuming the plural form of resource kinds.
create :: Client -> Maybe String -> Value -> IO Value
create client namespace value = do
  version <- field "apiVersion" value
  kind <- field @String "kind" value
  resources <- get client (apiPath version) >>= field @[Value] "resources"
  descriptor <- maybe (ioError $ userError ("Unknown Kubernetes kind: " <> kind)) pure $ find ((== Just kind) . optionalField "kind") resources
  plural <- field "name" descriptor
  namespaced <- field "namespaced" descriptor
  nsPath <- if namespaced then maybe (ioError $ userError "Resource needs a namespace") (pure . ("/namespaces/" <>) . segment) namespace else pure ""
  request client "POST" (apiPath version <> nsPath <> "/" <> plural) (Just value)

withNamespace :: Client -> String -> (String -> IO a) -> IO a
withNamespace client name action = bracket acquire release (const $ action name)
  where
    acquire = create client Nothing (object ["apiVersion" .= String "v1", "kind" .= String "Namespace", "metadata" .= object ["name" .= name]]) >>= field "metadata" >>= field @String "uid"
    release uid = void $ request client "DELETE" ("/api/v1/namespaces/" <> segment name) (Just $ object ["apiVersion" .= String "v1", "kind" .= String "DeleteOptions", "preconditions" .= object ["uid" .= uid]])

waitFor :: String -> IO Bool -> IO ()
waitFor description check = do
  result <- timeout 180000000 loop
  require (isJust result) ("Timed out waiting for " <> description)
  where
    loop = check >>= \ready -> unless ready (threadDelay 1000000 >> loop)

condition :: String -> Value -> Bool
condition wanted value = fromMaybe False $ do
  status <- optionalField @Value "status" value
  conditions <- optionalField @[Value] "conditions" status
  pure $ any (\entry -> optionalField @String "type" entry == Just wanted && optionalField @String "status" entry == Just "True") conditions

waitProgrammed :: Client -> String -> String -> String -> IO ()
waitProgrammed client namespace plural name = waitFor (plural <> "/" <> name) $ condition "Programmed" <$> get client ("/apis/gateway.networking.k8s.io/v1/namespaces/" <> segment namespace <> "/" <> plural <> "/" <> segment name)

forwardTarget :: Client -> String -> String -> IO ForwardTarget
forwardTarget client controllerNamespace ownerNamespace = do
  let selector = "gateway.envoyproxy.io/owning-gateway-namespace=" <> ownerNamespace <> ",gateway.envoyproxy.io/owning-gateway-name=regression"
      root = "/api/v1/namespaces/" <> segment controllerNamespace
      selected kind = get client (root <> "/" <> kind <> "?labelSelector=" <> segment selector) >>= field @[Value] "items"
  waitFor "ready Envoy proxy" $ any (condition "Ready") <$> selected "pods"
  pods <- filter (condition "Ready") <$> selected "pods"
  pod <- case pods of
    first : _ -> field "metadata" first >>= field "name"
    [] -> ioError (userError "No ready Envoy proxy")
  services <- selected "services"
  service <- case services of [one] -> pure one; _ -> ioError (userError "Expected one dedicated proxy Service")
  ports <- field "spec" service >>= field @[Value] "ports"
  https <- maybe (ioError $ userError "Proxy has no HTTPS port") pure $ find ((== Just (443 :: Int)) . optionalField "port") ports
  port <- field "targetPort" https
  pure ForwardTarget {namespace = controllerNamespace, pod, port}

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
      initialized <- newIORef ([] :: [Int])
      buffer <- newIORef BS.empty
      let receive = do
            frame <- WS.receiveData connection
            channels <- readIORef initialized
            case decodeForwardFrame target.port channels frame of
              Left err -> ioError (userError err)
              Right (channel, initial, bytes) ->
                if initial
                  then modifyIORef' initialized (channel :) >> receive
                  else
                    if channel == 1
                      then ioError (userError "Kubernetes port-forward error")
                      else if BS.null bytes then receive else pure bytes
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

-- Kubernetes' port-forward channels begin with a two-byte little-endian port.
-- Reject malformed frames instead of letting them masquerade as TLS failures.
decodeForwardFrame :: Int -> [Int] -> BS.ByteString -> Either String (Int, Bool, BS.ByteString)
decodeForwardFrame port initialized frame = case BS.uncons frame of
  Just (tag, bytes)
    | tag <= 1 ->
        let channel = fromIntegral tag
         in if channel `elem` initialized
              then Right (channel, False, bytes)
              else
                if bytes == BS.pack [fromIntegral port, fromIntegral (port `div` 256)]
                  then Right (channel, True, BS.empty)
                  else Left "Invalid initial Kubernetes port-forward frame"
  _ -> Left "Invalid Kubernetes port-forward channel"
