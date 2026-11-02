{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
-- SPDX-License-Identifier: AGPL-3.0-only

module Testlib.Ingress.Kubeconfig
  ( ConnectionConfig (..),
    Material (..),
    parseConfig,
    loadConfig,
    readMaterial,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (unless, when)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import Data.List (find)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text.Encoding as Text
import qualified Data.Yaml as Yaml
import System.Directory (getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath
import Prelude

-- These contain credentials; deliberately do not derive Show.
data Material = Inline BS.ByteString | FromFile FilePath

data ConnectionConfig = ConnectionConfig
  { server :: String,
    authority :: Material,
    clientCertificate :: Maybe (Material, Material),
    bearerToken :: Maybe Material
  }

data Named a = Named String a

parseNamed :: (FromJSON a) => Key -> Value -> Parser (Named a)
parseNamed key = withObject "named kubeconfig entry" $ \o -> Named <$> o .: "name" <*> o .: key

select :: String -> [Named a] -> Parser a
select name entries = case find (\(Named candidate _) -> candidate == name) entries of
  Just (Named _ value) -> pure value
  Nothing -> fail $ "Missing kubeconfig entry: " <> name

parseConfig :: Value -> Parser ConnectionConfig
parseConfig = withObject "kubeconfig" $ \o -> do
  current <- o .: "current-context"
  contexts <- o .: "contexts" >>= traverse (parseNamed "context")
  context <- select current contexts
  clusterName <- context .: "cluster"
  userName <- context .: "user"
  clusters <- o .: "clusters" >>= traverse (parseNamed "cluster")
  users <- o .: "users" >>= traverse (parseNamed "user")
  cluster <- select clusterName clusters
  user <- select userName users
  insecure <- cluster .:? "insecure-skip-tls-verify" .!= False
  when insecure $ fail "Insecure Kubernetes TLS verification is not supported"
  rejectFields ["exec", "auth-provider"] user
  rejectFields ["tls-server-name", "proxy-url"] cluster
  server <- cluster .: "server"
  authority <- requiredMaterial cluster "certificate-authority-data" "certificate-authority"
  certificate <- optionalMaterial user "client-certificate-data" "client-certificate"
  key <- optionalMaterial user "client-key-data" "client-key"
  clientCertificate <- case (certificate, key) of
    (Nothing, Nothing) -> pure Nothing
    (Just cert, Just privateKey) -> pure $ Just (cert, privateKey)
    _ -> fail "Client certificate and key must be supplied together"
  token <- fmap (Inline . Text.encodeUtf8) <$> user .:? "token"
  tokenFile <- fmap FromFile <$> user .:? "tokenFile"
  let bearerToken = token <|> tokenFile
  when (isNothing clientCertificate && isNothing bearerToken) $ fail "Kubeconfig needs a client certificate or bearer token"
  pure ConnectionConfig {server, authority, clientCertificate, bearerToken}
  where
    rejectFields keys o = when (any (`KeyMap.member` o) keys) $ fail "Unsupported kubeconfig option (exec/auth-provider, proxy-url or tls-server-name)"

optionalMaterial :: Object -> Key -> Key -> Parser (Maybe Material)
optionalMaterial o embedded path = do
  encoded <- o .:? embedded
  file <- o .:? path
  case encoded of
    Just bytes -> Just . Inline <$> either (const $ fail "Invalid base64 kubeconfig data") pure (Base64.decode $ Text.encodeUtf8 bytes)
    Nothing -> pure (FromFile <$> file)

requiredMaterial :: Object -> Key -> Key -> Parser Material
requiredMaterial o embedded path = optionalMaterial o embedded path >>= maybe (fail "Missing kubeconfig certificate material") pure

loadConfig :: IO (FilePath, ConnectionConfig)
loadConfig = do
  home <- getHomeDirectory
  path <- fromMaybe (home </> ".kube/config") <$> lookupEnv "KUBECONFIG"
  unless (length (splitSearchPath path) == 1) $ ioError (userError "Use a single kubeconfig file")
  value <- Yaml.decodeFileThrow path
  config <- either (ioError . userError) pure (parseEither parseConfig value)
  pure (path, config)

readMaterial :: FilePath -> Material -> IO BS.ByteString
readMaterial _ (Inline bytes) = pure bytes
readMaterial config (FromFile path) = BS.readFile $ if isAbsolute path then path else takeDirectory config </> path
