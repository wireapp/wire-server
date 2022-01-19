-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Brig.Effects.SFT
  ( SFTError (..),
    SFTGetResponse (..),
    SFT (..),
    sftGetAllServers,
    interpretSFT,
    interpretSFTInMemory,
  )
where

import qualified Data.Aeson as Aeson
import Data.ByteString.Conversion
import qualified Data.Map as Map
import Data.Misc
import Data.Schema
import Data.String.Conversions (cs)
import Imports hiding (intercalate)
import Network.HTTP.Client
import Polysemy
import Polysemy.Internal
import Polysemy.TinyLog
import qualified System.Logger as Log
import URI.ByteString (uriPath)
import Wire.API.Call.Config

newtype SFTError = SFTError {unSFTError :: String}
  deriving (Eq, Show)

newtype SFTGetResponse = SFTGetResponse
  {unSFTGetResponse :: Either SFTError [SFTServer]}
  deriving newtype (Eq)

data SFT m a where
  SFTGetAllServers :: HttpsUrl -> SFT m SFTGetResponse

sftGetAllServers :: Member SFT r => HttpsUrl -> Sem r SFTGetResponse
sftGetAllServers = send . SFTGetAllServers

interpretSFT :: Members [Embed IO, TinyLog] r => Manager -> Sem (SFT ': r) a -> Sem r a
interpretSFT httpManager = interpret $ \(SFTGetAllServers url) -> do
  let urlWithPath = ensureHttpsUrl $ (httpsUrl url) {uriPath = "/sft_servers_all.json"}
      req = parseRequest_ . cs . toByteString' $ urlWithPath
  responseURLsRaw <- liftIO (responseBody <$> httpLbs req httpManager)
  let eList = Aeson.eitherDecode @AllURLs responseURLsRaw
      res = bimap SFTError (fmap sftServer . unAllURLs) eList
  void $ case res of
    Left e ->
      err $
        Log.field "sft_err" (show e) . Log.msg ("Error for URL: " <> toByteString' urlWithPath)
    Right servers ->
      info $
        Log.field "IPv4s" (show servers) . Log.msg ("Fetched the following server URLs" :: ByteString)
  pure . SFTGetResponse $ res

newtype AllURLs = AllURLs {unAllURLs :: [HttpsUrl]}
  deriving (Aeson.FromJSON) via Schema AllURLs

instance ToSchema AllURLs where
  schema =
    object "AllURLs" $
      AllURLs
        <$> unAllURLs .= field "sft_servers_all" (array schema)

interpretSFTInMemory ::
  Member TinyLog r =>
  Map HttpsUrl SFTGetResponse ->
  Sem (SFT ': r) a ->
  Sem r a
interpretSFTInMemory m = interpret $ \(SFTGetAllServers url) ->
  case Map.lookup url m of
    Nothing -> do
      let msg = "No value in the lookup map"
      err $ Log.field "url" (show url) . Log.msg (cs msg :: ByteString)
      pure . SFTGetResponse . Left . SFTError $ msg
    Just ss -> pure ss
