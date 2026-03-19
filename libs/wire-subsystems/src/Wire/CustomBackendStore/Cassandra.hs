{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.CustomBackendStore.Cassandra (interpretCustomBackendStoreToCassandra) where

import Cassandra
import Data.Domain (Domain)
import Data.Misc
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import Wire.API.CustomBackend
import Wire.ConversationStore.Cassandra.Instances ()
import Wire.CustomBackendStore (CustomBackendStore (..))
import Wire.Util

interpretCustomBackendStoreToCassandra ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r
  ) =>
  Sem (CustomBackendStore ': r) a ->
  Sem r a
interpretCustomBackendStoreToCassandra = interpret $ \case
  GetCustomBackend dom -> do
    logEffect "CustomBackendStore.GetCustomBackend"
    embedClientInput $ getCustomBackend dom
  SetCustomBackend dom b -> do
    logEffect "CustomBackendStore.SetCustomBackend"
    embedClientInput $ setCustomBackend dom b
  DeleteCustomBackend dom -> do
    logEffect "CustomBackendStore.DeleteCustomBackend"
    embedClientInput $ deleteCustomBackend dom

getCustomBackend :: (MonadClient m) => Domain -> m (Maybe CustomBackend)
getCustomBackend domain =
  fmap toCustomBackend <$> do
    retry x1 $ query1 q (params LocalQuorum (Identity domain))
  where
    toCustomBackend (backendConfigJsonUrl, backendWebappWelcomeUrl) =
      CustomBackend {..}
    q :: PrepQuery R (Identity Domain) (HttpsUrl, HttpsUrl)
    q = "select config_json_url, webapp_welcome_url from custom_backend where domain = ?"

setCustomBackend :: (MonadClient m) => Domain -> CustomBackend -> m ()
setCustomBackend domain CustomBackend {..} = do
  retry x5 $ write q (params LocalQuorum (backendConfigJsonUrl, backendWebappWelcomeUrl, domain))
  where
    q :: PrepQuery W (HttpsUrl, HttpsUrl, Domain) ()
    q = "update custom_backend set config_json_url = ?, webapp_welcome_url = ? where domain = ?"

deleteCustomBackend :: (MonadClient m) => Domain -> m ()
deleteCustomBackend domain = do
  retry x5 $ write q (params LocalQuorum (Identity domain))
  where
    q :: PrepQuery W (Identity Domain) ()
    q = "delete from custom_backend where domain = ?"
