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

module Galley.Cassandra.Services where

import Cassandra
import Control.Lens
import Data.Id
import Galley.Cassandra.Queries
import Galley.Cassandra.Store
import Galley.Cassandra.Util
import Galley.Data.Services
import Galley.Effects.ServiceStore hiding (deleteService)
import Galley.Types.Conversations.Members (lmService, newMember)
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import Wire.API.Bot.Service qualified as Bot
import Wire.API.Provider.Service hiding (DeleteService)

-- FUTUREWORK: support adding bots to a remote conversation
addBotMember :: ServiceRef -> BotId -> ConvId -> Client BotMember
addBotMember s bot cnv = do
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    addPrepQuery insertUserConv (botUserId bot, cnv)
    addPrepQuery insertBot (cnv, bot, sid, pid)
  pure (BotMember mem)
  where
    pid = s ^. serviceRefProvider
    sid = s ^. serviceRefId
    mem = (newMember (botUserId bot)) {lmService = Just s}

-- Service --------------------------------------------------------------------

interpretServiceStoreToCassandra ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r
  ) =>
  Sem (ServiceStore ': r) a ->
  Sem r a
interpretServiceStoreToCassandra = interpret $ \case
  CreateService s -> do
    logEffect "ServiceStore.CreateService"
    embedClient $ insertService s
  GetService sr -> do
    logEffect "ServiceStore.GetService"
    embedClient $ lookupService sr
  DeleteService sr -> do
    logEffect "ServiceStore.DeleteService"
    embedClient $ deleteService sr

insertService :: (MonadClient m) => Bot.Service -> m ()
insertService s = do
  let sid = s ^. Bot.serviceRef . serviceRefId
  let pid = s ^. Bot.serviceRef . serviceRefProvider
  let tok = s ^. Bot.serviceToken
  let url = s ^. Bot.serviceUrl
  let fps = Set (s ^. Bot.serviceFingerprints)
  let ena = s ^. Bot.serviceEnabled
  retry x5 $ write insertSrv (params LocalQuorum (pid, sid, url, tok, fps, ena))

lookupService :: (MonadClient m) => ServiceRef -> m (Maybe Bot.Service)
lookupService s =
  fmap toService
    <$> retry x1 (query1 selectSrv (params LocalQuorum (s ^. serviceRefProvider, s ^. serviceRefId)))
  where
    toService (url, tok, Set fps, ena) =
      Bot.newService s url tok fps & set Bot.serviceEnabled ena

deleteService :: (MonadClient m) => ServiceRef -> m ()
deleteService s = retry x5 (write rmSrv (params LocalQuorum (s ^. serviceRefProvider, s ^. serviceRefId)))
