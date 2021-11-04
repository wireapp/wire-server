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

module Galley.Cassandra.Services where

import Cassandra
import Control.Lens
import Data.Id
import Galley.Cassandra.Store
import Galley.Cassandra.Queries
import Galley.Data.Services
import Galley.Effects.ServiceStore (ServiceStore (..))
import Galley.Types hiding (Conversation)
import Galley.Types.Bot
import Galley.Types.Conversations.Members (newMember)
import Imports
import Polysemy
import qualified Polysemy.Reader as P

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
  Members '[Embed IO, P.Reader ClientState] r =>
  Sem (ServiceStore ': r) a ->
  Sem r a
interpretServiceStoreToCassandra = interpret $ \case
  CreateService s -> embedClient $ insertService s
  GetService sr -> embedClient $ lookupService sr
  DeleteService sr -> embedClient $ deleteService sr

insertService :: MonadClient m => Service -> m ()
insertService s = do
  let sid = s ^. serviceRef . serviceRefId
  let pid = s ^. serviceRef . serviceRefProvider
  let tok = s ^. serviceToken
  let url = s ^. serviceUrl
  let fps = Set (s ^. serviceFingerprints)
  let ena = s ^. serviceEnabled
  retry x5 $ write insertSrv (params LocalQuorum (pid, sid, url, tok, fps, ena))

lookupService :: MonadClient m => ServiceRef -> m (Maybe Service)
lookupService s =
  fmap toService
    <$> retry x1 (query1 selectSrv (params LocalQuorum (s ^. serviceRefProvider, s ^. serviceRefId)))
  where
    toService (url, tok, Set fps, ena) =
      newService s url tok fps & set serviceEnabled ena

deleteService :: MonadClient m => ServiceRef -> m ()
deleteService s = retry x5 (write rmSrv (params LocalQuorum (s ^. serviceRefProvider, s ^. serviceRefId)))
