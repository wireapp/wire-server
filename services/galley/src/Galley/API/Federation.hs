-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.API.Federation
  ( PlainApi,
    server,
  )
where

import Data.Id (ConvId)
import Data.IdMapping (MappedOrLocalId (Mapped))
import Data.Qualified (Qualified (_qLocalPart), unqualified)
import qualified Galley.API.Update as Update
import Galley.API.Util (createUserIdMapping)
import Galley.App (Env, Galley, runGalley)
import qualified Galley.Federation as Util.Fed
import Imports
import Network.Wai (Request)
import Servant (Proxy (Proxy))
import Servant.API.Generic (AsApi, ToServant)
import Servant.Server (Handler, Server, ServerT, hoistServer)
import Servant.Server.Generic (AsServerT, genericServerT)
import qualified Wire.API.Federation as Fed
import qualified Wire.API.Federation.Conversation as Fed.Cnv
import qualified Wire.API.Federation.Types.Event as Fed

type PlainApi = ToServant Fed.Api AsApi

server :: Env -> Request -> Server PlainApi
server e r = hoistServer (Proxy @PlainApi) nat serverT
  where
    nat :: Galley a -> Handler a
    nat = liftIO . runGalley e r

serverT :: ServerT PlainApi Galley
serverT = genericServerT routes

routes :: Fed.Api (AsServerT Galley)
routes =
  Fed.Api
    { Fed.conversation = genericServerT conversationRoutes
    }

conversationRoutes :: Fed.Cnv.Api (AsServerT Galley)
conversationRoutes =
  Fed.Cnv.Api
    { Fed.Cnv.joinConversationById = joinConversationById
    }

joinConversationById ::
  Qualified ConvId ->
  Fed.Cnv.JoinConversationByIdRequest ->
  Galley (Fed.Cnv.ConversationUpdateResult Fed.MemberJoin)
joinConversationById qualifiedConvId req = do
  convId <- unqualified <$> ensureConversationIsLocal qualifiedConvId
  userId <- Mapped <$> createUserIdMapping (Fed.Cnv.joinUserId req)
  -- TODO: what is the connection ID used for? how to get one?
  let connId = undefined
  qualifyResult =<< Update.joinConversationById userId connId convId
  where
    qualifyResult r = do
      qr <- Util.Fed.qualifyConversationUpdateResult r
      for qr $ \case
        Fed.DataMemberJoin memberJoin -> pure memberJoin

ensureConversationIsLocal :: Qualified ConvId -> Galley ConvId
ensureConversationIsLocal qualifiedConvId =
  -- TODO: this is a hack, doesn't work in general
  pure (_qLocalPart qualifiedConvId)
