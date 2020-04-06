{-# LANGUAGE RecordWildCards #-}

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

module Federator.Impl
  ( app,
  )
where

import Brig.Types (PrekeyBundle (PrekeyBundle))
import Data.Handle (Handle)
import Data.Id (ConvId, UserId, makeIdOpaque, randomId)
import Data.Qualified (Qualified (Qualified, _qDomain, _qLocalPart))
import qualified Federator.API as API
import Federator.App (AppIO, runAppT)
import Federator.Types (Env)
import Imports
import Network.Wai (Application)
import Servant.API.Generic ()
import Servant.Server (Handler)
import Servant.Server.Generic (AsServerT, genericServeT)
import qualified Wire.API.Federation.Conversation as Fed
import qualified Wire.API.Federation.Types.Event as Fed

app :: Env -> Application
app env = genericServeT nat api
  where
    nat :: AppIO a -> Handler a
    nat = liftIO . runAppT env

api :: API.API (AsServerT AppIO)
api =
  API.API
    { _gapiSearch,
      _gapiPrekeys,
      _gapiJoinConversationById
    }

-- | dummy implementation
_gapiSearch :: Qualified Handle -> AppIO API.FUser
_gapiSearch qualifiedHandle = do
  uuid <- randomId
  let qualifiedId = Qualified uuid (_qDomain qualifiedHandle)
  pure $ API.FUser qualifiedHandle qualifiedId

-- | dummy implementation
_gapiPrekeys :: Qualified UserId -> AppIO PrekeyBundle
_gapiPrekeys qualifiedUser = do
  pure $ PrekeyBundle (unsafeMakeOpaque qualifiedUser) []
  where
    -- FUTUREWORK(federation):
    -- this is unsafe, we should use the qualified ID (once the API type allows it)
    unsafeMakeOpaque = makeIdOpaque . _qLocalPart

-- | dummy implementation
_gapiJoinConversationById :: Qualified ConvId -> API.JoinConversationByIdRequest -> AppIO (Fed.ConversationUpdateResult Fed.MemberJoin)
_gapiJoinConversationById _ _ = do
  pure Fed.ConversationUnchanged
