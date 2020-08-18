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

module Federator.API
  ( Api (..),
    module Fed,
  )
where

import Data.Id (ConvId, UserId)
import Data.Qualified (Qualified)
import Imports
import Servant.API
import Servant.API.Generic
import Wire.API.Federation.API.Conversation as Fed hiding (Api)
import Wire.API.Federation.Event as Fed
import Wire.API.User.Client.Prekey (PrekeyBundle)

data Api route = Api
  { _gapiPrekeys ::
      route
        :- "i"
        :> "users"
        :> Capture "id" (Qualified UserId)
        :> "prekeys"
        -- FUTUREWORK(federation):
        -- this should return a version of PrekeyBundle with qualified UserId,
        -- defined in wire-api-federation
        :> Get '[JSON] PrekeyBundle,
    _gapiJoinConversationById ::
      route
        :- "i"
        :> "conversations"
        :> Capture "cnv" (Qualified ConvId)
        :> "join"
        :> ReqBody '[JSON] Fed.JoinConversationByIdRequest
        :> Post '[JSON] (Fed.ConversationUpdateResult Fed.MemberJoin)
  }
  deriving (Generic)

----------------------------------------------------------------------
-- FUTUREWORK: add roundtrip tests for *HttpApiData, *JSON, ...
