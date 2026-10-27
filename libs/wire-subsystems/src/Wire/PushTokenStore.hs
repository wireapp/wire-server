{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

-- | The user push-token store, extracted from "Gundeck.Push.Data".
--
-- The ARN travels as plain 'Text' here: 'EndpointArn' is gundeck-owned and
-- unreachable from wire-subsystems. The conversion happens at the gundeck
-- facade ("Gundeck.Push.Data").
module Wire.PushTokenStore
  ( PushTokenStore (..),
    lookupPushTokens,
    insertPushToken,
    updatePushTokenArn,
    deletePushToken,
    erasePushTokens,
    PushTokenRow,
  )
where

import Cassandra (Consistency)
import Data.Id (ClientId, ConnId, UserId)
import Imports
import Polysemy
import Wire.API.Push.V2 (AppName, Token, Transport)

-- | One row of @user_push@, in the column order of the Cassandra select:
-- (usr, transport, app, ptoken, arn, connection, client).
type PushTokenRow =
  ( UserId,
    Transport,
    AppName,
    Token,
    Maybe Text,
    ConnId,
    Maybe ClientId
  )

data PushTokenStore m a where
  LookupPushTokens :: UserId -> Consistency -> PushTokenStore m [PushTokenRow]
  InsertPushToken :: UserId -> Transport -> AppName -> Token -> Text -> ConnId -> ClientId -> PushTokenStore m ()
  UpdatePushTokenArn :: UserId -> Transport -> AppName -> Token -> Text -> PushTokenStore m ()
  DeletePushToken :: UserId -> Transport -> AppName -> Token -> PushTokenStore m ()
  ErasePushTokens :: UserId -> PushTokenStore m ()

makeSem ''PushTokenStore
