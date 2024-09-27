{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

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

module Gundeck.Types.Push.V2
  ( Push (..),
    newPush,
    pushRecipients,
    pushOrigin,
    pushConnections,
    pushOriginConnection,
    pushTransient,
    pushNativeIncludeOrigin,
    pushNativeEncrypt,
    pushNativeAps,
    pushNativePriority,
    pushPayload,
    singletonPayload,
    Recipient (..),
    RecipientClients (..),
    recipient,
    recipientId,
    recipientRoute,
    recipientClients,
    Route (..),
    ApsData,
    ApsLocKey (..),
    ApsSound (..),
    apsData,
    apsLocKey,
    apsLocArgs,
    apsSound,
    apsBadge,

    -- * Priority (re-export)
    Priority (..),

    -- * PushToken (re-export)
    PushTokenList (..),
    PushToken,
    pushToken,
    tokenTransport,
    tokenApp,
    tokenClient,
    token,

    -- * PushToken fields (re-export)
    Token (..),
    Transport (..),
    AppName (..),
  )
where

import Wire.API.Push.V2 -- TODO: just remove this module...
