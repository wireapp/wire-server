{-# LANGUAGE OverloadedStrings #-}

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

module Network.Wai.Utilities.ZAuth (ZAuthType (..)) where

import Data.ByteString.Conversion (FromByteString (..))
import Imports

-- ZAuth headers --------------------------------------------------------------

-- | Identifies the type of token used in an authenticated request.
data ZAuthType
  = -- | (Typically short-lived) access token.
    ZAuthAccess
  | -- | A user (aka refresh) token that can itself be used to
    -- obtain (short-lived) access tokens.
    ZAuthUser
  | -- | A bot token scoped to a specific bot and conversation,
    -- and issued to a certain service provider.
    ZAuthBot
  | -- | A provider token scoped to the provider management API.
    ZAuthProvider
  deriving (Eq, Show, Enum, Bounded, Ord)

instance FromByteString ZAuthType where
  parser = do
    t <- parser
    case (t :: ByteString) of
      "access" -> pure ZAuthAccess
      "user" -> pure ZAuthUser
      "bot" -> pure ZAuthBot
      "provider" -> pure ZAuthProvider
      _ -> fail $ "Invalid ZAuth type: " ++ show t
