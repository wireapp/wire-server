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

module Network.Wai.Utilities.ZAuth
  ( ZAuthType (..),
    zauthType,
    zauth,
    zauthUserId,
    zauthConnId,
    zauthBotId,
    zauthConvId,
    zauthProviderId,
    (<&.),
    (.&>),
  )
where

import Data.ByteString.Conversion
import Data.Id
import Imports
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai.Predicate
import Network.Wai.Predicate.Request

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

-- | A token type is present if the request was authenticated.
zauthType :: (HasHeaders r) => Predicate r Error ZAuthType
zauthType = zheader "Z-Type"

-- | Require a specific token type to be used.
zauth :: (HasHeaders r) => ZAuthType -> Predicate r Error ()
zauth t = do
  r <- zauthType
  pure $ case r of
    Okay _ z | z == t -> Okay 0 ()
    _ -> Fail accessDenied

-- | A zauth user ID is present if 'zauthType' is either 'ZAuthAccess'
-- or 'ZAuthUser'.
zauthUserId :: (HasHeaders r) => Predicate r Error UserId
zauthUserId = zheader "Z-User"

-- | A zauth connection ID is present if 'zauthType' is 'ZAuthAccess'.
zauthConnId :: (HasHeaders r) => Predicate r Error ConnId
zauthConnId = zheader "Z-Connection"

-- | A zauth bot ID is present if 'zauthType' is 'ZAuthBot'.
zauthBotId :: (HasHeaders r) => Predicate r Error BotId
zauthBotId = zheader "Z-Bot"

-- | A zauth conversation ID is present if 'zauthType' is 'ZAuthBot'.
zauthConvId :: (HasHeaders r) => Predicate r Error ConvId
zauthConvId = zheader "Z-Conversation"

-- | A provider ID is present if 'zauthType' is either 'ZAuthBot'
-- or 'ZAuthProvider'.
zauthProviderId :: (HasHeaders r) => Predicate r Error ProviderId
zauthProviderId = zheader "Z-Provider"

-- Extra Predicate Combinators ------------------------------------------------

-- Variations of '.&.' that keep only the result of the left or right
-- predicate, respectively. These might be useful to add upstream
-- in 'wai-predicates'.

infixr 3 <&.

infixr 3 .&>

(<&.) :: Predicate a f t -> Predicate a f t' -> Predicate a f t
(<&.) a b = fmap (fmap hd) (a .&. b)

(.&>) :: Predicate a f t -> Predicate a f t' -> Predicate a f t'
(.&>) a b = fmap (fmap tl) (a .&. b)

-- Internal -------------------------------------------------------------------

-- | Missing or invalid zauth-related headers due to a misconfiguration
-- between the zauth ACL and / or API handlers should yield an opaque 403
-- error, in order not to leak such details to clients on public API endpoints.
zheader :: (HasHeaders r, FromByteString a) => HeaderName -> Predicate r Error a
zheader = fmap (result (Fail . const accessDenied) Okay) . header

accessDenied :: Error
accessDenied = setMessage "Access denied" (err status403)
