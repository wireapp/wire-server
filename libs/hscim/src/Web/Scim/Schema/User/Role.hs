{-# LANGUAGE DerivingVia #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Web.Scim.Schema.User.Role where

import Data.Aeson (FromJSON (..), ToJSON, Value (String))
import qualified Data.OpenApi as S
import Data.Schema
import Data.Text (Text)
import Web.Scim.Schema.Common (ScimBool (..))

-- | A SCIM @roles@ entry. RFC 7643 defines @roles@ as a complex, multi-valued
-- attribute, so each element is an object with (optional) sub-attributes rather
-- than a bare string.
data Role = Role
  { value :: Maybe Text,
    typ :: Maybe Text,
    display :: Maybe Text,
    primary :: Maybe ScimBool
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, S.ToSchema) via (Schema Role)

instance ToSchema Role where
  schema =
    object
      $ Role
      <$> (value .= maybe_ (optField "value" schema))
      <*> (typ .= maybe_ (optField "type" schema))
      <*> (display .= maybe_ (optField "display" schema))
      <*> (primary .= maybe_ (optField "primary" schema))

-- | We accept both the RFC-compliant object form (parsed via the schema above)
-- and a plain string (for backwards compatibility with clients that send
-- @"roles": ["member"]@).
instance FromJSON Role where
  parseJSON (String s) = pure $ Role (Just s) Nothing Nothing Nothing
  parseJSON v = schemaIn (schema @Role) v
