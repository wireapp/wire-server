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

module Web.Scim.Schema.Schema where

import Data.Aeson (FromJSON, ToJSON, Value, parseJSON, toJSON, withText)
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as Parser
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Web.Scim.Capabilities.MetaSchema.Group
import Web.Scim.Capabilities.MetaSchema.ResourceType
import Web.Scim.Capabilities.MetaSchema.SPConfig
import Web.Scim.Capabilities.MetaSchema.Schema
import Web.Scim.Capabilities.MetaSchema.User

-- | All schemas that we support.
data Schema
  = User20
  | ServiceProviderConfig20
  | Group20
  | Schema20
  | ResourceType20
  | ListResponse20
  | Error20
  | PatchOp20
  | CustomSchema Text
  deriving (Show, Eq)

instance FromJSON Schema where
  parseJSON = withText "schema" $ \t -> pure (fromSchemaUri t)

instance ToJSON Schema where
  toJSON = toJSON . getSchemaUri

-- | Get schema URI (e.g. @urn:ietf:params:scim:schemas:core:2.0:User@).
getSchemaUri :: Schema -> Text
getSchemaUri User20 =
  "urn:ietf:params:scim:schemas:core:2.0:User"
getSchemaUri ServiceProviderConfig20 =
  "urn:ietf:params:scim:schemas:core:2.0:ServiceProviderConfig"
getSchemaUri Group20 =
  "urn:ietf:params:scim:schemas:core:2.0:Group"
getSchemaUri Schema20 =
  "urn:ietf:params:scim:schemas:core:2.0:Schema"
getSchemaUri ResourceType20 =
  "urn:ietf:params:scim:schemas:core:2.0:ResourceType"
getSchemaUri ListResponse20 =
  "urn:ietf:params:scim:api:messages:2.0:ListResponse"
getSchemaUri Error20 =
  "urn:ietf:params:scim:api:messages:2.0:Error"
getSchemaUri PatchOp20 =
  "urn:ietf:params:scim:api:messages:2.0:PatchOp"
getSchemaUri (CustomSchema x) =
  x

-- TODO(akshay): Make everything Text, ByteStrings are unnecessary here

-- | Parser for schemas
--
-- NOTE: according to the spec, this parser needs to be case insensitive, but
-- that is literally insane. Won't implement.
pSchema :: [Schema] -> Parser Schema
pSchema supportedSchemas =
  Parser.choice $
    map (\s -> fromSchemaUri . decodeUtf8 <$> Parser.string (encodeUtf8 $ getSchemaUri s)) supportedSchemas

-- | Get a schema by its URI.
--
-- NOTE: case sensitive against the spec.  Same as 'pSchema'.
--
-- TODO(arianvp): probably too lenient. want to only accept valid URNs
-- This means the CustomSchema part might go... We need to kind of
-- rethink how we're  gonna do extensions anyway, as we're gonna have to
-- support multiple extensions, which is currently a bit iffy I think
fromSchemaUri :: Text -> Schema
fromSchemaUri s = case s of
  "urn:ietf:params:scim:schemas:core:2.0:User" ->
    User20
  "urn:ietf:params:scim:schemas:core:2.0:ServiceProviderConfig" ->
    ServiceProviderConfig20
  "urn:ietf:params:scim:schemas:core:2.0:Group" ->
    Group20
  "urn:ietf:params:scim:schemas:core:2.0:Schema" ->
    Schema20
  "urn:ietf:params:scim:schemas:core:2.0:ResourceType" ->
    ResourceType20
  "urn:ietf:params:scim:api:messages:2.0:ListResponse" ->
    ListResponse20
  "urn:ietf:params:scim:api:messages:2.0:Error" ->
    Error20
  "urn:ietf:params:scim:api:messages:2.0:PatchOp" ->
    PatchOp20
  x ->
    CustomSchema x

-- | Get schema description as JSON.
getSchema :: Schema -> Maybe Value
getSchema ServiceProviderConfig20 =
  pure spConfigSchema
getSchema User20 =
  pure userSchema
getSchema Group20 =
  pure groupSchema
getSchema Schema20 =
  pure metaSchema
getSchema ResourceType20 =
  pure resourceSchema
-- Schemas for these types are not in the SCIM standard.
-- FUTUREWORK: write schema definitions anyway.
getSchema ListResponse20 =
  Nothing
getSchema Error20 =
  Nothing
getSchema PatchOp20 =
  Nothing
-- This is not controlled by @hscim@ so we can't write a schema.
-- FUTUREWORK: allow supplying schemas for 'CustomSchema'.
getSchema (CustomSchema _) =
  Nothing
