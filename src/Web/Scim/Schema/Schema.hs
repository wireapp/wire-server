
module Web.Scim.Schema.Schema where

import           Web.Scim.Capabilities.MetaSchema.User
import           Web.Scim.Capabilities.MetaSchema.SPConfig
import           Web.Scim.Capabilities.MetaSchema.Group
import           Web.Scim.Capabilities.MetaSchema.Schema
import           Web.Scim.Capabilities.MetaSchema.ResourceType

import           Data.Text
import           Data.Aeson
import Data.Attoparsec.ByteString (Parser,  (<?>))
import Control.Applicative ((<|>))

-- | All schemas that we support.
data Schema = User20
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

-- | Parser for known schemas. Fails on unknown schemas (E.g. CustomSchema escape hatch doesn't work)
--
-- NOTE: according to the spec, this parser needs to be case insensitive, but
-- that is literally insane. Won't implement.
pSchema :: Parser Schema
pSchema =
  (User20
    <$ "urn:ietf:params:scim:schemas:core:2.0:User" <|>
  ServiceProviderConfig20
    <$ "urn:ietf:params:scim:schemas:core:2.0:ServiceProviderConfig" <|>
  Group20
    <$ "urn:ietf:params:scim:schemas:core:2.0:Group" <|>
  Schema20
    <$ "urn:ietf:params:scim:schemas:core:2.0:Schema" <|>
  ResourceType20
    <$ "urn:ietf:params:scim:schemas:core:2.0:ResourceType" <|>
  ListResponse20
    <$ "urn:ietf:params:scim:api:messages:2.0:ListResponse" <|>
  Error20
    <$ "urn:ietf:params:scim:api:messages:2.0:Error" <|>
  PatchOp20
    <$ "urn:ietf:params:scim:api:messages:2.0:PatchOp") <?> "unknown schema"

-- | Get a schema by its URI.
--
-- NOTE: cas sensitive against the spec.  Same as 'pSchema'.
--
-- FUTUREWORK: implement this in terms of 'pSchema': parse all the non-custom schemas with
-- 'pSchema; in case of error, use the parser *input* as the custom schema.
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
