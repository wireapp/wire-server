
module Web.Scim.Schema.Schema where

import           Web.Scim.Capabilities.MetaSchema.User
import           Web.Scim.Capabilities.MetaSchema.SPConfig
import           Web.Scim.Capabilities.MetaSchema.Group
import           Web.Scim.Capabilities.MetaSchema.Schema
import           Web.Scim.Capabilities.MetaSchema.ResourceType

import           Data.Text
import           Data.Aeson

-- | All schemas that we support.
data Schema = User20
            | ServiceProviderConfig20
            | Group20
            | Schema20
            | ResourceType20
            | ListResponse2_0
            | Error2_0
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
getSchemaUri ListResponse2_0 =
  "urn:ietf:params:scim:api:messages:2.0:ListResponse"
getSchemaUri Error2_0 =
  "urn:ietf:params:scim:api:messages:2.0:Error"
getSchemaUri (CustomSchema x) =
  x

-- | Get a schema by its URI.
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
    ListResponse2_0
  "urn:ietf:params:scim:api:messages:2.0:Error" ->
    Error2_0
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
getSchema ListResponse2_0 =
  Nothing
getSchema Error2_0 =
  Nothing
-- This is not controlled by @hscim@ so we can't write a schema.
-- FUTUREWORK: allow supplying schemas for 'CustomSchema'.
getSchema (CustomSchema _) =
  Nothing
