{-# LANGUAGE QuasiQuotes #-}

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

module Web.Scim.Capabilities.MetaSchema.ResourceType
  ( resourceSchema,
  )
where

import Data.Aeson (Value)
import Data.Aeson.QQ

resourceSchema :: Value
resourceSchema =
  [aesonQQ|
{
  "id": "urn:ietf:params:scim:schemas:core:2.0:ResourceType",
  "name": "ResourceType",
  "description": "Specifies the schema that describes a SCIM resource type",
  "attributes": [
    {
      "name": "id",
      "type": "string",
      "multiValued": false,
      "description": "The resource type's server unique id. May be the same as the 'name' attribute.",
      "required": false,
      "caseExact": false,
      "mutability": "readOnly",
      "returned": "default",
      "uniqueness": "none"
    },
    {
      "name": "name",
      "type": "string",
      "multiValued": false,
      "description": "The resource type name.  When applicable, service providers MUST specify the name, e.g., 'User'.",
      "required": true,
      "caseExact": false,
      "mutability": "readOnly",
      "returned": "default",
      "uniqueness": "none"
    },
    {
      "name": "description",
      "type": "string",
      "multiValued": false,
      "description": "The resource type's human-readable description.  When applicable, service providers MUST specify the description.",
      "required": false,
      "caseExact": false,
      "mutability": "readOnly",
      "returned": "default",
      "uniqueness": "none"
    },
    {
      "name": "endpoint",
      "type": "reference",
      "referenceTypes": [
        "uri"
      ],
      "multiValued": false,
      "description": "The resource type's HTTP-addressable endpoint relative to the Base URL, e.g., '\/Users'.",
      "required": true,
      "caseExact": false,
      "mutability": "readOnly",
      "returned": "default",
      "uniqueness": "none"
    },
    {
      "name": "schema",
      "type": "reference",
      "referenceTypes": [
        "uri"
      ],
      "multiValued": false,
      "description": "The resource type's primary\/base schema URI.",
      "required": true,
      "caseExact": true,
      "mutability": "readOnly",
      "returned": "default",
      "uniqueness": "none"
    },
    {
      "name": "schemaExtensions",
      "type": "complex",
      "multiValued": false,
      "description": "A list of URIs of the resource type's schema extensions.",
      "required": true,
      "mutability": "readOnly",
      "returned": "default",
      "subAttributes": [
        {
          "name": "schema",
          "type": "reference",
          "referenceTypes": [
            "uri"
          ],
          "multiValued": false,
          "description": "The URI of a schema extension.",
          "required": true,
          "caseExact": true,
          "mutability": "readOnly",
          "returned": "default",
          "uniqueness": "none"
        },
        {
          "name": "required",
          "type": "boolean",
          "multiValued": false,
          "description": "A Boolean value that specifies whether or not the schema extension is required for the resource type.  If true, a resource of this type MUST include this schema extension and also include any attributes declared as required in this schema extension. If false, a resource of this type MAY omit this schema extension.",
          "required": true,
          "mutability": "readOnly",
          "returned": "default"
        }
      ]
    }
  ]
}
|]
