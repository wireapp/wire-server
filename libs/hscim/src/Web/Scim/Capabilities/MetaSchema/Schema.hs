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

module Web.Scim.Capabilities.MetaSchema.Schema
  ( metaSchema,
  )
where

import Data.Aeson (Value)
import Data.Aeson.QQ

metaSchema :: Value
metaSchema =
  [aesonQQ|
{
  "id": "urn:ietf:params:scim:schemas:core:2.0:Schema",
  "name": "Schema",
  "description": "Specifies the schema that describes a SCIM schema",
  "attributes": [
    {
      "name": "id",
      "type": "string",
      "multiValued": false,
      "description": "The unique URI of the schema. When applicable, service providers MUST specify the URI.",
      "required": true,
      "caseExact": false,
      "mutability": "readOnly",
      "returned": "default",
      "uniqueness": "none"
    },
    {
      "name": "name",
      "type": "string",
      "multiValued": false,
      "description": "The schema's human-readable name. When applicable, service providers MUST specify the name, e.g., 'User'.",
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
      "description": "The schema's human-readable name.  When applicable, service providers MUST specify the name, e.g., 'User'.",
      "required": false,
      "caseExact": false,
      "mutability": "readOnly",
      "returned": "default",
      "uniqueness": "none"
    },
    {
      "name": "attributes",
      "type": "complex",
      "multiValued": true,
      "description": "A complex attribute that includes the attributes of a schema.",
      "required": true,
      "mutability": "readOnly",
      "returned": "default",
      "subAttributes": [
        {
          "name": "name",
          "type": "string",
          "multiValued": false,
          "description": "The attribute's name.",
          "required": true,
          "caseExact": true,
          "mutability": "readOnly",
          "returned": "default",
          "uniqueness": "none"
        },
        {
          "name": "type",
          "type": "string",
          "multiValued": false,
          "description": "The attribute's data type. Valid values include 'string', 'complex', 'boolean', 'decimal', 'integer', 'dateTime', 'reference'.",
          "required": true,
          "canonicalValues": [
            "string",
            "complex",
            "boolean",
            "decimal",
            "integer",
            "dateTime",
            "reference"
          ],
          "caseExact": false,
          "mutability": "readOnly",
          "returned": "default",
          "uniqueness": "none"
        },
        {
          "name": "multiValued",
          "type": "boolean",
          "multiValued": false,
          "description": "A Boolean value indicating an attribute's plurality.",
          "required": true,
          "mutability": "readOnly",
          "returned": "default"
        },
        {
          "name": "description",
          "type": "string",
          "multiValued": false,
          "description": "A human-readable description of the attribute.",
          "required": false,
          "caseExact": true,
          "mutability": "readOnly",
          "returned": "default",
          "uniqueness": "none"
        },
        {
          "name": "required",
          "type": "boolean",
          "multiValued": false,
          "description": "A boolean value indicating whether or not the attribute is required.",
          "required": false,
          "mutability": "readOnly",
          "returned": "default"
        },
        {
          "name": "canonicalValues",
          "type": "string",
          "multiValued": true,
          "description": "A collection of canonical values.  When applicable, service providers MUST specify the canonical types, e.g., 'work', 'home'.",
          "required": false,
          "caseExact": true,
          "mutability": "readOnly",
          "returned": "default",
          "uniqueness": "none"
        },
        {
          "name": "caseExact",
          "type": "boolean",
          "multiValued": false,
          "description": "A Boolean value indicating whether or not a string attribute is case sensitive.",
          "required": false,
          "mutability": "readOnly",
          "returned": "default"
        },
        {
          "name": "mutability",
          "type": "string",
          "multiValued": false,
          "description": "Indicates whether or not an attribute is modifiable.",
          "required": false,
          "caseExact": true,
          "mutability": "readOnly",
          "returned": "default",
          "uniqueness": "none",
          "canonicalValues": [
            "readOnly",
            "readWrite",
            "immutable",
            "writeOnly"
          ]
        },
        {
          "name": "returned",
          "type": "string",
          "multiValued": false,
          "description": "Indicates when an attribute is returned in a response (e.g., to a query).",
          "required": false,
          "caseExact": true,
          "mutability": "readOnly",
          "returned": "default",
          "uniqueness": "none",
          "canonicalValues": [
            "always",
            "never",
            "default",
            "request"
          ]
        },
        {
          "name": "uniqueness",
          "type": "string",
          "multiValued": false,
          "description": "Indicates how unique a value must be.",
          "required": false,
          "caseExact": true,
          "mutability": "readOnly",
          "returned": "default",
          "uniqueness": "none",
          "canonicalValues": [
            "none",
            "server",
            "global"
          ]
        },
        {
          "name": "referenceTypes",
          "type": "string",
          "multiValued": true,
          "description": "Used only with an attribute of type 'reference'.  Specifies a SCIM resourceType that a reference attribute MAY refer to, e.g., 'User'.",
          "required": false,
          "caseExact": true,
          "mutability": "readOnly",
          "returned": "default",
          "uniqueness": "none"
        },
        {
          "name": "subAttributes",
          "type": "complex",
          "multiValued": true,
          "description": "Used to define the sub-attributes of a complex attribute.",
          "required": false,
          "mutability": "readOnly",
          "returned": "default",
          "subAttributes": [
            {
              "name": "name",
              "type": "string",
              "multiValued": false,
              "description": "The attribute's name.",
              "required": true,
              "caseExact": true,
              "mutability": "readOnly",
              "returned": "default",
              "uniqueness": "none"
            },
            {
              "name": "type",
              "type": "string",
              "multiValued": false,
              "description": "The attribute's data type. Valid values include 'string', 'complex', 'boolean', 'decimal', 'integer', 'dateTime', 'reference'.",
              "required": true,
              "caseExact": false,
              "mutability": "readOnly",
              "returned": "default",
              "uniqueness": "none",
              "canonicalValues": [
                "string",
                "complex",
                "boolean",
                "decimal",
                "integer",
                "dateTime",
                "reference"
              ]
            },
            {
              "name": "multiValued",
              "type": "boolean",
              "multiValued": false,
              "description": "A Boolean value indicating an attribute's plurality.",
              "required": true,
              "mutability": "readOnly",
              "returned": "default"
            },
            {
              "name": "description",
              "type": "string",
              "multiValued": false,
              "description": "A human-readable description of the attribute.",
              "required": false,
              "caseExact": true,
              "mutability": "readOnly",
              "returned": "default",
              "uniqueness": "none"
            },
            {
              "name": "required",
              "type": "boolean",
              "multiValued": false,
              "description": "A boolean value indicating whether or not the attribute is required.",
              "required": false,
              "mutability": "readOnly",
              "returned": "default"
            },
            {
              "name": "canonicalValues",
              "type": "string",
              "multiValued": true,
              "description": "A collection of canonical values.  When applicable, service providers MUST specify the canonical types, e.g., 'work', 'home'.",
              "required": false,
              "caseExact": true,
              "mutability": "readOnly",
              "returned": "default",
              "uniqueness": "none"
            },
            {
              "name": "caseExact",
              "type": "boolean",
              "multiValued": false,
              "description": "A Boolean value indicating whether or not a string attribute is case sensitive.",
              "required": false,
              "mutability": "readOnly",
              "returned": "default"
            },
            {
              "name": "mutability",
              "type": "string",
              "multiValued": false,
              "description": "Indicates whether or not an attribute is modifiable.",
              "required": false,
              "caseExact": true,
              "mutability": "readOnly",
              "returned": "default",
              "uniqueness": "none",
              "canonicalValues": [
                "readOnly",
                "readWrite",
                "immutable",
                "writeOnly"
              ]
            },
            {
              "name": "returned",
              "type": "string",
              "multiValued": false,
              "description": "Indicates when an attribute is returned in a response (e.g., to a query).",
              "required": false,
              "caseExact": true,
              "mutability": "readOnly",
              "returned": "default",
              "uniqueness": "none",
              "canonicalValues": [
                "always",
                "never",
                "default",
                "request"
              ]
            },
            {
              "name": "uniqueness",
              "type": "string",
              "multiValued": false,
              "description": "Indicates how unique a value must be.",
              "required": false,
              "caseExact": true,
              "mutability": "readOnly",
              "returned": "default",
              "uniqueness": "none",
              "canonicalValues": [
                "none",
                "server",
                "global"
              ]
            },
            {
              "name": "referenceTypes",
              "type": "string",
              "multiValued": false,
              "description": "Used only with an attribute of type 'reference'.  Specifies a SCIM resourceType that a reference attribute MAY refer to, e.g., 'User'.",
              "required": false,
              "caseExact": true,
              "mutability": "readOnly",
              "returned": "default",
              "uniqueness": "none"
            }
          ]
        }
      ]
    }
  ]
}
|]
