{-# LANGUAGE QuasiQuotes #-}

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

module Web.Scim.Capabilities.MetaSchema.SPConfig
  ( spConfigSchema,
  )
where

import Data.Aeson (Value)
import Data.Aeson.QQ

-- NB: it looks like 'authenticationSchemes' should also have a 'type'
-- attribute. The sample schema from the RFC doesn't list it, though.
spConfigSchema :: Value
spConfigSchema =
  [aesonQQ|
{
  "id": "urn:ietf:params:scim:schemas:core:2.0:ServiceProviderConfig",
  "name": "Service Provider Configuration",
  "description": "Schema for representing the service provider's configuration",
  "attributes": [
    {
      "name": "documentationUri",
      "type": "reference",
      "referenceTypes": [
        "external"
      ],
      "multiValued": false,
      "description": "An HTTP-addressable URL pointing to the service provider's human-consumable help documentation.",
      "required": false,
      "caseExact": false,
      "mutability": "readOnly",
      "returned": "default",
      "uniqueness": "none"
    },
    {
      "name": "patch",
      "type": "complex",
      "multiValued": false,
      "description": "A complex type that specifies PATCH configuration options.",
      "required": true,
      "returned": "default",
      "mutability": "readOnly",
      "subAttributes": [
        {
          "name": "supported",
          "type": "boolean",
          "multiValued": false,
          "description": "A Boolean value specifying whether or not the operation is supported.",
          "required": true,
          "mutability": "readOnly",
          "returned": "default"
        }
      ]
    },
    {
      "name": "bulk",
      "type": "complex",
      "multiValued": false,
      "description": "A complex type that specifies bulk configuration options.",
      "required": true,
      "returned": "default",
      "mutability": "readOnly",
      "subAttributes": [
        {
          "name": "supported",
          "type": "boolean",
          "multiValued": false,
          "description": "A Boolean value specifying whether or not the operation is supported.",
          "required": true,
          "mutability": "readOnly",
          "returned": "default"
        },
        {
          "name": "maxOperations",
          "type": "integer",
          "multiValued": false,
          "description": "An integer value specifying the maximum number of operations.",
          "required": true,
          "mutability": "readOnly",
          "returned": "default",
          "uniqueness": "none"
        },
        {
          "name": "maxPayloadSize",
          "type": "integer",
          "multiValued": false,
          "description": "An integer value specifying the maximum payload size in bytes.",
          "required": true,
          "mutability": "readOnly",
          "returned": "default",
          "uniqueness": "none"
        }
      ]
    },
    {
      "name": "filter",
      "type": "complex",
      "multiValued": false,
      "description": "A complex type that specifies FILTER options.",
      "required": true,
      "returned": "default",
      "mutability": "readOnly",
      "subAttributes": [
        {
          "name": "supported",
          "type": "boolean",
          "multiValued": false,
          "description": "A Boolean value specifying whether or not the operation is supported.",
          "required": true,
          "mutability": "readOnly",
          "returned": "default"
        },
        {
          "name": "maxResults",
          "type": "integer",
          "multiValued": false,
          "description": "An integer value specifying the maximum number of resources returned in a response.",
          "required": true,
          "mutability": "readOnly",
          "returned": "default",
          "uniqueness": "none"
        }
      ]
    },
    {
      "name": "changePassword",
      "type": "complex",
      "multiValued": false,
      "description": "A complex type that specifies configuration options related to changing a password.",
      "required": true,
      "returned": "default",
      "mutability": "readOnly",
      "subAttributes": [
        {
          "name": "supported",
          "type": "boolean",
          "multiValued": false,
          "description": "A Boolean value specifying whether or not the operation is supported.",
          "required": true,
          "mutability": "readOnly",
          "returned": "default"
        }
      ]
    },
    {
      "name": "sort",
      "type": "complex",
      "multiValued": false,
      "description": "A complex type that specifies sort result options.",
      "required": true,
      "returned": "default",
      "mutability": "readOnly",
      "subAttributes": [
        {
          "name": "supported",
          "type": "boolean",
          "multiValued": false,
          "description": "A Boolean value specifying whether or not the operation is supported.",
          "required": true,
          "mutability": "readOnly",
          "returned": "default"
        }
      ]
    },
    {
      "name": "authenticationSchemes",
      "type": "complex",
      "multiValued": true,
      "description": "A complex type that specifies supported authentication scheme properties.",
      "required": true,
      "returned": "default",
      "mutability": "readOnly",
      "subAttributes": [
        {
          "name": "name",
          "type": "string",
          "multiValued": false,
          "description": "The common authentication scheme name, e.g., HTTP Basic.",
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
          "description": "A description of the authentication scheme.",
          "required": true,
          "caseExact": false,
          "mutability": "readOnly",
          "returned": "default",
          "uniqueness": "none"
        },
        {
          "name": "specUri",
          "type": "reference",
          "referenceTypes": [
            "external"
          ],
          "multiValued": false,
          "description": "An HTTP-addressable URL pointing to the authentication scheme's specification.",
          "required": false,
          "caseExact": false,
          "mutability": "readOnly",
          "returned": "default",
          "uniqueness": "none"
        },
        {
          "name": "documentationUri",
          "type": "reference",
          "referenceTypes": [
            "external"
          ],
          "multiValued": false,
          "description": "An HTTP-addressable URL pointing to the authentication scheme's usage documentation.",
          "required": false,
          "caseExact": false,
          "mutability": "readOnly",
          "returned": "default",
          "uniqueness": "none"
        }
      ]
    }
  ]
}
|]
