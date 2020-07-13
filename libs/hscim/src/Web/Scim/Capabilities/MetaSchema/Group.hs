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

module Web.Scim.Capabilities.MetaSchema.Group
  ( groupSchema,
  )
where

import Data.Aeson (Value)
import Data.Aeson.QQ

-- NB: errata for the SCIM RFC says that 'displayName' should have
-- 'required: true'. This makes sense but we're not going to change this for
-- now because nobody seems to care about these schemas anyway.
-- See https://www.rfc-editor.org/errata_search.php?rfc=7643
groupSchema :: Value
groupSchema =
  [aesonQQ|
{
  "id": "urn:ietf:params:scim:schemas:core:2.0:Group",
  "name": "Group",
  "description": "Group",
  "attributes": [
    {
      "name": "displayName",
      "type": "string",
      "multiValued": false,
      "description": "A human-readable name for the Group. REQUIRED.",
      "required": false,
      "caseExact": false,
      "mutability": "readWrite",
      "returned": "default",
      "uniqueness": "none"
    },
    {
      "name": "members",
      "type": "complex",
      "multiValued": true,
      "description": "A list of members of the Group.",
      "required": false,
      "subAttributes": [
        {
          "name": "value",
          "type": "string",
          "multiValued": false,
          "description": "Identifier of the member of this Group.",
          "required": false,
          "caseExact": false,
          "mutability": "immutable",
          "returned": "default",
          "uniqueness": "none"
        },
        {
          "name": "$ref",
          "type": "reference",
          "referenceTypes": [
            "User",
            "Group"
          ],
          "multiValued": false,
          "description": "The URI corresponding to a SCIM resource that is a member of this Group.",
          "required": false,
          "caseExact": false,
          "mutability": "immutable",
          "returned": "default",
          "uniqueness": "none"
        },
        {
          "name": "type",
          "type": "string",
          "multiValued": false,
          "description": "A label indicating the type of resource, e.g., 'User' or 'Group'.",
          "required": false,
          "caseExact": false,
          "canonicalValues": [
            "User",
            "Group"
          ],
          "mutability": "immutable",
          "returned": "default",
          "uniqueness": "none"
        }
      ],
      "mutability": "readWrite",
      "returned": "default"
    }
  ],
  "meta": {
    "resourceType": "Schema",
    "location": "/v2/Schemas/urn:ietf:params:scim:schemas:core:2.0:Group"
  }
}
|]
