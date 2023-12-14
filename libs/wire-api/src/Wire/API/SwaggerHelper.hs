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

module Wire.API.SwaggerHelper where

import Control.Lens
import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict.InsOrd
import Data.OpenApi hiding (Contact, Header, Schema, ToSchema)
import Data.OpenApi qualified as S
import Data.Text qualified as T
import Imports hiding (head)

cleanupSwagger :: OpenApi -> OpenApi
cleanupSwagger =
  (S.security %~ nub)
    -- sanitise definitions
    . (S.components . S.schemas . traverse %~ sanitise)
    -- strip the default errors
    . ( S.allOperations
          . S.responses
          . S.responses
          %~ foldrWithKey stripDefaultErrors mempty
      )
    -- sanitise general responses
    . (S.components . S.responses . traverse . S.content . traverse . S.schema . _Just . S._Inline %~ sanitise)
    -- sanitise all responses of all paths
    . ( S.allOperations
          . S.responses
          . S.responses
          . traverse
          . S._Inline
          . S.content
          . traverse
          . S.schema
          . _Just
          . S._Inline
          %~ sanitise
      )
  where
    sanitise :: S.Schema -> S.Schema
    sanitise =
      (S.properties . traverse . S._Inline %~ sanitise)
        . (S.required %~ nubOrd)
        . (S.enum_ . _Just %~ nub)
    -- servant-openapi and servant-swagger both insert default responses with codes 404 and 400.
    -- They have a simple structure that we can match against, and remove from the final structure.
    stripDefaultErrors :: HttpStatusCode -> Referenced Response -> Responses' -> Responses'
    stripDefaultErrors code resp resps =
      case code of
        400 -> case resp ^? _Inline . S.description of
          (Just desc) ->
            if "Invalid "
              `T.isPrefixOf` desc
              && resp
                ^? _Inline
                  . links
                == pure mempty
              && resp
                ^? _Inline
                  . content
                == pure mempty
              && resp
                ^? _Inline
                  . headers
                == pure mempty
              then resps
              else insert code resp resps
          Nothing -> insert code resp resps
        404 -> case resp ^? _Inline . S.description of
          (Just desc) ->
            if " not found"
              `T.isSuffixOf` desc
              && resp
                ^? _Inline
                  . links
                == pure mempty
              && resp
                ^? _Inline
                  . content
                == pure mempty
              && resp
                ^? _Inline
                  . headers
                == pure mempty
              then resps
              else insert code resp resps
          Nothing -> insert code resp resps
        _ -> insert code resp resps

type Responses' = InsOrdHashMap HttpStatusCode (Referenced Response)
