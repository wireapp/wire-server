{-# LANGUAGE TemplateHaskell #-}

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

module Galley.Effects.CodeStore
  ( -- * Code store effect
    CodeStore (..),

    -- * Create code
    createCode,

    -- * Read code
    getCode,

    -- * Delete code
    deleteCode,

    -- * Code generation
    makeKey,
    generateCode,

    -- * Configuration
    getConversationCodeURI,
  )
where

import Data.Code
import Data.Id
import Data.Misc
import Galley.Data.Types
import Imports
import Polysemy
import Wire.API.Password

data CodeStore m a where
  CreateCode :: Code -> Maybe Password -> CodeStore m ()
  GetCode :: Key -> Scope -> CodeStore m (Maybe (Code, Maybe Password))
  DeleteCode :: Key -> Scope -> CodeStore m ()
  MakeKey :: ConvId -> CodeStore m Key
  GenerateCode :: ConvId -> Scope -> Timeout -> CodeStore m Code
  GetConversationCodeURI :: Maybe Text -> CodeStore m (Maybe HttpsUrl)

makeSem ''CodeStore
