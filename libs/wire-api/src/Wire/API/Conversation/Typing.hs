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

module Wire.API.Conversation.Typing
  ( TypingStatus (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.OpenApi qualified as S
import Data.Schema
import Imports
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

data TypingStatus
  = StartedTyping
  | StoppedTyping
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform TypingStatus)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema TypingStatus

instance ToSchema TypingStatus where
  schema =
    object "TypingData" $
      field "status" typingStatusSchema

typingStatusSchema :: ValueSchema NamedSwaggerDoc TypingStatus
typingStatusSchema =
  enum @Text "TypingStatus" $
    element "started" StartedTyping
      <> element "stopped" StoppedTyping
