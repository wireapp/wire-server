{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Wire.API.Conversation.Typing
  ( -- * TypingData
    TypingData (..),
    TypingStatus (..),

    -- * Swagger
    modelTyping,
    typeTypingStatus,
  )
where

import Data.Aeson
import qualified Data.Swagger.Build.Api as Doc
import Imports

newtype TypingData = TypingData
  { tdStatus :: TypingStatus
  }
  deriving (Eq, Show, Generic)

modelTyping :: Doc.Model
modelTyping = Doc.defineModel "Typing" $ do
  Doc.description "Data to describe typing info"
  Doc.property "status" typeTypingStatus $ Doc.description "typing status"

instance ToJSON TypingStatus where
  toJSON StartedTyping = String "started"
  toJSON StoppedTyping = String "stopped"

instance FromJSON TypingStatus where
  parseJSON (String "started") = return StartedTyping
  parseJSON (String "stopped") = return StoppedTyping
  parseJSON x = fail $ "No status-type: " <> show x

data TypingStatus
  = StartedTyping
  | StoppedTyping
  deriving (Eq, Ord, Show, Generic)

typeTypingStatus :: Doc.DataType
typeTypingStatus =
  Doc.string $
    Doc.enum
      [ "started",
        "stopped"
      ]

instance ToJSON TypingData where
  toJSON t = object ["status" .= tdStatus t]

instance FromJSON TypingData where
  parseJSON = withObject "typing-data" $ \o ->
    TypingData <$> o .: "status"
