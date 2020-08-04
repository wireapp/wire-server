{-# LANGUAGE DerivingVia #-}

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

module Wire.API.Federation.API.Conversation where

import Data.Aeson (FromJSON, ToJSON)
import Data.Id (ConvId, UserId)
import Data.Qualified (Qualified)
import Imports
import Servant.API (Capture, JSON, Post, ReqBody, (:>))
import Servant.API.Generic ((:-))
import Test.QuickCheck (Arbitrary (arbitrary))
import qualified Test.QuickCheck as QC
import Wire.API.Federation.Event (Event, MemberJoin)
import Wire.API.Federation.Util.Aeson (CustomEncoded (CustomEncoded))

data Api routes = Api
  { joinConversationById ::
      routes
        :- "f"
        :> "conversation"
        :> Capture "cnv" (Qualified ConvId)
        :> "join"
        :> ReqBody '[JSON] JoinConversationByIdRequest
        :> Post '[JSON] (ConversationUpdateResult MemberJoin)
  }
  deriving stock (Generic)

data JoinConversationByIdRequest = JoinConversationByIdRequest
  { joinUserId :: Qualified UserId
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded JoinConversationByIdRequest)

data ConversationUpdateResult a
  = ConversationUpdated (Event a)
  | ConversationUnchanged
  deriving stock (Eq, Show, Generic, Foldable, Functor, Traversable)
  deriving (ToJSON, FromJSON) via (CustomEncoded (ConversationUpdateResult a))

-- Arbitrary

instance Arbitrary JoinConversationByIdRequest where
  arbitrary = JoinConversationByIdRequest <$> arbitrary

instance Arbitrary a => Arbitrary (ConversationUpdateResult a) where
  arbitrary = QC.oneof [pure ConversationUnchanged, ConversationUpdated <$> arbitrary]
