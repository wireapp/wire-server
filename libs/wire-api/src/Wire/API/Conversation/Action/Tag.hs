{-# LANGUAGE StandaloneKindSignatures #-}
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
-- Ignore unused `genSingletons` Template Haskell results
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Wire.API.Conversation.Action.Tag where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.OpenApi qualified as S
import Data.Schema hiding (tag)
import Data.Singletons.TH
import Imports
import Test.QuickCheck (elements)
import Wire.Arbitrary (Arbitrary (..))

data ConversationActionTag
  = ConversationJoinTag
  | ConversationLeaveTag
  | ConversationRemoveMembersTag
  | ConversationMemberUpdateTag
  | ConversationDeleteTag
  | ConversationRenameTag
  | ConversationMessageTimerUpdateTag
  | ConversationReceiptModeUpdateTag
  | ConversationAccessDataTag
  | ConversationUpdateProtocolTag
  | ConversationUpdateAddPermissionTag
  | ConversationResetTag
  deriving (Show, Eq, Generic, Bounded, Enum)

instance Arbitrary ConversationActionTag where
  arbitrary = elements [minBound .. maxBound]

instance ToSchema ConversationActionTag where
  schema =
    enum @Text "ConversationActionTag" $
      mconcat
        [ element "ConversationJoinTag" ConversationJoinTag,
          element "ConversationLeaveTag" ConversationLeaveTag,
          element "ConversationRemoveMembersTag" ConversationRemoveMembersTag,
          element "ConversationMemberUpdateTag" ConversationMemberUpdateTag,
          element "ConversationDeleteTag" ConversationDeleteTag,
          element "ConversationRenameTag" ConversationRenameTag,
          element "ConversationMessageTimerUpdateTag" ConversationMessageTimerUpdateTag,
          element "ConversationReceiptModeUpdateTag" ConversationReceiptModeUpdateTag,
          element "ConversationAccessDataTag" ConversationAccessDataTag,
          element "ConversationUpdateProtocolTag" ConversationUpdateProtocolTag,
          element "ConversationUpdateAddPermissionTag" ConversationUpdateAddPermissionTag
        ]

instance ToJSON ConversationActionTag where
  toJSON = schemaToJSON

instance FromJSON ConversationActionTag where
  parseJSON = schemaParseJSON

instance S.ToSchema ConversationActionTag where
  declareNamedSchema = schemaToSwagger

$(genSingletons [''ConversationActionTag])

$(singDecideInstance ''ConversationActionTag)
