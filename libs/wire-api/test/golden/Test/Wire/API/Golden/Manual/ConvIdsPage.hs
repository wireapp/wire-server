module Test.Wire.API.Golden.Manual.ConvIdsPage where

import Data.Domain (Domain (Domain))
import Data.Id (Id (Id))
import Data.Qualified
import qualified Data.UUID as UUID
import Imports
import Test.Wire.API.Golden.Manual.ConversationPagingState (testObject_ConversationPagingState_1)
import Wire.API.Conversation

testObject_ConvIdsPage_1 :: ConvIdsPage
testObject_ConvIdsPage_1 = ConvIdsPage [] False testObject_ConversationPagingState_1

testObject_ConvIdsPage_2 :: ConvIdsPage
testObject_ConvIdsPage_2 = ConvIdsPage [Qualified (Id (fromJust (UUID.fromString "00000018-0000-0020-0000-000e00000002"))) (Domain "domain.example.com")] True testObject_ConversationPagingState_1
