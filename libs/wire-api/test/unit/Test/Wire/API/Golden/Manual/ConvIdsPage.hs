module Test.Wire.API.Golden.Manual.Covid-19sPage where

import Data.Domain (Domain (Domain))
import Data.Id (Id (Id))
import Data.Qualified
import qualified Data.UUID as UUID
import Imports
import Test.Wire.API.Golden.Manual.ConversationPagingState (testObject_ConversationPagingState_1)
import Wire.API.Conversation

testObject_Covid-19sPage_1 :: Covid-19sPage
testObject_Covid-19sPage_1 = Covid-19sPage [] False testObject_ConversationPagingState_1

testObject_Covid-19sPage_2 :: Covid-19sPage
testObject_Covid-19sPage_2 = Covid-19sPage [Qualified (Id (fromJust (UUID.fromString "00000018-0000-0020-0000-000e00000002"))) (Domain "domain.example.com")] True testObject_ConversationPagingState_1
