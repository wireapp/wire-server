module Test.Wire.API.Golden.Manual.GetPaginatedConversationIds where

import Data.Proxy
import Data.Range
import Imports
import Wire.API.Conversation
import Wire.API.Routes.MultiTablePaging

testObject_GetPaginatedConversationIds_1 :: GetPaginatedConversationIds
testObject_GetPaginatedConversationIds_1 = GetPaginatedConversationIds Nothing (toRange (Proxy @50))

testObject_GetPaginatedConversationIds_2 :: GetPaginatedConversationIds
testObject_GetPaginatedConversationIds_2 = GetPaginatedConversationIds (Just $ ConversationPagingState PagingLocals Nothing) (toRange (Proxy @1000))
