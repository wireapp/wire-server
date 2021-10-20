module Test.Wire.API.Golden.Manual.ConversationPagingState where

import qualified Data.ByteString as BS
import Imports
import Wire.API.Conversation
import Wire.API.Routes.MultiTablePaging

testObject_ConversationPagingState_1 :: ConversationPagingState
testObject_ConversationPagingState_1 = ConversationPagingState PagingLocals Nothing

testObject_ConversationPagingState_2 :: ConversationPagingState
testObject_ConversationPagingState_2 = ConversationPagingState PagingLocals (Just (BS.pack [0, 1, 88, 99, 125, 88]))

testObject_ConversationPagingState_3 :: ConversationPagingState
testObject_ConversationPagingState_3 = ConversationPagingState PagingRemotes Nothing

testObject_ConversationPagingState_4 :: ConversationPagingState
testObject_ConversationPagingState_4 = ConversationPagingState PagingRemotes (Just (BS.pack [88, 12, 23]))
