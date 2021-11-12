-- |
module Test.Wire.API.Golden.Manual.ConversationCoverView where

import Data.Id (Id (Id))
import qualified Data.UUID as UUID
import Imports
import Wire.API.Conversation (ConversationCoverView (..))

testObject_ConversationCoverView_1 :: ConversationCoverView
testObject_ConversationCoverView_1 =
  ConversationCoverView
    (Id (fromJust (UUID.fromString "00000018-0000-0020-0000-000e00000002")))
    Nothing

testObject_ConversationCoverView_2 :: ConversationCoverView
testObject_ConversationCoverView_2 =
  ConversationCoverView
    (Id (fromJust (UUID.fromString "00000018-0000-0020-0000-000e00000002")))
    (Just "conversation name")

testObject_ConversationCoverView_3 :: ConversationCoverView
testObject_ConversationCoverView_3 =
  ConversationCoverView
    (Id (fromJust (UUID.fromString "00000018-0000-0020-0000-000e00000002")))
    (Just "")
