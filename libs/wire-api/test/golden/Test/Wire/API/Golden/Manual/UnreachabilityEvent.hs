module Test.Wire.API.Golden.Manual.UnreachabilityEvent where

import Data.Domain
import Data.Id
import Data.Qualified
import qualified Data.UUID as UUID (fromString)
import Imports
import Test.Wire.API.Golden.Generated.Event_user
import Wire.API.Unreachable

testObject_UnreachabilityEvent_1 :: UnreachabilityEvent
testObject_UnreachabilityEvent_1 =
  UnreachabilityEvent
    { event = testObject_Event_user_12,
      failedToProcess = mempty
    }

testObject_UnreachabilityEvent_2 :: UnreachabilityEvent
testObject_UnreachabilityEvent_2 =
  UnreachabilityEvent
    { event = testObject_Event_user_12,
      failedToProcess =
        mempty
          { add =
              unreachableFromList
                [ Qualified (Id (fromJust (UUID.fromString "0000114a-0000-7da8-0000-40cb00007fcf"))) (Domain "faraway.example.com")
                ]
          }
    }
