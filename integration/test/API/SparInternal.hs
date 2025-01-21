module API.SparInternal where

import Testlib.Prelude

getAllIdPs :: (HasCallStack, MakesValue domain) => domain -> String -> App Response
getAllIdPs domain tid = do
  req <- baseRequest domain Spar Unversioned $ joinHttpPath ["i", "identity-providers", tid]
  submit "GET" req
