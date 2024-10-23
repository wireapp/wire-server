module Performance.BigConversation where

import SetupHelpers
import Testlib.Prelude

testBigMLSConversation :: App ()
testBigMLSConversation = do
  void $ createTeam OwnDomain 50
