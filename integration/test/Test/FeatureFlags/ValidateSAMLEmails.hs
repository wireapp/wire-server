module Test.FeatureFlags.ValidateSAMLEmails where

import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testPatchValidateSAMLEmails :: (HasCallStack) => App ()
testPatchValidateSAMLEmails =
  checkPatch OwnDomain "validateSAMLemails"
    $ object ["status" .= "disabled"]

testValidateSAMLEmailsInternal :: (HasCallStack) => App ()
testValidateSAMLEmailsInternal = do
  (alice, tid, _) <- createTeam OwnDomain 0
  withWebSocket alice $ \ws -> do
    setFlag InternalAPI ws tid "validateSAMLemails" disabled
    setFlag InternalAPI ws tid "validateSAMLemails" enabled
