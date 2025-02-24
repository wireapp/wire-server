module Test.FeatureFlags.ValidateSAMLEmails where

import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testPatchValidateSAMLEmails :: (HasCallStack) => FeatureTable -> App ()
testPatchValidateSAMLEmails table =
  checkPatchWithTable table OwnDomain "validateSAMLemails"
    $ object ["status" .= "disabled"]

testValidateSAMLEmailsInternal :: (HasCallStack) => FeatureTable -> App ()
testValidateSAMLEmailsInternal table = do
  (alice, tid, _) <- createTeam OwnDomain 0
  updateMigrationState OwnDomain tid table
  withWebSocket alice $ \ws -> do
    setFlag InternalAPI ws tid "validateSAMLemails" disabled
    setFlag InternalAPI ws tid "validateSAMLemails" enabled
