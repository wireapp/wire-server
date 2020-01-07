-- | A set of acceptance tests that you can use to test that your server is
module Test.AcceptanceSpec where

import           Test.Hspec (Spec, describe)
import           Web.Scim.Server (app)
import           Web.Scim.Server.Mock

import           Web.Scim.Capabilities.MetaSchema (empty)
import           Web.Scim.Test.Acceptance (microsoftAzure)


spec :: Spec
spec = do
  let 
    app' = do
      storage <- emptyTestStorage
      pure (app @Mock empty (nt storage))

  describe "Azure" $ microsoftAzure  app'
