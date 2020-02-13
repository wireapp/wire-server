module Test.Spar.Intra.BrigSpec (spec) where

import Imports hiding (head)
import Util

spec :: SpecWith TestEnv
spec = do
  describe "user deletion between brig and spar" $ do
    it "if a user gets deleted on brig, it will be deleted on spar as well." $ do
      pending
    it "if a user gets deleted on spar, it will be deleted on spar as well." $ do
      pendingWith "or deactivated?  we should decide what we want here."
