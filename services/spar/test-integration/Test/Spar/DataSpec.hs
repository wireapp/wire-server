module Test.Spar.DataSpec where

import Spar.Data ()
import Test.Hspec


spec :: Spec
spec = do
  describe "cql binding" $ do
    describe "storeRequest" $ do
      it "stores req ID with end-of-life to disk" $ do
        pending

      it "sets TTL" $ do
        pending

    describe "checkAgainstRequest" $ do
      context "request exists and is alive" $ do
        it "returns False" $ do
          pending

      context "request exists, but is outdated" $ do
        it "returns False" $ do
          pending

      context "request does not exist" $ do
        it "returns True" $ do
          pending

    describe "storeAssertion" $ do
      context "assertion exists and is alive" $ do
        it "returns False" $ do
          pending

        it "does not write anything" $ do
          pending

      context "assertion exists, but is outdated" $ do
        it "returns False" $ do
          pending

        it "does not write anything" $ do
          pending

      context "assertion does not exist" $ do
        it "returns True" $ do
          pending

        it "stores the new assertion" $ do
          pending

        it "sets new assertion's TTL" $ do
          pending
