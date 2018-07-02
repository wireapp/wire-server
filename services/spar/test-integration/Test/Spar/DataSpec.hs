{-# LANGUAGE ScopedTypeVariables #-}

module Test.Spar.DataSpec where

import Spar.Data ()
import Test.Hspec
import Util


spec :: SpecWith TestEnv
spec = do
  describe "TTL" $ do
    it "works in seconds" $ \_ -> do
      pending


  -- TODO: do we need any of the ones below at all?

  describe "cql binding" $ do
    describe "storeRequest" $ do
      it "stores req ID with end-of-life to disk" $ \_ -> do
        pending

      it "sets TTL" $ \_ -> do
        pending

    describe "checkAgainstRequest" $ do
      context "request exists and is alive" $ do
        it "returns False" $ \_ -> do
          pending

      context "request exists, but is outdated" $ do
        it "returns False" $ \_ -> do
          pending

      context "request does not exist" $ do
        it "returns True" $ \_ -> do
          pending

    describe "storeAssertion" $ do
      context "assertion exists and is alive" $ do
        it "returns False" $ \_ -> do
          pending

        it "does not write anything" $ \_ -> do
          pending

      context "assertion exists, but is outdated" $ do
        it "returns False" $ \_ -> do
          pending

        it "does not write anything" $ \_ -> do
          pending

      context "assertion does not exist" $ do
        it "returns True" $ \_ -> do
          pending

        it "stores the new assertion" $ \_ -> do
          pending

        it "sets new assertion's TTL" $ \_ -> do
          pending
