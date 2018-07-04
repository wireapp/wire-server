{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Spar.DataSpec where

import Cassandra as Cas
import Control.Concurrent
import Control.Monad.Reader
import Data.Time
import Spar.Options as Options
import Lens.Micro
import Spar.Data as Data
import Util

import qualified SAML2.WebSSO as SAML


spec :: SpecWith TestEnv
spec = do
  describe "TTL" $ do
    it "works in seconds" $ do
      env <- ask
      (_, _, idpid) <- createTestIdP
      (_, req) <- call $ callAuthnReq (env ^. teSpar) idpid

      let probe :: IO Bool
          probe = do
            denv :: Data.Env <- Data.mkEnv (env ^. teOpts) <$> getCurrentTime
            runClient (env ^. teCql) (checkAgainstRequest (req ^. SAML.rqID) `runReaderT` denv)

          maxttl :: Int  -- musec
          maxttl = (fromIntegral . fromTTL $ env ^. teOpts . to maxttlAuthreq) * 1000 * 1000

      liftIO $ do
        maxttl `shouldSatisfy` (< 60 * 1000 * 1000)  -- otherwise the test will be really slow.
        probe `shouldReturn` True
        threadDelay ((maxttl `div` 10) * 8)
        probe `shouldReturn` True
        threadDelay  ((maxttl `div` 10) * 4)
        probe `shouldReturn` False


  -- TODO: do we need any of the ones below at all?

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
