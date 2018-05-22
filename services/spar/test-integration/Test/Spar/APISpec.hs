module Test.Spar.APISpec where

import Spar.API ()
import Test.Hspec


spec :: Spec
spec = do
  describe "the Spar monad" $ do
    describe "createUser" $ do
      it "creates the user on brig." $ do
        pending

    describe "forwardBrigLogin" $ do
      it "retrieves a session cookie from brig and sends it to the client in a redirect (302) response" $ do
        pending

  describe "happy flow" $ do
    pure ()

  describe "access denied" $ do
    pure ()
