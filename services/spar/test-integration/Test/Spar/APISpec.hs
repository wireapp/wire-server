{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Spar.APISpec where

import Bilge
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.String.Conversions
import Spar.API ()
import Spar.Options as Opts
import Test.Hspec
import Util.Options
import SAML2.WebSSO as SAML


mkspec :: IO Spec
mkspec = do
  opts :: Opts <- getOpts
  mgr :: Manager <- newManager defaultManagerSettings
  let brigreq :: (Request -> Request)
      brigreq = Bilge.host (opts ^. to Opts.brig . epHost . to cs)
              . Bilge.port (opts ^. to Opts.brig . epPort)
      sparreq :: (Request -> Request)
      sparreq = Bilge.host (opts ^. to Opts.saml . SAML.cfgSPHost . to cs)
              . Bilge.port (opts ^. to Opts.saml . SAML.cfgSPPort . to fromIntegral)

  let shouldRespondWith :: forall a. (HasCallStack, Show a, Eq a) => Http a -> (a -> Bool) -> Expectation
      shouldRespondWith action proper = liftIO (runHttpT mgr action) >>= \resp -> resp `shouldSatisfy` proper

  pure $ do
    describe "happy flow" $ do
      it "brig /i/status" $ do
        ping brigreq `shouldRespondWith` (== ())

      it "spar /i/status" $ do
        ping sparreq `shouldRespondWith` (== ())

      it "/i/meta" $ do
        get (sparreq . path "/i/meta" . expect2xx)
          `shouldRespondWith` (error . show)

      it "/i/authreq" $ do
        get (sparreq . path "/i/authreq/azure-test" . expect2xx)
          `shouldRespondWith` (error . show)

      it "/i/authresp" $ do
        pending
        -- (just fake the response from the IdP?)

    describe "access denied" $ do
      it "/i/authresp" $ do
        pending

    it "rejects responses not matching any request" $ do
      pending

    it "rejects replayed assertions" $ do
      pending


ping :: (Request -> Request) -> Http ()
ping req = void . get $ req . path "/i/status" . expect2xx
