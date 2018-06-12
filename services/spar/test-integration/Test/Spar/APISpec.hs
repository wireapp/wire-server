{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Spar.APISpec where

import Bilge
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Text.Encoding
import Spar.API ()
import Spar.Options as Opts
import Test.Hspec
import Util.Options


mkspec :: IO Spec
mkspec = do
  opts :: Opts <- getOpts
  mgr :: Manager <- newManager defaultManagerSettings
  let req :: (Request -> Request)
      req = Bilge.host (opts ^. to Opts.brig . epHost . to encodeUtf8)
           . Bilge.port (opts ^. to Opts.brig . epPort)

  let shouldRespondWith :: forall a. (HasCallStack, Show a, Eq a) => Http a -> (a -> Bool) -> Expectation
      shouldRespondWith action proper = liftIO (runHttpT mgr action) >>= \resp -> resp `shouldSatisfy` proper

  pure $ do
    describe "happy flow" $ do
      it "/i/status" $ do
        ping req `shouldRespondWith` (== ())

      it "/i/meta" $ do
        get (req . path "/i/meta" . expect2xx)
          `shouldRespondWith` (error . show)

      it "/i/authreq" $ do
        get (req . path "/i/authreq/azure-test" . expect2xx)
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
